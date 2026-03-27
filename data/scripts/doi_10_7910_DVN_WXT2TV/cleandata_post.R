# Load data
library(readr)
library(lubridate)
dat <- read_csv("Data/Raw/postsurvey.csv")
dat <- dat[3:nrow(dat),]

# Open pre rids

rids <-  read_csv("Data/Raw/ridmatch.csv")
colnames(rids) <- c("rid_pre","rid_post")

dat$rid_post <- dat$rid
dat$rid <- NULL
dat <- merge(dat,rids,by="rid_post")

# Only include finished responses
dat <- dat[dat$Finished=="True",]

# Attention checks
dat$attention1 <- ifelse(dat$Q1=="I understand,I don't understand",1,0)
dat$attention2 <- ifelse(dat$Q97=="Extremely interested,Very interested",1,0)
day <- day(dat$StartDate)
min <- day-1
max <- day+1 
dat$attention3 <- ifelse(as.numeric(dat$Q77)>=min & as.numeric(dat$Q77)<=max,1,0)


# Note that variables ending in 2 are used to assess placement knowledge and to create scales. Self-placements should
# be coded so 1 indicates the most pro-Trump option

# Variables without a 2 have the original categories

# Code placement questions
# If missing (means did not answer question) code as 0 
# The government should make every effort to improve the social and economic position of Blacks
dat$improveblacks_you <- as.factor(dat$Q28_1)
dat$improveblacks_you2 <- dat$improveblacks_you 
levels(dat$improveblacks_you2) <- c(3,1,2)
dat$improveblacks_you2 <- as.numeric(as.character(dat$improveblacks_you2))

dat$improveblacks_dem <- as.factor(dat$Q28_2)
dat$improveblacks_dem2 <- dat$improveblacks_dem
levels(dat$improveblacks_dem2) <- c(3,1,2)
dat$improveblacks_dem2 <- as.numeric(as.character(dat$improveblacks_dem2))

dat$improveblacks_biden <- as.factor(dat$Q28_3)
dat$improveblacks_biden2 <- dat$improveblacks_biden
levels(dat$improveblacks_biden2) <- c(3,1,2)
dat$improveblacks_biden2 <- as.numeric(as.character(dat$improveblacks_biden2))

dat$improveblacks_rep <- as.factor(dat$Q28_4)
dat$improveblacks_rep2 <- dat$improveblacks_rep
levels(dat$improveblacks_rep2) <- c(3,1,2)
dat$improveblacks_rep2 <- as.numeric(as.character(dat$improveblacks_rep2))

dat$improveblacks_trump <- as.factor(dat$Q28_5)
dat$improveblacks_trump2 <- dat$improveblacks_trump
levels(dat$improveblacks_trump2) <- c(3,1,2)
dat$improveblacks_trump2 <- as.numeric(as.character(dat$improveblacks_trump2))

dat$improveblacks_whites <- as.factor(dat$Q28_6)
dat$improveblacks_whites2 <- dat$improveblacks_whites
levels(dat$improveblacks_whites2) <- c(3,1,2)
dat$improveblacks_whites2 <- as.numeric(as.character(dat$improveblacks_whites2))

dat$improveblacks_blacks <- as.factor(dat$Q28_7)
dat$improveblacks_blacks2 <- dat$improveblacks_blacks
levels(dat$improveblacks_blacks2) <- c(3,1,2)
dat$improveblacks_blacks2 <- as.numeric(as.character(dat$improveblacks_blacks2))

dat$improveblacks_cand <- ifelse(dat$improveblacks_biden2>dat$improveblacks_trump2,1,0)
dat$improveblacks_party <- ifelse(dat$improveblacks_dem2>dat$improveblacks_rep2,1,0)
dat$improveblacks_groups <- ifelse(dat$improveblacks_blacks2>dat$improveblacks_whites2,1,0)
dat$improveblacks_cand[is.na(dat$improveblacks_cand)] <- 0 
dat$improveblacks_party[is.na(dat$improveblacks_party)] <- 0 
dat$improveblacks_groups[is.na(dat$improveblacks_groups)] <- 0 

dat$improveblacks_cand_biden <- ifelse(dat$improveblacks_biden=="Agree",1,0)
dat$improveblacks_cand_trump <- ifelse(dat$improveblacks_trump=="Disagree",1,0)
dat$improveblacks_cand_biden[is.na(dat$improveblacks_cand_biden)] <- 0 
dat$improveblacks_cand_trump[is.na(dat$improveblacks_cand_trump)] <- 0 

# Rescale so that 1 indicates the most liberal option
dat$improveblacks_you2 <- (dat$improveblacks_you2-1)/2 

# A constitutional amendment banning same sex marriages
dat$ssmban_you <- as.factor(dat$Q29_1)
dat$ssmban_you2 <- dat$ssmban_you
levels(dat$ssmban_you2) <- c(3,1,2)
dat$ssmban_you2 <- as.numeric(as.character(dat$ssmban_you2))

dat$ssmban_dem <- as.factor(dat$Q29_2)
dat$ssmban_dem2 <- dat$ssmban_dem
levels(dat$ssmban_dem2) <- c(3,1,2)
dat$ssmban_dem2 <- as.numeric(as.character(dat$ssmban_dem2))

dat$ssmban_biden <- as.factor(dat$Q29_3)
dat$ssmban_biden2 <- dat$ssmban_biden
levels(dat$ssmban_biden2) <- c(3,1,2)
dat$ssmban_biden2 <- as.numeric(as.character(dat$ssmban_biden2))

dat$ssmban_rep <- as.factor(dat$Q29_4)
dat$ssmban_rep2 <- dat$ssmban_rep
levels(dat$ssmban_rep2) <- c(3,1,2)
dat$ssmban_rep2 <- as.numeric(as.character(dat$ssmban_rep2))

dat$ssmban_trump <- as.factor(dat$Q29_5)
dat$ssmban_trump2 <- dat$ssmban_trump
levels(dat$ssmban_trump2) <- c(3,1,2)
dat$ssmban_trump2 <- as.numeric(as.character(dat$ssmban_trump2))

dat$ssmban_cand <- ifelse(dat$ssmban_biden2<dat$ssmban_trump2,1,0)
dat$ssmban_party <- ifelse(dat$ssmban_dem2<dat$ssmban_rep2,1,0)
dat$ssmban_cand[is.na(dat$ssmban_cand)] <- 0 
dat$ssmban_party[is.na(dat$ssmban_party)] <- 0 

dat$ssmban_cand_biden <- ifelse(dat$ssmban_biden=="Disagree",1,0)
dat$ssmban_cand_trump <- ifelse(dat$ssmban_trump=="Agree",1,0)
dat$ssmban_cand_biden[is.na(dat$ssmban_cand_biden)] <- 0 
dat$ssmban_cand_trump[is.na(dat$ssmban_cand_trump)] <- 0 

dat$ssmban_you2 <- (3-dat$ssmban_you2)/2 

#Lowering federal taxes
dat$lowertax_you <- as.factor(dat$Q30_1)
dat$lowertax_you2 <- dat$lowertax_you
levels(dat$lowertax_you2) <- c(3,1,2)
dat$lowertax_you2 <- as.numeric(as.character(dat$lowertax_you2))

dat$lowertax_dem <- as.factor(dat$Q30_2)
dat$lowertax_dem2 <- dat$lowertax_dem
levels(dat$lowertax_dem2) <- c(3,1,2)
dat$lowertax_dem2 <- as.numeric(as.character(dat$lowertax_dem2))

dat$lowertax_biden <- as.factor(dat$Q30_3)
dat$lowertax_biden2 <- dat$lowertax_biden
levels(dat$lowertax_biden2) <- c(3,1,2)
dat$lowertax_biden2 <- as.numeric(as.character(dat$lowertax_biden2))

dat$lowertax_rep <- as.factor(dat$Q30_4)
dat$lowertax_rep2 <- dat$lowertax_rep
levels(dat$lowertax_rep2) <- c(3,1,2)
dat$lowertax_rep2 <- as.numeric(as.character(dat$lowertax_rep2))

dat$lowertax_trump <- as.factor(dat$Q30_5)
dat$lowertax_trump2 <- dat$lowertax_trump
levels(dat$lowertax_trump2) <- c(3,1,2)
dat$lowertax_trump2 <- as.numeric(as.character(dat$lowertax_trump2))

dat$lowertax_cand <- ifelse(dat$lowertax_biden2<dat$lowertax_trump2,1,0)
dat$lowertax_party <- ifelse(dat$lowertax_dem2<dat$lowertax_rep2,1,0)
dat$lowertax_cand[is.na(dat$lowertax_cand)] <- 0 
dat$lowertax_party[is.na(dat$lowertax_party)] <- 0 

dat$lowertax_cand_biden <- ifelse(dat$lowertax_biden=="Disagree",1,0)
dat$lowertax_cand_trump <- ifelse(dat$lowertax_trump=="Agree",1,0)
dat$lowertax_cand_biden[is.na(dat$lowertax_cand_biden)] <- 0 
dat$lowertax_cand_trump[is.na(dat$lowertax_cand_trump)] <- 0 

dat$lowertax_you2 <- (3-dat$lowertax_you2)/2 

# Restrictions on whether a woman can get an abortion
dat$abortion_you <- as.factor(dat$Q31_1)
dat$abortion_you2 <- dat$abortion_you
levels(dat$abortion_you2) <- c(3,1,2)
dat$abortion_you2 <- as.numeric(as.character(dat$abortion_you2))

dat$abortion_dem <- as.factor(dat$Q31_2)
dat$abortion_dem2 <- dat$abortion_dem
levels(dat$abortion_dem2) <- c(3,1,2)
dat$abortion_dem2 <- as.numeric(as.character(dat$abortion_dem2))

dat$abortion_biden <- as.factor(dat$Q31_3)
dat$abortion_biden2 <- dat$abortion_biden
levels(dat$abortion_biden2) <- c(3,1,2)
dat$abortion_biden2 <- as.numeric(as.character(dat$abortion_biden2))

dat$abortion_rep <- as.factor(dat$Q31_4)
dat$abortion_rep2 <- dat$abortion_rep
levels(dat$abortion_rep2) <- c(3,1,2)
dat$abortion_rep2 <- as.numeric(as.character(dat$abortion_rep2))

dat$abortion_trump <- as.factor(dat$Q31_5)
dat$abortion_trump2 <- dat$abortion_trump
levels(dat$abortion_trump2) <- c(3,1,2)
dat$abortion_trump2 <- as.numeric(as.character(dat$abortion_trump2))

dat$abortion_cand_biden <- ifelse(dat$abortion_biden=="Disagree",1,0)
dat$abortion_cand_trump <- ifelse(dat$abortion_trump=="Agree",1,0)
dat$abortion_cand_biden[is.na(dat$abortion_cand_biden)] <- 0 
dat$abortion_cand_trump[is.na(dat$abortion_cand_trump)] <- 0 

dat$abortion_cand <- ifelse(dat$abortion_biden2<dat$abortion_trump2,1,0)
dat$abortion_party <- ifelse(dat$abortion_dem2<dat$abortion_rep2,1,0)
dat$abortion_cand[is.na(dat$abortion_cand)] <- 0 
dat$abortion_party[is.na(dat$abortion_party)] <- 0 

dat$abortion_you2 <- (3-dat$abortion_you2)/2 

#Strengthening background checks to prevent guns from being sold to people with criminal records or mental illness
dat$backchecks_you <- as.factor(dat$Q32_1)
dat$backchecks_you2 <- dat$backchecks_you
levels(dat$backchecks_you2) <- c(3,1,2)
dat$backchecks_you2 <- as.numeric(as.character(dat$backchecks_you2))

dat$backchecks_dem <- as.factor(dat$Q32_2)
dat$backchecks_dem2 <- dat$backchecks_dem
levels(dat$backchecks_dem2) <- c(3,1,2)
dat$backchecks_dem2 <- as.numeric(as.character(dat$backchecks_dem2))

dat$backchecks_biden <- as.factor(dat$Q32_3)
dat$backchecks_biden2 <- dat$backchecks_biden
levels(dat$backchecks_biden2) <- c(3,1,2)
dat$backchecks_biden2 <- as.numeric(as.character(dat$backchecks_biden2))

dat$backchecks_rep <- as.factor(dat$Q32_4)
dat$backchecks_rep2 <- dat$backchecks_rep
levels(dat$backchecks_rep2) <- c(3,1,2)
dat$backchecks_rep2 <- as.numeric(as.character(dat$backchecks_rep2))

dat$backchecks_trump <- as.factor(dat$Q32_5)
dat$backchecks_trump2 <- dat$backchecks_trump
levels(dat$backchecks_trump2) <- c(3,1,2)
dat$backchecks_trump2 <- as.numeric(as.character(dat$backchecks_trump2))

dat$backchecks_whites <- as.factor(dat$Q32_6)
dat$backchecks_whites2 <- dat$backchecks_whites
levels(dat$backchecks_whites2) <- c(3,1,2)
dat$backchecks_whites2 <- as.numeric(as.character(dat$backchecks_whites2))

dat$backchecks_blacks <- as.factor(dat$Q32_7)
dat$backchecks_blacks2 <- dat$backchecks_blacks
levels(dat$backchecks_blacks2) <- c(3,1,2)
dat$backchecks_blacks2 <- as.numeric(as.character(dat$backchecks_blacks2))

dat$backchecks_cand <- ifelse(dat$backchecks_biden2>dat$backchecks_trump2,1,0)
dat$backchecks_party <- ifelse(dat$backchecks_dem2>dat$backchecks_rep2,1,0)
dat$backchecks_groups <- ifelse(dat$backchecks_blacks2>dat$backchecks_whites2,1,0)
dat$backchecks_cand[is.na(dat$backchecks_cand)] <- 0 
dat$backchecks_party[is.na(dat$backchecks_party)] <- 0 
dat$backchecks_groups[is.na(dat$backchecks_groups)] <- 0 

dat$backchecks_cand_biden <- ifelse(dat$backchecks_biden=="Agree",1,0)
dat$backchecks_cand_trump <- ifelse(dat$backchecks_trump=="Disagree",1,0)
dat$backchecks_cand_biden[is.na(dat$backchecks_cand_biden)] <- 0 
dat$backchecks_cand_trump[is.na(dat$backchecks_cand_trump)] <- 0 

dat$backchecks_you2 <- (dat$backchecks_you2-1)/2 

#Restricting the amount of carbon dioxide (CO2) factories can emit
dat$co2_you <- as.factor(dat$Q33_1)
dat$co2_you2 <- dat$co2_you
levels(dat$co2_you2) <- c(3,1,2)
dat$co2_you2 <- as.numeric(as.character(dat$co2_you2))

dat$co2_dem <- as.factor(dat$Q33_2)
dat$co2_dem2 <- dat$co2_dem
levels(dat$co2_dem2) <- c(3,1,2)
dat$co2_dem2 <- as.numeric(as.character(dat$co2_dem2))

dat$co2_biden <- as.factor(dat$Q33_3)
dat$co2_biden2 <- dat$co2_biden
levels(dat$co2_biden2) <- c(3,1,2)
dat$co2_biden2 <- as.numeric(as.character(dat$co2_biden2))

dat$co2_rep <- as.factor(dat$Q33_4)
dat$co2_rep2 <- dat$co2_rep
levels(dat$co2_rep2) <- c(3,1,2)
dat$co2_rep2 <- as.numeric(as.character(dat$co2_rep2))

dat$co2_trump <- as.factor(dat$Q33_5)
dat$co2_trump2 <- dat$co2_trump
levels(dat$co2_trump2) <- c(3,1,2)
dat$co2_trump2 <- as.numeric(as.character(dat$co2_trump2))

dat$co2_cand <- ifelse(dat$co2_biden2>dat$co2_trump2,1,0)
dat$co2_party <- ifelse(dat$co2_dem2>dat$co2_rep2,1,0)
dat$co2_cand[is.na(dat$co2_cand)] <- 0 
dat$co2_party[is.na(dat$co2_party)] <- 0 

dat$co2_cand_biden <- ifelse(dat$co2_biden=="Agree",1,0)
dat$co2_cand_trump <- ifelse(dat$co2_trump=="Disagree",1,0)
dat$co2_cand_biden[is.na(dat$co2_cand_biden)] <- 0 
dat$co2_cand_trump[is.na(dat$co2_cand_trump)] <- 0 

dat$co2_you2 <- (dat$co2_you2-1)/2 

#Preventing any Muslim who is not a US citizen from entering the country
dat$muslim_you <- as.factor(dat$Q34_1)
dat$muslim_you2 <- dat$muslim_you
levels(dat$muslim_you2) <- c(3,1,2)
dat$muslim_you2 <- as.numeric(as.character(dat$muslim_you2))

dat$muslim_dem <- as.factor(dat$Q34_2)
dat$muslim_dem2 <- dat$muslim_dem
levels(dat$muslim_dem2) <- c(3,1,2)
dat$muslim_dem2 <- as.numeric(as.character(dat$muslim_dem2))

dat$muslim_biden <- as.factor(dat$Q34_3)
dat$muslim_biden2 <- dat$muslim_biden
levels(dat$muslim_biden2) <- c(3,1,2)
dat$muslim_biden2 <- as.numeric(as.character(dat$muslim_biden2))

dat$muslim_rep <- as.factor(dat$Q34_4)
dat$muslim_rep2 <- dat$muslim_rep
levels(dat$muslim_rep2) <- c(3,1,2)
dat$muslim_rep2 <- as.numeric(as.character(dat$muslim_rep2))

dat$muslim_trump <- as.factor(dat$Q34_5)
dat$muslim_trump2 <- dat$muslim_trump
levels(dat$muslim_trump2) <- c(3,1,2)
dat$muslim_trump2 <- as.numeric(as.character(dat$muslim_trump2))

dat$muslim_cand <- ifelse(dat$muslim_biden2<dat$muslim_trump2,1,0)
dat$muslim_party <- ifelse(dat$muslim_dem2<dat$muslim_rep2,1,0)
dat$muslim_cand[is.na(dat$muslim_cand)] <- 0 
dat$muslim_party[is.na(dat$muslim_party)] <- 0 

dat$muslim_cand_biden <- ifelse(dat$muslim_biden=="Disagree",1,0)
dat$muslim_cand_trump <- ifelse(dat$muslim_trump=="Agree",1,0)
dat$muslim_cand_biden[is.na(dat$muslim_cand_biden)] <- 0 
dat$muslim_cand_trump[is.na(dat$muslim_cand_trump)] <- 0 

dat$muslim_you2 <- (3-dat$muslim_you2)/2 

# Repeal ACA
dat$repealaca_you <- as.factor(dat$Q35_1)
dat$repealaca_you2 <- dat$repealaca_you
levels(dat$repealaca_you2) <- c(3,1,2)
dat$repealaca_you2 <- as.numeric(as.character(dat$repealaca_you2))

dat$repealaca_dem <- as.factor(dat$Q35_2)
dat$repealaca_dem2 <- dat$repealaca_dem
levels(dat$repealaca_dem2) <- c(3,1,2)
dat$repealaca_dem2 <- as.numeric(as.character(dat$repealaca_dem2))

dat$repealaca_biden <- as.factor(dat$Q35_3)
dat$repealaca_biden2 <- dat$repealaca_biden
levels(dat$repealaca_biden2) <- c(3,1,2)
dat$repealaca_biden2 <- as.numeric(as.character(dat$repealaca_biden2))

dat$repealaca_rep <- as.factor(dat$Q35_4)
dat$repealaca_rep2 <- dat$repealaca_rep
levels(dat$repealaca_rep2) <- c(3,1,2)
dat$repealaca_rep2 <- as.numeric(as.character(dat$repealaca_rep2))

dat$repealaca_trump <- as.factor(dat$Q35_5)
dat$repealaca_trump2 <- dat$repealaca_trump
levels(dat$repealaca_trump2) <- c(3,1,2)
dat$repealaca_trump2 <- as.numeric(as.character(dat$repealaca_trump2))

dat$repealaca_cand <- ifelse(dat$repealaca_biden2<dat$repealaca_trump2,1,0)
dat$repealaca_party <- ifelse(dat$repealaca_dem2<dat$repealaca_rep2,1,0)
dat$repealaca_cand[is.na(dat$repealaca_cand)] <- 0 
dat$repealaca_party[is.na(dat$repealaca_party)] <- 0 

dat$repealaca_cand_biden <- ifelse(dat$repealaca_biden=="Disagree",1,0)
dat$repealaca_cand_trump <- ifelse(dat$repealaca_trump=="Agree",1,0)
dat$repealaca_cand_biden[is.na(dat$repealaca_cand_biden)] <- 0 
dat$repealaca_cand_trump[is.na(dat$repealaca_cand_trump)] <- 0 

dat$repealaca_you2 <- (3-dat$repealaca_you2)/2 

# The United States should be part of the World Health Organization (WHO)
dat$who_you <- as.factor(dat$Q36_1)
dat$who_you2 <- dat$who_you
levels(dat$who_you2) <- c(3,1,2)
dat$who_you2 <- as.numeric(as.character(dat$who_you2))

dat$who_dem <- as.factor(dat$Q36_2)
dat$who_dem2 <- dat$who_dem
levels(dat$who_dem2) <- c(3,1,2)
dat$who_dem2 <- as.numeric(as.character(dat$who_dem2))

dat$who_biden <- as.factor(dat$Q36_3)
dat$who_biden2 <- dat$who_biden
levels(dat$who_biden2) <- c(3,1,2)
dat$who_biden2 <- as.numeric(as.character(dat$who_biden2))

dat$who_rep <- as.factor(dat$Q36_4)
dat$who_rep2 <- dat$who_rep
levels(dat$who_rep2) <- c(3,1,2)
dat$who_rep2 <- as.numeric(as.character(dat$who_rep2))

dat$who_trump <- as.factor(dat$Q36_5)
dat$who_trump2 <- dat$who_trump
levels(dat$who_trump2) <- c(3,1,2)
dat$who_trump2 <- as.numeric(as.character(dat$who_trump2))

dat$who_cand <- ifelse(dat$who_biden2>dat$who_trump2,1,0)
dat$who_party <- ifelse(dat$who_dem2>dat$who_rep2,1,0)
dat$who_cand[is.na(dat$who_cand)] <- 0 
dat$who_party[is.na(dat$who_party)] <- 0 

dat$who_cand_biden <- ifelse(dat$who_biden=="Agree",1,0)
dat$who_cand_trump <- ifelse(dat$who_trump=="Disagree",1,0)
dat$who_cand_biden[is.na(dat$who_cand_biden)] <- 0 
dat$who_cand_trump[is.na(dat$who_cand_trump)] <- 0 

dat$who_you2 <- (dat$who_you2-1)/2 

#Building a wall along the Mexican border
dat$mexicowall_you <- as.factor(dat$Q37_5)
dat$mexicowall_you2 <- dat$mexicowall_you
levels(dat$mexicowall_you2) <- c(3,1,2)
dat$mexicowall_you2 <- as.numeric(as.character(dat$mexicowall_you2))

dat$mexicowall_dem <- as.factor(dat$Q37_1)
dat$mexicowall_dem2 <- dat$mexicowall_dem 
levels(dat$mexicowall_dem2) <- c(3,1,2)
dat$mexicowall_dem2 <- as.numeric(as.character(dat$mexicowall_dem2))

dat$mexicowall_biden <- as.factor(dat$Q37_2)
dat$mexicowall_biden2 <- dat$mexicowall_biden
levels(dat$mexicowall_biden2) <- c(3,1,2)
dat$mexicowall_biden2 <- as.numeric(as.character(dat$mexicowall_biden2))

dat$mexicowall_rep <- as.factor(dat$Q37_3)
dat$mexicowall_rep2 <- dat$mexicowall_rep
levels(dat$mexicowall_rep2) <- c(3,1,2)
dat$mexicowall_rep2 <- as.numeric(as.character(dat$mexicowall_rep2))

dat$mexicowall_trump <- as.factor(dat$Q37_4)
dat$mexicowall_trump2 <- dat$mexicowall_trump
levels(dat$mexicowall_trump2) <- c(3,1,2)
dat$mexicowall_trump2 <- as.numeric(as.character(dat$mexicowall_trump2))

dat$mexicowall_cand <- ifelse(dat$mexicowall_biden2<dat$mexicowall_trump2,1,0)
dat$mexicowall_party <- ifelse(dat$mexicowall_dem2<dat$mexicowall_rep2,1,0)
dat$mexicowall_cand[is.na(dat$mexicowall_cand)] <- 0 
dat$mexicowall_party[is.na(dat$mexicowall_party)] <- 0 

dat$mexicowall_cand_biden <- ifelse(dat$mexicowall_biden=="Disagree",1,0)
dat$mexicowall_cand_trump <- ifelse(dat$mexicowall_trump=="Agree",1,0)
dat$mexicowall_cand_biden[is.na(dat$mexicowall_cand_biden)] <- 0 
dat$mexicowall_cand_trump[is.na(dat$mexicowall_cand_trump)] <- 0 

dat$mexicowall_you2 <- (3-dat$mexicowall_you2)/2 

# Requiring people to wear masks in public to slow the spread of the coronavirus 
dat$maskspublic_you <- as.factor(dat$Q38_5)
dat$maskspublic_you2 <- dat$maskspublic_you
levels(dat$maskspublic_you2) <- c(3,1,2)
dat$maskspublic_you2 <- as.numeric(as.character(dat$maskspublic_you2))

dat$maskspublic_dem <- as.factor(dat$Q38_1)
dat$maskspublic_dem2 <- dat$maskspublic_dem
levels(dat$maskspublic_dem2) <- c(3,1,2)
dat$maskspublic_dem2 <- as.numeric(as.character(dat$maskspublic_dem2))

dat$maskspublic_biden <- as.factor(dat$Q38_2)
dat$maskspublic_biden2 <- dat$maskspublic_biden
levels(dat$maskspublic_biden2) <- c(3,1,2)
dat$maskspublic_biden2 <- as.numeric(as.character(dat$maskspublic_biden2))

dat$maskspublic_rep <- as.factor(dat$Q38_3)
dat$maskspublic_rep2 <- dat$maskspublic_rep
levels(dat$maskspublic_rep2) <- c(3,1,2)
dat$maskspublic_rep2 <- as.numeric(as.character(dat$maskspublic_rep2))

dat$maskspublic_trump <- as.factor(dat$Q38_4)
dat$maskspublic_trump2 <- dat$maskspublic_trump
levels(dat$maskspublic_trump2) <- c(3,1,2)
dat$maskspublic_trump2 <- as.numeric(as.character(dat$maskspublic_trump2))

dat$maskspublic_cand <- ifelse(dat$maskspublic_biden2>dat$maskspublic_trump2,1,0)
dat$maskspublic_party <- ifelse(dat$maskspublic_dem2>dat$maskspublic_rep2,1,0)
dat$maskspublic_cand[is.na(dat$maskspublic_cand)] <- 0 
dat$maskspublic_party[is.na(dat$maskspublic_party)] <- 0 

dat$maskspublic_cand_biden <- ifelse(dat$maskspublic_biden=="Agree",1,0)
dat$maskspublic_cand_trump <- ifelse(dat$maskspublic_trump=="Disagree",1,0)
dat$maskspublic_cand_biden[is.na(dat$maskspublic_cand_biden)] <- 0 
dat$maskspublic_cand_trump[is.na(dat$maskspublic_cand_trump)] <- 0 

dat$maskspublic_you2 <- (dat$maskspublic_you2-1)/2 

#Increasing free trade with other countries
dat$freetrade_you <- as.factor(dat$Q39_1)
dat$freetrade_you2 <- dat$freetrade_you
levels(dat$freetrade_you2) <- c(3,1,2)
dat$freetrade_you2 <- as.numeric(as.character(dat$freetrade_you2))

dat$freetrade_dem <- as.factor(dat$Q39_2)
dat$freetrade_dem2 <- dat$freetrade_dem
levels(dat$freetrade_dem2) <- c(3,1,2)
dat$freetrade_dem2 <- as.numeric(as.character(dat$freetrade_dem2))

dat$freetrade_biden <- as.factor(dat$Q39_3)
dat$freetrade_biden2 <- dat$freetrade_biden
levels(dat$freetrade_biden2) <- c(3,1,2)
dat$freetrade_biden2 <- as.numeric(as.character(dat$freetrade_biden2))

dat$freetrade_rep <- as.factor(dat$Q39_4)
dat$freetrade_rep2 <- dat$freetrade_rep
levels(dat$freetrade_rep2) <- c(3,1,2)
dat$freetrade_rep2 <- as.numeric(as.character(dat$freetrade_rep2))

dat$freetrade_trump <- as.factor(dat$Q39_5)
dat$freetrade_trump2 <- dat$freetrade_trump
levels(dat$freetrade_trump2) <- c(3,1,2)
dat$freetrade_trump2 <- as.numeric(as.character(dat$freetrade_trump2))

dat$freetrade_cand <- ifelse(dat$freetrade_biden2>dat$freetrade_trump2,1,0)
dat$freetrade_party <- ifelse(dat$freetrade_dem2>dat$freetrade_rep2,1,0)
dat$freetrade_cand[is.na(dat$freetrade_cand)] <- 0 
dat$freetrade_party[is.na(dat$freetrade_party)] <- 0 

dat$freetrade_cand_biden <- ifelse(dat$freetrade_biden=="Agree",1,0)
dat$freetrade_cand_trump <- ifelse(dat$freetrade_trump=="Disagree",1,0)
dat$freetrade_cand_biden[is.na(dat$freetrade_cand_biden)] <- 0 
dat$freetrade_cand_trump[is.na(dat$freetrade_cand_trump)] <- 0 

dat$freetrade_you2 <- (dat$freetrade_you2-1)/2 

#Reducing the size of the income gap between rich and poor Americans
dat$incomegap_you <- as.factor(dat$Q40_1)
dat$incomegap_you2 <- dat$incomegap_you
levels(dat$incomegap_you2) <- c(3,1,2)
dat$incomegap_you2 <- as.numeric(as.character(dat$incomegap_you2))

dat$incomegap_dem <- as.factor(dat$Q40_2)
dat$incomegap_dem2 <- dat$incomegap_dem
levels(dat$incomegap_dem2) <- c(3,1,2)
dat$incomegap_dem2 <- as.numeric(as.character(dat$incomegap_dem2))

dat$incomegap_biden <- as.factor(dat$Q40_3)
dat$incomegap_biden2 <- dat$incomegap_biden
levels(dat$incomegap_biden2) <- c(3,1,2)
dat$incomegap_biden2 <- as.numeric(as.character(dat$incomegap_biden2))

dat$incomegap_rep <- as.factor(dat$Q40_4)
dat$incomegap_rep2 <- dat$incomegap_rep
levels(dat$incomegap_rep2) <- c(3,1,2)
dat$incomegap_rep2 <- as.numeric(as.character(dat$incomegap_rep2))

dat$incomegap_trump <- as.factor(dat$Q40_5)
dat$incomegap_trump2 <- dat$incomegap_trump
levels(dat$incomegap_trump2) <- c(3,1,2)
dat$incomegap_trump2 <- as.numeric(as.character(dat$incomegap_trump2))

dat$incomegap_whites <- as.factor(dat$Q40_6)
dat$incomegap_whites2 <- dat$incomegap_whites
levels(dat$incomegap_whites2) <- c(3,1,2)
dat$incomegap_whites2 <- as.numeric(as.character(dat$incomegap_whites2))

dat$incomegap_blacks <- as.factor(dat$Q40_7)
dat$incomegap_blacks2 <- dat$incomegap_blacks
levels(dat$incomegap_blacks2) <- c(3,1,2)
dat$incomegap_blacks2 <- as.numeric(as.character(dat$incomegap_blacks2))

dat$incomegap_cand <- ifelse(dat$incomegap_biden2>dat$incomegap_trump2,1,0)
dat$incomegap_party <- ifelse(dat$incomegap_dem2>dat$incomegap_rep2,1,0)
dat$incomegap_groups <- ifelse(dat$incomegap_blacks2>dat$incomegap_whites2,1,0)
dat$incomegap_cand[is.na(dat$incomegap_cand)] <- 0 
dat$incomegap_party[is.na(dat$incomegap_party)] <- 0 
dat$incomegap_groups[is.na(dat$incomegap_groups)] <- 0 

dat$incomegap_cand_biden <- ifelse(dat$incomegap_biden=="Agree",1,0)
dat$incomegap_cand_trump <- ifelse(dat$incomegap_trump=="Disagree",1,0)
dat$incomegap_cand_biden[is.na(dat$incomegap_cand_biden)] <- 0 
dat$incomegap_cand_trump[is.na(dat$incomegap_cand_trump)] <- 0 

dat$incomegap_you2 <- (dat$incomegap_you2-1)/2

# Closing non-essential businesses to slow the spread of the coronavirus
dat$closebusiness_you <- as.factor(dat$Q41_5)
dat$closebusiness_you2 <- dat$closebusiness_you
levels(dat$closebusiness_you2) <- c(3,1,2)
dat$closebusiness_you2 <- as.numeric(as.character(dat$closebusiness_you2))

dat$closebusiness_dem <- as.factor(dat$Q41_1)
dat$closebusiness_dem2 <- dat$closebusiness_dem
levels(dat$closebusiness_dem2) <- c(3,1,2)
dat$closebusiness_dem2 <- as.numeric(as.character(dat$closebusiness_dem2))

dat$closebusiness_biden <- as.factor(dat$Q41_2)
dat$closebusiness_biden2 <- dat$closebusiness_biden
levels(dat$closebusiness_biden2) <- c(3,1,2)
dat$closebusiness_biden2 <- as.numeric(as.character(dat$closebusiness_biden2))

dat$closebusiness_rep <- as.factor(dat$Q41_3)
dat$closebusiness_rep2 <- dat$closebusiness_rep
levels(dat$closebusiness_rep2) <- c(3,1,2)
dat$closebusiness_rep2 <- as.numeric(as.character(dat$closebusiness_rep2))

dat$closebusiness_trump <- as.factor(dat$Q41_4)
dat$closebusiness_trump2 <- dat$closebusiness_trump
levels(dat$closebusiness_trump2) <- c(3,1,2)
dat$closebusiness_trump2 <- as.numeric(as.character(dat$closebusiness_trump2))

dat$closebusiness_cand <- ifelse(dat$closebusiness_biden2>dat$closebusiness_trump2,1,0)
dat$closebusiness_party <- ifelse(dat$closebusiness_dem2>dat$closebusiness_rep2,1,0)
dat$closebusiness_cand[is.na(dat$closebusiness_cand)] <- 0 
dat$closebusiness_party[is.na(dat$closebusiness_party)] <- 0 

dat$closebusiness_cand_biden <- ifelse(dat$closebusiness_biden=="Agree",1,0)
dat$closebusiness_cand_trump <- ifelse(dat$closebusiness_trump=="Disagree",1,0)
dat$closebusiness_cand_biden[is.na(dat$closebusiness_cand_biden)] <- 0 
dat$closebusiness_cand_trump[is.na(dat$closebusiness_cand_trump)] <- 0 

dat$closebusiness_you2 <- (dat$closebusiness_you2-1)/2

# Respondents' policy positions on other issues (issues without perceptions)
# Allowing existing illegal immigrants to obtain a work permit to stay in the US
# These are scaled so that higher values indicate more liberal positions
dat$workpermit <- as.factor(dat$Q42_1)
levels(dat$workpermit) <- c(3,4,2,5,1)
dat$workpermit <- (as.numeric(as.character(dat$workpermit))-1)/4

#Increasing taxes on households with very high incomes
dat$highincomes <- as.factor(dat$Q42_2)
levels(dat$highincomes) <- c(3,4,2,5,1)
dat$highincomes <- (as.numeric(as.character(dat$highincomes))-1)/4

# Blacks should work their way up without any special favors
dat$blackswork <- as.factor(dat$Q42_3)
levels(dat$blackswork) <- c(3,4,2,5,1)
dat$blackswork <- (5-as.numeric(as.character(dat$blackswork)))/4

#Requiring people to have health insurance
dat$healthinsurance <- as.factor(dat$Q42_4)
levels(dat$healthinsurance) <- c(3,4,2,5,1)
dat$healthinsurance <- (as.numeric(as.character(dat$healthinsurance))-1)/4

#Restrictions on abortions in the LAST THREE MONTHS of pregnancy
dat$abortionlast3 <- as.factor(dat$Q42_5)
levels(dat$abortionlast3) <- c(3,4,2,5,1)
dat$abortionlast3 <- (5-as.numeric(as.character(dat$abortionlast3)))/4

#Restrictions on abortions in the FIRST THREE MONTHS of pregnancy
dat$abortionfirst3 <- as.factor(dat$Q42_6)
levels(dat$abortionfirst3) <- c(3,4,2,5,1)
dat$abortionfirst3 <- (5-as.numeric(as.character(dat$abortionfirst3)))/4

#Deporting all people who entered the US illegally back to their country of origin
dat$deportillegals <- as.factor(dat$Q42_7)
levels(dat$deportillegals) <- c(3,4,2,5,1)
dat$deportillegals <- (5-as.numeric(as.character(dat$deportillegals)))/4

#Appointing judges to the Supreme Court who would overturn the Roe v Wade decision that allowed abortion
dat$roevwade <- as.factor(dat$Q42_8)
levels(dat$roevwade) <- c(3,4,2,5,1)
dat$roevwade <- (5-as.numeric(as.character(dat$roevwade)))/4

#White people in the US have advantages because of the color of their skin
dat$whiteadvantage <- as.factor(dat$Q42_9)
levels(dat$whiteadvantage) <- c(3,4,2,5,1)
dat$whiteadvantage <- (as.numeric(as.character(dat$whiteadvantage))-1)/4

#Affirmative action to help Blacks
dat$affirmativeaction <- as.factor(dat$Q42_10)
levels(dat$affirmativeaction) <- c(3,4,2,5,1)
dat$affirmativeaction <- (as.numeric(as.character(dat$affirmativeaction))-1)/4

# Stopping employers from hiring illegal immigrants
dat$stophiring <- as.factor(dat$Q43_1)
levels(dat$stophiring) <- c(1,5,2,4,3)
dat$stophiring <- (5-as.numeric(as.character(dat$stophiring)))/4

# Coronavirus testing
dat$coronavirustesting <- as.factor(dat$Q43_2)
levels(dat$coronavirustesting) <- c(1,5,2,4,3)
dat$coronavirustesting <- (as.numeric(as.character(dat$coronavirustesting))-1)/4

# Protecting the environment
dat$protectenv <- as.factor(dat$Q43_3)
levels(dat$protectenv) <- c(1,5,2,4,3)
dat$protectenv <- (as.numeric(as.character(dat$protectenv))-1)/4

# Making sure people with the coronavirus stay at home
dat$stayathome <- as.factor(dat$Q43_4)
levels(dat$stayathome) <- c(1,5,2,4,3)
dat$stayathome <- (as.numeric(as.character(dat$stayathome))-1)/4

# Protecting Americans from the coronavirus
dat$protectcoronavirus <- as.factor(dat$Q43_5)
levels(dat$protectcoronavirus) <- c(1,5,2,4,3)
dat$protectcoronavirus <- (as.numeric(as.character(dat$protectcoronavirus))-1)/4

# Protecting the air we breathe
dat$protectair <- as.factor(dat$Q43_6)
levels(dat$protectair) <- c(1,5,2,4,3)
dat$protectair <- (as.numeric(as.character(dat$protectair))-1)/4

#Developing alternative sources of energy that do not depend on oil
dat$alternativeenergy <- as.factor(dat$Q44_1)
levels(dat$alternativeenergy) <- c(1,5,2,4,3)
dat$alternativeenergy <- (as.numeric(as.character(dat$alternativeenergy))-1)/4

# Health care
dat$healthcare <- as.factor(dat$Q44_2)
levels(dat$healthcare) <- c(1,5,2,4,3)
dat$healthcare <- (as.numeric(as.character(dat$healthcare))-1)/4

# Health care assistance to people who cannot afford health insurance
dat$healthassist <- as.factor(dat$Q44_3)
levels(dat$healthassist) <- c(1,5,2,4,3)
dat$healthassist <- (as.numeric(as.character(dat$healthassist))-1)/4

# Assistance to poor people
dat$assistpoor <- as.factor(dat$Q44_4)
levels(dat$assistpoor) <- c(1,5,2,4,3)
dat$assistpoor <- (as.numeric(as.character(dat$assistpoor))-1)/4

# Helping Black Americans get ahead
dat$helpblacks <- as.factor(dat$Q44_5)
levels(dat$helpblacks) <- c(1,5,2,4,3)
dat$helpblacks <- (as.numeric(as.character(dat$helpblacks))-1)/4


# Party id
dat$pid <- as.factor(dat$Q3)
levels(dat$pid) <- c("Democrat","Independent","Other","Republican") 

dat$strrep <- as.factor(dat$Q4)
levels(dat$strrep) <- c("Not very strong","Strong")

dat$strdem <- as.factor(dat$Q5)
levels(dat$strdem) <- c("Not very strong","Strong")

dat$leaner <- as.factor(dat$Q6)
levels(dat$leaner) <- c("Closer Democrat","Closer Republican","Neither")

# Party id scale
dat$pidscale <- NA
dat$pidscale[dat$pid=="Democrat" & dat$strdem=="Strong"] <- 1
dat$pidscale[dat$pid=="Democrat" & dat$strdem=="Not very strong"] <- 2
dat$pidscale[dat$leaner=="Closer Democrat"] <- 3
dat$pidscale[dat$leaner=="Neither" & dat$pid=="Independent"] <- 4
dat$pidscale[dat$leaner=="Closer Republican"] <- 5
dat$pidscale[dat$pid=="Republican" & dat$strrep=="Not very strong"] <- 6
dat$pidscale[dat$pid=="Republican" & dat$strrep=="Strong"] <- 7

# Party id as a social identity scale
dat$pidsoc_dem1 <- as.factor(dat$Q7_1)
levels(dat$pidsoc_dem1) <- c(3,4,2,5,1)
dat$pidsoc_dem1 <- (as.numeric(as.character(dat$pidsoc_dem1))-1)/4

dat$pidsoc_dem2 <- as.factor(dat$Q7_2)
levels(dat$pidsoc_dem2) <- c(3,4,2,5,1)
dat$pidsoc_dem2 <- (as.numeric(as.character(dat$pidsoc_dem2))-1)/4

dat$pidsoc_dem3 <- as.factor(dat$Q7_3)
levels(dat$pidsoc_dem3) <- c(3,4,2,5,1)
dat$pidsoc_dem3 <- (as.numeric(as.character(dat$pidsoc_dem3))-1)/4

dat$pidsoc_dem4 <- as.factor(dat$Q7_4)
levels(dat$pidsoc_dem4) <- c(3,4,2,5,1)
dat$pidsoc_dem4 <- (as.numeric(as.character(dat$pidsoc_dem4))-1)/4

dat$pidsoc_dem5 <- as.factor(dat$Q7_5) 
levels(dat$pidsoc_dem5) <- c(3,4,2,5,1)
dat$pidsoc_dem5 <- (as.numeric(as.character(dat$pidsoc_dem5))-1)/4

dat$pidsoc_dem6 <- as.factor(dat$Q7_6) 
levels(dat$pidsoc_dem6) <- c(3,4,2,5,1)
dat$pidsoc_dem6 <- (as.numeric(as.character(dat$pidsoc_dem6))-1)/4

dat$pidsoc_dem7 <- as.factor(dat$Q7_7) 
levels(dat$pidsoc_dem7) <- c(3,4,2,5,1)
dat$pidsoc_dem7 <- (as.numeric(as.character(dat$pidsoc_dem7))-1)/4

dat$pidsoc_dem8 <- as.factor(dat$Q7_8)  
levels(dat$pidsoc_dem8) <- c(3,4,2,5,1)
dat$pidsoc_dem8 <- (as.numeric(as.character(dat$pidsoc_dem8))-1)/4

psych::alpha(cbind(dat$pidsoc_dem1,dat$pidsoc_dem2,dat$pidsoc_dem3,dat$pidsoc_dem4,dat$pidsoc_dem5,dat$pidsoc_dem6,dat$pidsoc_dem7,dat$pidsoc_dem8))


dat$pidsoc_rep1 <- as.factor(dat$Q8_1)
levels(dat$pidsoc_rep1) <- c(3,4,2,5,1)
dat$pidsoc_rep1 <- (as.numeric(as.character(dat$pidsoc_rep1))-1)/4

dat$pidsoc_rep2 <- as.factor(dat$Q8_2)
levels(dat$pidsoc_rep2) <- c(3,4,2,5,1)
dat$pidsoc_rep2 <- (as.numeric(as.character(dat$pidsoc_rep2))-1)/4

dat$pidsoc_rep3 <- as.factor(dat$Q8_3)
levels(dat$pidsoc_rep3) <- c(3,4,2,5,1)
dat$pidsoc_rep3 <- (as.numeric(as.character(dat$pidsoc_rep3))-1)/4

dat$pidsoc_rep4 <- as.factor(dat$Q8_4)
levels(dat$pidsoc_rep4) <- c(3,4,2,5,1)
dat$pidsoc_rep4 <- (as.numeric(as.character(dat$pidsoc_rep4))-1)/4

dat$pidsoc_rep5 <- as.factor(dat$Q8_5) 
levels(dat$pidsoc_rep5) <- c(3,4,2,5,1)
dat$pidsoc_rep5 <- (as.numeric(as.character(dat$pidsoc_rep5))-1)/4

dat$pidsoc_rep6 <- as.factor(dat$Q8_6) 
levels(dat$pidsoc_rep6) <- c(3,4,2,5,1)
dat$pidsoc_rep6 <- (as.numeric(as.character(dat$pidsoc_rep6))-1)/4

dat$pidsoc_rep7 <- as.factor(dat$Q8_7) 
levels(dat$pidsoc_rep7) <- c(3,4,2,5,1)
dat$pidsoc_rep7 <- (as.numeric(as.character(dat$pidsoc_rep7))-1)/4

dat$pidsoc_rep8 <- as.factor(dat$Q8_8)  
levels(dat$pidsoc_rep8) <- c(3,4,2,5,1)
dat$pidsoc_rep8 <- (as.numeric(as.character(dat$pidsoc_rep8))-1)/4

psych::alpha(cbind(dat$pidsoc_rep1,dat$pidsoc_rep2,dat$pidsoc_rep3,dat$pidsoc_rep4,dat$pidsoc_rep5,dat$pidsoc_rep6,dat$pidsoc_rep7,dat$pidsoc_rep8))

dat$pidsoc_dem <- apply(cbind(dat$pidsoc_dem1,dat$pidsoc_dem2,dat$pidsoc_dem3,dat$pidsoc_dem4,dat$pidsoc_dem5,dat$pidsoc_dem6,dat$pidsoc_dem7,dat$pidsoc_dem8),1,mean,na.rm=T)
dat$pidsoc_rep <- apply(cbind(dat$pidsoc_rep1,dat$pidsoc_rep2,dat$pidsoc_rep3,dat$pidsoc_rep4,dat$pidsoc_rep5,dat$pidsoc_rep6,dat$pidsoc_rep7,dat$pidsoc_rep8),1,mean,na.rm=T)

# Code people who do not identify with a party as 0
dat$pidsoc_dem[dat$pidscale>3] <- 0
dat$pidsoc_rep[dat$pidscale<5] <- 0

dat$pidscale[is.na(dat$pidscale)] <- 4
dat$pidsocscale <- NA
dat$pidsocscale[dat$pidscale<4] <- -dat$pidsoc_dem[dat$pidscale<4]
dat$pidsocscale[dat$pidscale==4] <- 0
dat$pidsocscale[dat$pidscale>4] <- dat$pidsoc_rep[dat$pidscale>4]

dat$trumpapproval <- dat$Q22

# Coronavirus importance variables 
dat$coronavirus_test <- as.factor(dat$Q24)
levels(dat$coronavirus_test) <- c(0,1)
dat$coronavirus_test <- as.numeric(as.character(dat$coronavirus_test))

dat$coronavirus_died <- as.factor(dat$Q25)
levels(dat$coronavirus_died) <- c(0,1)
dat$coronavirus_died <- as.numeric(as.character(dat$coronavirus_died))


dat$covidapproval_trump <- as.factor(dat$Q26_1)
dat$covidapproval_governor <- as.factor(dat$Q26_2)


# Party/candidate ratings
dat$rating_dp <- as.factor(dat$Q45_1)
levels(dat$rating_dp) <- 1:7
dat$rating_dp <- as.numeric(as.character(dat$rating_dp))
dat$rating_biden <- as.factor(dat$Q45_2)
levels(dat$rating_biden) <- 1:7
dat$rating_biden <- as.numeric(as.character(dat$rating_biden))
dat$rating_rp <- as.factor(dat$Q45_3)
levels(dat$rating_rp) <- 1:7
dat$rating_rp <- as.numeric(as.character(dat$rating_rp))
dat$rating_trump <- as.factor(dat$Q45_4)
levels(dat$rating_trump) <- 1:7
dat$rating_trump <- as.numeric(as.character(dat$rating_trump))

dat$trumpbiden <- dat$rating_trump - dat$rating_biden


# Voter turnout and vote choice
dat$voted <- as.factor(dat$Q83)

# Vote choice
dat$Q46[is.na(dat$Q46)] <- "No response"
dat$vote <- as.factor(dat$Q46)
levels(dat$vote) <- c("Trump","Biden","No response","Other")


# liberal/conservative self/party/candidates

dat$libcon_you <- as.factor(dat$Q47_1)
dat$libcon_dp <- as.factor(dat$Q47_2)
dat$libcon_biden <- as.factor(dat$Q47_3)
dat$libcon_rp <- as.factor(dat$Q47_4)
dat$libcon_trump <- as.factor(dat$Q47_5)

# Political knowledge
dat$pelosi <- ifelse(dat$Q92=="Speaker of the House of Representatives",1,0)
dat$housecontrol <- ifelse(dat$Q93...162=="Democrats",1,0)
dat$senatecontrol <- ifelse(dat$Q94=="Republicans",1,0)
dat$barrett <- ifelse(dat$Q95=="Amy Coney Barrett",1,0)
dat$harris <- ifelse(dat$Q96=="US senator",1,0)

# Pol knowledge scale
dat$polk <- dat$pelosi + dat$housecontrol + dat$senatecontrol + dat$barrett + dat$harris

# Covid outcomes
dat$friends_24hrs <- dat$Q90

dat$maskpastweek <- as.factor(dat$Q91...167)

dat$maskspublic <- as.factor(dat$Q87)
dat$maskspublic2 <- dat$maskspublic
levels(dat$maskspublic2) <- c(1,0.8,0,0.2,0.4,0.6)

dat$sixfeet <- as.factor(dat$Q89)
dat$sixfeet2 <- dat$sixfeet
levels(dat$sixfeet2) <- c(1,0.8,0,0.2,0.4,0.6)

dat$covidthreat <- as.factor(dat$Q91...170)
dat$covidthreat2 <- dat$covidthreat
levels(dat$covidthreat2) <- c(0.25,1,0,0.5,0.75)

dat$prepandemiclives <- as.factor(dat$Q93...171)
dat$prepandemiclives2 <- dat$prepandemiclives
levels(dat$prepandemiclives2) <-  c(0.5,0.75,0.25,1,0)

dat$covidserious_trump <- as.factor(dat$Q95_1)
levels(dat$covidserious_trump) <- c(1,0,0.25,0.5,0.75)
dat$covidserious_trump <- as.numeric(as.character(dat$covidserious_trump))

dat$covidserious_biden <- as.factor(dat$Q95_2)
levels(dat$covidserious_biden) <- c(1,0,0.25,0.5,0.75)
dat$covidserious_biden <- as.numeric(as.character(dat$covidserious_biden))

dat$covidserious_cand <- ifelse(dat$covidserious_biden>dat$covidserious_trump,1,0)


# LGB question
dat$lgb <- as.factor(dat$Q81)

# Scales
# Function to create scales 
scalefun <- function(x){
  mean(na.omit(x),na.rm=T)
}
## Blacks
psych::alpha(cbind(dat$improveblacks_you2,dat$blackswork,dat$whiteadvantage,dat$affirmativeaction,dat$helpblacks))
dat$blacks <- apply(cbind(dat$improveblacks_you2,dat$blackswork,dat$whiteadvantage,dat$affirmativeaction,dat$helpblacks),1,scalefun)
dat$blacks <- dat$blacks

## Redistribution
psych::alpha(cbind(dat$incomegap_you2,dat$highincomes,dat$healthassist,dat$assistpoor))
dat$redistribution <- apply(cbind(dat$incomegap_you2,dat$highincomes,dat$healthassist,dat$assistpoor),1,scalefun)
dat$redistribution <- dat$redistribution

# Abortion
psych::alpha(cbind(dat$abortion_you2,dat$abortionlast3,dat$abortionfirst3,dat$roevwade))
dat$abortion <- apply(cbind(dat$abortion_you2,dat$abortionlast3,dat$abortionfirst3,dat$roevwade),1,scalefun)
dat$abortion <- dat$abortion

# Environment
psych::alpha(cbind(dat$co2_you2,dat$protectenv,dat$protectair,dat$alternativeenergy))
dat$environment <- apply(cbind(dat$co2_you2,dat$protectenv,dat$protectair,dat$alternativeenergy),1,scalefun)
dat$environment <- dat$environment

# Immigration
psych::alpha(cbind(dat$muslim_you2,dat$mexicowall_you2,dat$workpermit,dat$deportillegals,dat$stophiring))
dat$immigration <- apply(cbind(dat$muslim_you2,dat$mexicowall_you2,dat$workpermit,dat$deportillegals,dat$stophiring),1,scalefun)
dat$immigration <- dat$immigration

# Health care
psych::alpha(cbind(dat$repealaca_you2,dat$healthinsurance,dat$healthcare,dat$healthassist))
dat$healthcare <- apply(cbind(dat$repealaca_you2,dat$healthinsurance,dat$healthcare,dat$healthassist),1,scalefun)
dat$healthcare <- dat$healthcare

# Coronavirus 
psych::alpha(cbind(dat$maskspublic_you2,dat$who_you2,dat$closebusiness_you2,dat$coronavirustesting,dat$stayathome,dat$protectcoronavirus))
dat$coronavirus <- apply(cbind(dat$maskspublic_you2,dat$who_you2,dat$closebusiness_you2,dat$coronavirustesting,dat$stayathome,dat$protectcoronavirus),1,scalefun)

# Others: ssmban_you2, lowertax_you2, backchecks_you2, freetrade_you2, incomegap_you2

# Coronavirus concern scale
psych::alpha(cbind(dat$masksself2,dat$sixfeet2,dat$covidthreat2,dat$prepandemiclives2))
dat$coronavirusconcern <- apply(cbind(dat$masksself2,dat$sixfeet2,dat$covidthreat2,dat$prepandemiclives2),1,scalefun)
dat$coronavirusconcern <- 2*dat$coronavirusconcern - 1


# Overall knowledge
dat$candcorrect <- dat$improveblacks_cand + dat$ssmban_cand + dat$lowertax_cand + dat$abortion_cand + dat$backchecks_cand + dat$co2_cand + dat$muslim_cand + dat$repealaca_cand + dat$who_cand + dat$mexicowall_cand + dat$maskspublic_cand + dat$freetrade_cand + dat$incomegap_cand + dat$closebusiness_cand
dat$candcorrect <- dat$candcorrect/14

dat$partycorrect <- dat$improveblacks_party + dat$ssmban_party + dat$lowertax_party + dat$abortion_party + dat$backchecks_party + dat$co2_party + dat$muslim_party + dat$repealaca_party + dat$who_party + dat$mexicowall_party + dat$maskspublic_party + dat$incomegap_party + dat$closebusiness_party
dat$partycorrect <- dat$partycorrect/13



colnames(dat) <- paste0(colnames(dat),"_post")
dat <- dat[,c(1,2,3,7,8,19,20,21,22,23,177:482)]
save(dat,file="Data/Cleaned/postelection.Rda")
