## This file imports the raw dataset from the survey 
## (Data_DRC_CRSVSurveyResponses.csv)  and
## codes new variables for subsequent analyses
## The file then outputs (Data_DRC_CRSVwCodedVars.csv)
## which includes the newly coded variables alongside the raw variables
## This command file also creates specific data frames for
## use in analyses with no missing variables
## according to list experiment package requirements.

# This file is designed to run after the following files
# uncomment to run previous coding files
# source("00_Packages.R")

# Import data ------------------------------------------------------------
D <- read.csv("Data_DRC_CRSVSurveyResponses.csv")

D <- data.frame(D)

### code variables ##########################################

## main independent variables
table(D$nb_items, useNA ='ifany')
D$treat_list_CRSV<-ifelse(D$nb_items==3, 0 ,1)
table(D$rape_yes, useNA ='ifany')

## main outcome variables####################################

# Personal exchanges

table(D$ingroup_visit_out, useNA='ifany')
table(D$ingroup_visit_in, useNA='ifany')
length(D$ingroup_visit_out)

D$ingroup_visit_mean <- NA
D$ingroup_visit_mean <- (D$ingroup_visit_in+D$ingroup_visit_out)/2
table(D$ingroup_visit_mean, useNA='ifany')

# Organization Leader
D$org_leader_d <- D$org_leader # recode such that >1=1
D$org_leader_d[D$org_leader>=1 & D$org_leader<9] <- 1 
table(D$org_leader_d, useNA = 'ifany') 

D$org_leader_d <- NA
D$org_leader_d[D$farmer_member==0 | D$health_member==0 | D$edu_member==0 | D$NGO_member==0] <- 0
D$org_leader_d[D$farmer_member==1 & D$famer_role==1] <- 1  ## farmer_role
D$org_leader_d[D$health_member==1 & D$health_role==1] <- 1
D$org_leader_d[D$edu_member==1 & D$edu_role==1] <- 1
D$org_leader_d[D$NGO_member==1 & D$NGO_role==1] <- 1
table(D$org_leader_d, useNA='ifany')

#civic engagement  - social and political action

table(D$farmer_member, useNA='ifany')
table(D$health_member, useNA='ifany')
table(D$edu_member, useNA='ifany')
table(D$NGO_member, useNA='ifany')
                        
# organization membership

D$org_member_r1 <- D$farmer_member + D$health_member + D$edu_member + D$NGO_member
table(D$org_member_r1, useNA='ifany')

# Event Engagements

table(D$event_comwork, useNA = "ifany")
D$event_comwork_r1 <- NA
D$event_comwork_r1[D$event_comwork>=0] <- D$event_comwork[D$event_comwork>=0]
table(D$event_comwork_r1,D$event_comwork, useNA = 'ifany')
D$event_comwork_r2 <- NA # reverse coding so high level is more often
  D$event_comwork_r2[D$event_comwork_r1==1] <- 4
  D$event_comwork_r2[D$event_comwork_r1==2] <- 3  
  D$event_comwork_r2[D$event_comwork_r1==3] <- 2  
  D$event_comwork_r2[D$event_comwork_r1==4] <- 1 
  D$event_comwork_r2[D$event_comwork_r1==5] <- 0
table(D$event_comwork_r2, D$event_comwork, useNA = "ifany")

table(D$event_community, useNA = 'ifany')
D$event_community_r1 <- NA  # reverse coding so high level is more often
  D$event_community_r1[D$event_community==1] <- 4
  D$event_community_r1[D$event_community==2] <- 3  
  D$event_community_r1[D$event_community==3] <- 2  
  D$event_community_r1[D$event_community==4] <- 1 
  D$event_community_r1[D$event_community==5] <- 0
table(D$event_community, D$event_community_r1, useNA = "ifany")

D$event_com_mean <- (D$event_comwork_r2 + D$event_community_r1)/2 # take average of the two measures
table(D$event_com_mean, useNA='ifany')

# public goods
table(D$donate_yes, D$donate_amount, useNA ='ifany')

## code mechanisms ###############################################

#anticipated stigma
table(D$stigma_anticip1, useNA='ifany')
table(D$stigma_anticip2, useNA='ifany')
table(D$stigma_anticip3, useNA='ifany')

D$stigma_anticip_m <- NA
D$stigma_anticip_m <- rowMeans(D[,c("stigma_anticip1", "stigma_anticip2", "stigma_anticip3")], na.rm=TRUE)

#internal stigma

table(D$stigma_internal1, useNA='ifany')
table(D$stigma_internal2, useNA='ifany')
table(D$stigma_internal3, useNA='ifany')
table(D$stigma_internal5, useNA='ifany')
table(D$stigma_internal6, useNA='ifany')  ## SUMMER CHECK INTERNAL 7 AND INTERNAL 4 IN CODEBOOK WHICH ARE NOT HERE

D$stigma_internal_r1 <- (D$stigma_internal1 + D$stigma_internal2 + D$stigma_internal3 + 
                         D$stigma_internal5 + D$stigma_internal6)/5
table(D$stigma_internal_r1, useNA='ifany')

#experienced stigma
table(D$stigma_experienc1, useNA = 'ifany')
D$stigma_experienc1_r1 <- NA
D$stigma_experienc1_r1[D$stigma_experienc1==1] <- 4
D$stigma_experienc1_r1[D$stigma_experienc1==2] <- 3
D$stigma_experienc1_r1[D$stigma_experienc1==3]<- 2
D$stigma_experienc1_r1[D$stigma_experienc1==4] <- 1
D$stigma_experienc1_r1[D$stigma_experienc1==5] <- 0
table(D$stigma_experienc1, D$stigma_experienc1_r1, useNA = 'ifany')

table(D$stigma_experienc2, useNA = 'ifany')
D$stigma_experienc2_r1 <- NA
D$stigma_experienc2_r1[D$stigma_experienc2==1] <- 4
D$stigma_experienc2_r1[D$stigma_experienc2==2] <- 3
D$stigma_experienc2_r1[D$stigma_experienc2==3]<- 2
D$stigma_experienc2_r1[D$stigma_experienc2==4] <- 1
D$stigma_experienc2_r1[D$stigma_experienc2==5] <- 0
table(D$stigma_experienc2, D$stigma_experienc2_r1, useNA = 'ifany')

D$stigma_exp_m <- NA
D$stigma_exp_m <- rowMeans(D[,c("stigma_experienc1_r1", "stigma_experienc2_r1")], na.rm=TRUE)

# post traumatic growth

D$ptg_all <- NA
D$ptg_all <- rowMeans(D[,c("ptg1", "ptg2", "ptg3", "ptg4", "ptg5", "ptg6", "ptg7", "ptg8", "ptg9", "ptg10")], na.rm=TRUE)

# additional descriptive variables for appendix descriptives and balance checks                  
table(D$population, useNA='ifany')
table(D$ethnic_group_count, useNA='ifany')
table(D$militia_number, useNA='ifany')
table(D$geography3, useNA='ifany')


## control variables #################################################

D$assets_sum <- (D$ass_phone + D$ass_radio + D$ass_bike + D$ass_moto + D$ass_car)/5

D$exchange_prev <- NA
D$exchange_prev_temp <- NA
D$exchange_prev_temp <- rowMeans(data.frame(D$exchange5_rec, D$exchange10_rec, D$exchange15_rec, D$exchange20_rec), na.rm=TRUE)
D$exchange_prev[D$exchange_prev_temp>=0 &  D$exchange_prev_temp<.5] <- 0
D$exchange_prev[D$exchange_prev_temp>=.5 &  D$exchange_prev_temp<1.5] <- 1
D$exchange_prev[D$exchange_prev_temp>=1.5 &  D$exchange_prev_temp<2.5] <- 2
D$exchange_prev[D$exchange_prev_temp>=2.5 &  D$exchange_prev_temp<3.5] <- 3
D$exchange_prev[D$exchange_prev_temp>=3.5 &  D$exchange_prev_temp<4.5] <- 4
D$exchange_prev[D$exchange_prev_temp>=4.5 &  D$exchange_prev_temp<5.5] <- 5


# "PE social inclusion" for balance table

D$activity_prev #more missingness (44 missing) inf activity_prev 2 of these missing are rape_yes=1
#table(D$activity_prev, useNA='ifany')
#table(D$activity_prev, D$rape_yes, useNA='ifany')

D$t.fizi <- 0
D$t.fizi[D$territoire=="Fizi"] <- 1
D$t.kabare <- 0
D$t.kabare[D$territoire=="Kabare"] <- 1
D$t.kalehe <- 0
D$t.kalehe[D$territoire=="Kalehe"] <- 1
D$t.mwenga <- 0
D$t.mwenga[D$territoire=="Mwenga"] <- 1
D$t.uvira <- 0
D$t.uvira[D$territoire=="Uvira"] <- 1
D$t.walungu <- 0
D$t.walungu[D$territoire=="Walungu"] <- 1


## create normalized outcome variables for comparability######################

mmnorm.out <- function(x){(x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE)-min(x, na.rm=TRUE))}

D$ingroup_visit_mean_norm <- mmnorm.out(D$ingroup_visit_mean)
D$org_member_r1_norm  <- mmnorm.out(D$org_member_r1)
D$event_com_mean_norm  <- mmnorm.out(D$event_com_mean)
D$donate_amount_norm  <- mmnorm.out(D$donate_amount)

D$stigma_anticip_m_norm <- mmnorm.out(D$stigma_anticip_m)
D$stigma_exp_m_norm <- mmnorm.out(D$stigma_exp_m)
D$stigma_internal_r1_norm  <- mmnorm.out(D$stigma_internal_r1)
D$ptg_all_norm  <- mmnorm.out(D$ptg_all)



## CREATE DATASETS WITH NO MISSING VARIABLES##################################
## because the list package requires no missing variables
## multiple datasets created so that no extra data is lost due to differential missingness across outcome variables

D.vis <- data.frame(na.omit(D[,c("ingroup_visit_mean","ingroup_visit_mean_norm","rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "female", "age", "edu_level", "hh_size", "assets_sum", "exchange_prev", "territoire", "stigma_anticip_m", "stigma_exp_m", "stigma_internal_r1", "ptg_all")]))
D.org <- data.frame(na.omit(D[,c("org_leader_d", "rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "female", "age", "edu_level", "hh_size", "assets_sum", "exchange_prev", "territoire", "stigma_anticip_m", "stigma_exp_m", "stigma_internal_r1", "ptg_all")]))
D.mem <- data.frame(na.omit(D[,c("org_member_r1", "org_member_r1_norm","rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "female", "age", "edu_level", "hh_size", "assets_sum", "exchange_prev", "territoire", "stigma_anticip_m", "stigma_exp_m", "stigma_internal_r1", "ptg_all")]))
D.eve <- data.frame(na.omit(D[,c("event_com_mean","event_com_mean_norm", "rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "female", "age", "edu_level", "hh_size", "assets_sum", "exchange_prev", "territoire", "stigma_anticip_m", "stigma_exp_m", "stigma_internal_r1", "ptg_all")]))
D.don <- data.frame(na.omit(D[,c("donate_amount", "donate_amount_norm","rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "female", "age", "edu_level", "hh_size", "assets_sum", "exchange_prev", "territoire", "stigma_anticip_m", "stigma_exp_m", "stigma_internal_r1", "ptg_all")]))

## Create Datasets with no NAs for mechanisms excluding outcome variables and also adding standardized versions of variables

D.sti <- data.frame(na.omit(D[,c("rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "female", "age", "edu_level", "hh_size", "assets_sum", "exchange_prev", "territoire", "stigma_anticip_m", "stigma_exp_m", "stigma_internal_r1", "ptg_all", "stigma_anticip_m_norm", "stigma_exp_m_norm", "stigma_internal_r1_norm", "ptg_all_norm")]))


## dataset for balance table including additional descriptive variables
D.bal <- data.frame(na.omit(D[,c("rape_yes", "treat_list_CRSV", "list1_CRSV", "vio_witness1", "murder_yes", "leavehome_yes", "hhhum_assist_d", "female", "age", "edu_level", "hh_size", "assets_sum", "territoire", "stigma_anticip_m","stigma_internal_r1", "ptg_all", "stigma_anticip_m_norm", "stigma_internal_r1_norm", "ptg_all_norm", "isHHhead",
                                 "occup_farmer", "married", "occup_sal","exchange_prev", "activity_prev", "dist_village", "geography3", "dist_militia", "survey_before", 
                                 "population", "rape_prev2", "rape_prev7", "rape_prev15")]))


## export dataset with new variables for direct loading##################
write.csv(x = D, file = "Data_DRC_CRSVwCodedVars.csv", )
