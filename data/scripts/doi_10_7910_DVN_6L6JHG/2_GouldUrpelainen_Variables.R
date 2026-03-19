########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS CODE (#2) LOADS THE DATA AND DOES SOME VARIABLE PROCESSING



#########################
##### LOAD DATA #########
#########################

RawDataHH = read.table("~/Dataverse_Files/RawDataHH.tab", sep = "\t", header = TRUE)


####################################################
####  DUMMY VARAIBLES FOR REGRESSIONS ##############
####################################################

#create variable "id" with State, District, and Village Codes
RawDataHH$id = paste(RawDataHH$m1_q8_state_code, RawDataHH$m1_q9_district_code, 
                     RawDataHH$m1_q11_village_code, sep="-")

# Make Jharkhand the reference state for late analyses
# Jharkhand is chosen because it has the lowest % LPG
RawDataHH$m1_q8_state <- relevel(RawDataHH$m1_q8_state, ref = "JHARKHAND")

# Make covariates

RawDataHH <- RawDataHH %>%
  mutate(Religion_Hindu = ifelse(RawDataHH$m1_q24_religion == 1, 1, 0),
         Religion_Muslim = ifelse(RawDataHH$m1_q24_religion == 2, 1, 0),
         Caste_ScheduledCaste = ifelse(RawDataHH$m1_q25_caste == 1, 1, 0),
         Caste_ScheduledTribe = ifelse(RawDataHH$m1_q25_caste == 2, 1, 0),
         Caste_OtherBackwardClass = ifelse(RawDataHH$m1_q25_caste == 3, 1, 0),
         Caste_General = ifelse(RawDataHH$m1_q25_caste == 4, 1, 0),
         Edu_ThreeCategories = ifelse(RawDataHH$m1_q23_edu == 1, "No Formal",
                                      ifelse(RawDataHH$m1_q23_edu == 2, "Up to 5th",
                                             ifelse(RawDataHH$m1_q23_edu == 3 | 
                                                      RawDataHH$m1_q23_edu == 4 | 
                                                      RawDataHH$m1_q23_edu == 5, 
                                                    "More than 5th", 0))),
         Edu_NoFormalSchooling = ifelse(RawDataHH$m1_q23_edu == 1, 1, 0),
         Edu_UpTo5thStandard = ifelse(RawDataHH$m1_q23_edu == 2, 1, 0),
         Edu_MoreThan5thStandard = ifelse(RawDataHH$m1_q23_edu == 3 | 
                                            RawDataHH$m1_q23_edu == 4 | 
                                            RawDataHH$m1_q23_edu == 5, 1, 0),
         Decision_MaleHouseholdHead = ifelse(RawDataHH$m1_q38_decision_maker==1, 1,0),
         Decision_FemaleHouseholdHead = ifelse(RawDataHH$m1_q38_decision_maker==2, 1,0),
         Decision_Both = ifelse(RawDataHH$m1_q38_decision_maker==3, 1, 0),
         Any_Child = ifelse(RawDataHH$m1_q29_no_children>0, 1,0),
         HHSize = RawDataHH$m1_q29_no_children + RawDataHH$m1_q27_no_adults,
         FemaleHH = ifelse((RawDataHH$m1_q17_relationship==1 & RawDataHH$m1_q20_gender==0) | 
                             (RawDataHH$m1_q17_relationship==2 & RawDataHH$m1_q20_gender==1), 1, 0),
         m1_q32_month_expenditure_log = log(RawDataHH$m1_q32_month_expenditure))


RawDataHH <- RawDataHH %>%
  mutate(FemaleHH_DecisionMaker = ifelse(RawDataHH$FemaleHH == 1 & RawDataHH$Decision_FemaleHouseholdHead==1, 1, 0),
         Income_Agriculture = ifelse(RawDataHH$m1_q31_income_source==1, 1, 0))


# dissatisfaction dummy variables
RawDataHH <- RawDataHH %>%
  mutate(LPG_Unsatisfied = ifelse(RawDataHH$m4_q104_lpg_satisfy == 1, 1,0),
         LPG_Unsatisfied_Expense = ifelse(RawDataHH$m4_q104_1_lpg_unsatisfy_expense, 1,0),
         LPG_Unsatisfied_Avail = ifelse(RawDataHH$m4_q104_2_lpg_unsatisfy_avail, 1, 0),
         LPG_Unsatisfied_Far = ifelse(RawDataHH$m4_q104_3_lpg_unsatisfy_far, 1, 0),
         LPG_Unsatisfied_Maintain = ifelse(RawDataHH$m4_q104_4_lpg_unsatisfy_maintain, 1,0))

#satisfied with LPG
RawDataHH <- RawDataHH %>%
  mutate(LPG_Satisfied = ifelse(RawDataHH$m4_q104_lpg_satisfy == 3, 1, 0),
         No_LPG = ifelse(RawDataHH$m4_q103_lpg == 0, 1, 0),
         Has_LPG = ifelse(RawDataHH$m4_q103_lpg == 1, 1, 0)) 

# lpg use amounts
RawDataHH <- RawDataHH %>%
  mutate(Large_kg_lpg = RawDataHH$m4_q103_4_lpg_lcyl*14.2,
         Small_kg_lpg = RawDataHH$m4_q103_7_lpg_scyl*5)

RawDataHH <- RawDataHH %>%
  mutate(kg_lpg = RawDataHH$Large_kg_lpg + RawDataHH$Small_kg_lpg)

RawDataHH$lpg_morethan_14kg = ifelse(RawDataHH$kg_lpg > 168, 1, 0)
RawDataHH$kg_per_capita = (RawDataHH$kg_lpg / 
                             (RawDataHH$m1_q27_no_adults + RawDataHH$m1_q29_no_children)) / 12
RawDataHH$kg_firewood_per_capita = RawDataHH$m4_q109_1_firewood_quant / 
  (RawDataHH$m1_q27_no_adults + RawDataHH$m1_q29_no_children)

#items cooked with lpg
RawDataHH = RawDataHH %>%
  mutate(LPG_cooked = paste(RawDataHH$m4_q103_3_1_lpg_chapattis, RawDataHH$m4_q103_3_2_lpg_veg, 
                            RawDataHH$m4_q103_3_3_lpg_rice, RawDataHH$m4_q103_3_4_lpg_tea_snack, 
                            RawDataHH$m4_q103_3_5_lpg_milk, RawDataHH$m4_q103_3_6_lpg_water, sep = ""))

#fuel costs
RawDataHH$LPG_LCYLC_PRICE_100 = RawDataHH$m4_q103_10_lpg_lcyl_dist_price * 100
RawDataHH$LPG_LCYLC_PRICE_100a = RawDataHH$m4_q103_10_lpg_lcyl_dist_price / 100
RawDataHH$LPG_distance_km = ifelse(RawDataHH$m4_q103_14_lpg_cyl_deliver==1, 0,
                                   ifelse(RawDataHH$m4_q103_14_lpg_cyl_deliver==0, RawDataHH$m4_q103_15_lpg_distance, NA))

RawDataHH$Firewood_Collect_Daily = ifelse(RawDataHH$m4_q110_firewood_freq == 1, 1, 0)
RawDataHH$Firewood_Collect_FewWeek = ifelse(RawDataHH$m4_q110_firewood_freq == 2, 1, 0)
RawDataHH$Firewood_Collect_FewMonth = ifelse(RawDataHH$m4_q110_firewood_freq == 3, 1, 0)
RawDataHH$Firewood_Collect_FewYear = ifelse(RawDataHH$m4_q110_firewood_freq == 4, 1, 0)
RawDataHH$Firewood_Collect_NA = ifelse(RawDataHH$m4_q110_firewood_freq == 5 | is.na(RawDataHH$m4_q110_firewood_freq), 1, 0)
RawDataHH$Firewood_MostlyCollect = ifelse((RawDataHH$m4_q109_2_firewood_collect > RawDataHH$m4_q109_3_firewood_mkt) == TRUE, 1, 0)
RawDataHH$Firewood_Collect_Distance = RawDataHH$m4_q111_1_firewood_collect_dist
RawDataHH$Firewood_Market_Distance = RawDataHH$m4_q111_2_firewood_mkt_dist
RawDataHH$LPG_Distance = RawDataHH$m4_q103_15_lpg_distance 





##################################################
########### CREATING SURVEY WEIGHTS ##############
##################################################

# this is the subset we will use. drops 3 observations
RawDataHH <- subset(RawDataHH, !is.na(RawDataHH$Has_LPG)) 

RawDataHH = RawDataHH[is.na(RawDataHH$weight) %in% F,]
sdesign = svydesign(id = ~m1_q3_hhid,
                    strata = ~m1_q11_village_code,
                    data = RawDataHH,
                    weights = ~weight,
                    nest = T)

# subset w/ LPG
RawDataHH_lpg = RawDataHH[(RawDataHH$m4_q103_lpg) == 1,]

# subset w/ survey weights w/ LPG
RawDataHH_lpg = RawDataHH_lpg[is.na(RawDataHH_lpg$weight) %in% F,]
sdesign_lpg = svydesign(id = ~m1_q3_hhid,
                        strata = ~m1_q11_village_code,
                        data = RawDataHH,
                        weights = ~weight,
                        nest = T)

# confirm they have LPG
sdesign_LPG <- subset(sdesign, sdesign$variables$m4_q103_lpg==1) 
# opposite: non LPG owners
sdesign_noLPG <- subset(sdesign, sdesign$variables$m4_q103_lpg==0)
# additional subset, these observations have LPG and firewood
sdesign_LPG_firewod <- subset(sdesign_LPG, sdesign_LPG$variables$m4_q109_firewood==1)
# firewood owners
sdesign_firewood <- subset(sdesign, sdesign$variables$m4_q109_firewood==1)



# useful for descriptive figures (Fig A3)
RawDataHH_firewood <- subset(RawDataHH, RawDataHH$kg_firewood_per_capita>0 & !(is.na(RawDataHH$Has_LPG)))

RawDataHH_firewood$Has_LPG <- factor(RawDataHH_firewood$Has_LPG, labels = c("No LPG", "Has LPG"))
RawDataHH_firewood_LPG <- subset(RawDataHH_firewood, RawDataHH_firewood$Has_LPG=="Has LPG")
RawDataHH_firewood_NoLPG <- subset(RawDataHH_firewood, RawDataHH_firewood$Has_LPG=="No LPG")


# FOR STATE-BY-STATE REGRESSIONS

sdesign_Jharkhand <- subset(sdesign, sdesign$variables$m1_q8_state=="JHARKHAND")
sdesign_Madhya <- subset(sdesign, sdesign$variables$m1_q8_state=="MADHYA PRADESH")
sdesign_Odisha <- subset(sdesign, sdesign$variables$m1_q8_state=="ODISHA")
sdesign_Uttar <- subset(sdesign, sdesign$variables$m1_q8_state=="UTTAR PRADESH")
sdesign_WBengal <- subset(sdesign, sdesign$variables$m1_q8_state=="WEST BENGAL")
sdesign_Bihar <- subset(sdesign, sdesign$variables$m1_q8_state=="BIHAR")


# FOR ADDITIONAL REGRESSIONS THAT NEED CERTAIN VARIABLES

sdesign_LPG_lcylc <- subset(sdesign_LPG, !is.na(sdesign_LPG$variables$LPG_LCYLC_PRICE_100))
sdesign_LPG_lpgdistnace <- subset(sdesign_LPG, !is.na(sdesign_LPG$variables$LPG_distance_km))
sdesign_LPG_lcylc_distance <- subset(sdesign_LPG, !is.na(sdesign_LPG$variables$LPG_LCYLC_PRICE_100) &
                                       !is.na(sdesign_LPG$variables$LPG_distance_km))


sdesign_lcylc <- subset(sdesign, !is.na(sdesign$variables$LPG_LCYLC_PRICE_100 & sdesign$variables$m5_q118_main_cookfuel))
sdesign_lpgdistnace <- subset(sdesign, !is.na(sdesign$variables$LPG_distance_km))
sdesign_lcylc_distance <- subset(sdesign, !is.na(sdesign$variables$LPG_LCYLC_PRICE_100) &
                                       !is.na(sdesign$variables$LPG_distance_km))

# now need these subsets to have firewood
sdesign_LPG_firewod_lcyl <- subset(sdesign_LPG_firewod, !is.na(sdesign_LPG_firewod$variables$LPG_LCYLC_PRICE_100a))
sdesign_LPG_firewod_ldistance <- subset(sdesign_LPG_firewod, !is.na(sdesign_LPG_firewod$variables$LPG_distance_km))
sdesign_LPG_firewod_fdistance <- subset(sdesign_LPG_firewod, !is.na(sdesign_LPG_firewod$variables$Firewood_Collect_Distance))
sdesign_LPG_firewod_lcyl_distance <- subset(sdesign_LPG_firewod, !is.na(sdesign_LPG_firewod$variables$LPG_LCYLC_PRICE_100a &
                                                                          !is.na(sdesign_LPG_firewod$variables$LPG_distance_km)))


