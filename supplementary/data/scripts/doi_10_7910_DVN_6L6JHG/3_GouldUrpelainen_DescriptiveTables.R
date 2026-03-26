########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS IS THE CODE (#3) THAT MAKES DESCRIPTIVE TABLES



#########################
#####      Table 1   ####
#########################


covariates_all <- c("Log Monthly Expenditure", "Number of Adults", "Number of Children", 
                    "Age (year)", 
                    "Hindu", 
                    "Scheduled Caste", "Scheduled Tribe", "Other Backward Caste", 
                    "Up to 5th Standard", "More than 5th Standard", 
                    "Decision Maker: Female Household Head", "Decision Maker: Both")

##### Summary statistics #####
vars = c("m4_q103_lpg", 
         "Decision_FemaleHouseholdHead", "Decision_MaleHouseholdHead", "Decision_Both",
         "m1_q32_month_expenditure_log","m1_q27_no_adults", "m1_q29_no_children", 
         "m1_q19_age", 
         "Religion_Hindu", "Religion_Muslim", 
         "Edu_NoFormalSchooling", "Edu_UpTo5thStandard", "Edu_MoreThan5thStandard",
         "Caste_ScheduledCaste", "Caste_ScheduledTribe", "Caste_OtherBackwardClass", "Caste_General")

sw.sum = data.frame(matrix(NA, 17, 4))

RawDataHH <- subset(RawDataHH, !is.na(RawDataHH$Has_LPG))

sw.sum[1,1] = paste0(round(w.mean(RawDataHH$m4_q103_lpg, RawDataHH$weight), 2)*100, "%")
sw.sum[2,1] = paste0(round(w.mean(RawDataHH$Decision_FemaleHouseholdHead, RawDataHH[,395]), 2)*100, "%")
sw.sum[3,1] = paste0(round(w.mean(RawDataHH$Decision_MaleHouseholdHead, RawDataHH[,395]), 2)*100, "%")
sw.sum[4,1] = paste0(round(w.mean(RawDataHH$Decision_Both, RawDataHH[,395]), 2)*100, "%")
sw.sum[5,1] = round(w.mean(RawDataHH$m1_q32_month_expenditure_log, RawDataHH[,395]), 2)
sw.sum[6,1] = round(w.mean(RawDataHH$m1_q27_no_adults, RawDataHH[,395]), 2)
sw.sum[7,1] = round(w.mean(RawDataHH$m1_q29_no_children, RawDataHH[,395]), 2)
sw.sum[8,1] = round(w.mean(RawDataHH$m1_q19_age, RawDataHH[,395]), 2)
sw.sum[9,1] = paste0(round(w.mean(RawDataHH$Religion_Hindu, RawDataHH[,395]), 2)*100, "%")
sw.sum[10,1] = paste0(round(w.mean(RawDataHH$Religion_Muslim, RawDataHH[,395]), 2)*100, "%")
sw.sum[11,1] = paste0(round(w.mean(RawDataHH$Edu_NoFormalSchooling, RawDataHH[,395]), 2)*100, "%")
sw.sum[12,1] = paste0(round(w.mean(RawDataHH$Edu_UpTo5thStandard, RawDataHH[,395]), 2)*100, "%")
sw.sum[13,1] = paste0(round(w.mean(RawDataHH$Edu_MoreThan5thStandard, RawDataHH[,395]), 2)*100, "%")
sw.sum[14,1] = paste0(round(w.mean(RawDataHH$Caste_ScheduledCaste, RawDataHH[,395]), 2)*100, "%")
sw.sum[15,1] = paste0(round(w.mean(RawDataHH$Caste_ScheduledTribe, RawDataHH[,395]), 2)*100, "%")
sw.sum[16,1] = paste0(round(w.mean(RawDataHH$Caste_OtherBackwardClass, RawDataHH[,395]), 2)*100, "%")
sw.sum[17,1] = paste0(round(w.mean(RawDataHH$Caste_General, RawDataHH[,395]), 2)*100, "%")

sw.sum[5,2] = round(w.sd(RawDataHH$m1_q32_month_expenditure_log, RawDataHH[,395]), 2)
sw.sum[6,2] = round(w.sd(RawDataHH$m1_q27_no_adults, RawDataHH[,395]), 2)
sw.sum[7,2] = round(w.sd(RawDataHH$m1_q29_no_children, RawDataHH[,395]), 2)
sw.sum[8,2] = round(w.sd(RawDataHH$m1_q19_age, RawDataHH[,395]), 2)

sw.sum[5,3] = round(min(RawDataHH$m1_q32_month_expenditure_log, 2))
sw.sum[6,3] = round(min(RawDataHH$m1_q27_no_adults, 2))
sw.sum[7,3] = round(min(RawDataHH$m1_q29_no_children, 2))
sw.sum[8,3] = min(RawDataHH$m1_q19_age)

sw.sum[5,4] = round(max(RawDataHH$m1_q32_month_expenditure_log, 2))
sw.sum[6,4] = round(max(RawDataHH$m1_q27_no_adults, 2))
sw.sum[7,4] = round(max(RawDataHH$m1_q29_no_children, 2))
sw.sum[8,4] = max(RawDataHH$m1_q19_age, 2)

colnames(sw.sum) = c("Mean", "SD", "Min", "Max")
rownames(sw.sum) = c("LPG adoption (=1)",
                     "Decision Maker: Female Household Head", "Decision Maker: Male Household Head", "Decision Maker: Both",
                     "Monthly expenditure (log)",
                     "Number of adults",
                     "Number of children",
                     "Age (years)",
                     "Religion: Hindu", "Religion: Muslim",
                     "Education: No Formal Schooling", "Education: Up To 5th Standard", "Education: More Than 5th Standard",
                     "Caste: Scheduled Caste", "Caste: Scheduled Tribe", "Caste: Other Backward Class", "Caste: General")

sw.sum

# output to latex
# stargazer(sw.sum, summary=FALSE, float=FALSE, digits = 2,
#            out = "~/Dropbox/Fuels (Carlos, Johannes)/Submission to Journal of Development Studies/Revisions/Tables/sumstats.tex")

# output to html for quick viewing
# stargazer(sw.sum, summary=FALSE, float=FALSE, digits = 2,
#           type = "html", out = "/Users/Carlos/Desktop/sumstats.html")


#########################
#####     Table 2   ####
#########################

# WILL USE THESE VARIABLES
vars.decisions = c("m4_q103_lpg", 
                   "m1_q32_month_expenditure_log","m1_q27_no_adults", "m1_q29_no_children", 
                   "m1_q19_age", 
                   "Religion_Hindu", "Religion_Muslim", 
                   "Edu_NoFormalSchooling", "Edu_UpTo5thStandard", "Edu_MoreThan5thStandard",
                   "Caste_ScheduledCaste", "Caste_ScheduledTribe", "Caste_OtherBackwardClass", "Caste_General")

# CREATE THE DATAFRAME 
sw.sum.decision = data.frame(matrix(NA, 14, 3))
rownames(sw.sum.decision) = vars.decisions # APPLY VARIABLE NAMES

# MAKE RELEVANT SUBSETS
female_decision <- subset(RawDataHH, RawDataHH$Decision_FemaleHouseholdHead == 1) #486
male_decision <- subset(RawDataHH, RawDataHH$Decision_MaleHouseholdHead == 1) #6678
both_decision <- subset(RawDataHH, RawDataHH$Decision_Both == 1) #1251

# USE SURVEY WEIGHTS WITHIN EACH SUBSET TO GENERATE DESCRIPTIVE STATISTICS 
sw.sum.decision[1,1] = paste0(round(w.mean(female_decision$m4_q103_lpg, female_decision[,395]), 2)*100, "%")
sw.sum.decision[2,1] = round(w.mean(female_decision$m1_q32_month_expenditure_log, female_decision[,395]), 2)
sw.sum.decision[3,1] = round(w.mean(female_decision$m1_q27_no_adults, female_decision[,395]), 2)
sw.sum.decision[4,1] = round(w.mean(female_decision$m1_q29_no_children, female_decision[,395]), 2)
sw.sum.decision[5,1] = round(w.mean(female_decision$m1_q19_age, female_decision[,395]), 2)
sw.sum.decision[6,1] = paste0(round(w.mean(female_decision$Religion_Hindu, female_decision[,395]), 2)*100, "%")
sw.sum.decision[7,1] = paste0(round(w.mean(female_decision$Religion_Muslim, female_decision[,395]), 2)*100, "%")
sw.sum.decision[8,1] = paste0(round(w.mean(female_decision$Edu_NoFormalSchooling, female_decision[,395]), 2)*100, "%")
sw.sum.decision[9,1] = paste0(round(w.mean(female_decision$Edu_UpTo5thStandard, female_decision[,395]), 2)*100, "%")
sw.sum.decision[10,1] = paste0(round(w.mean(female_decision$Edu_MoreThan5thStandard, female_decision[,395]), 2)*100, "%")
sw.sum.decision[11,1] = paste0(round(w.mean(female_decision$Caste_ScheduledCaste, female_decision[,395]), 2)*100, "%")
sw.sum.decision[12,1] = paste0(round(w.mean(female_decision$Caste_ScheduledTribe, female_decision[,395]), 2)*100, "%")
sw.sum.decision[13,1] = paste0(round(w.mean(female_decision$Caste_OtherBackwardClass, female_decision[,395]), 2)*100, "%")
sw.sum.decision[14,1] = paste0(round(w.mean(female_decision$Caste_General, female_decision[,395]), 2)*100, "%")

sw.sum.decision[1,2] = paste0(round(w.mean(male_decision$m4_q103_lpg, male_decision[,395]), 2)*100, "%")
sw.sum.decision[2,2] = round(w.mean(male_decision$m1_q32_month_expenditure_log, male_decision[,395]), 2)
sw.sum.decision[3,2] = round(w.mean(male_decision$m1_q27_no_adults, male_decision[,395]), 2)
sw.sum.decision[4,2] = round(w.mean(male_decision$m1_q29_no_children, male_decision[,395]), 2)
sw.sum.decision[5,2] = round(w.mean(male_decision$m1_q19_age, male_decision[,395]), 2)
sw.sum.decision[6,2] = paste0(round(w.mean(male_decision$Religion_Hindu, male_decision[,395]), 2)*100, "%")
sw.sum.decision[7,2] = paste0(round(w.mean(male_decision$Religion_Muslim, male_decision[,395]), 2)*100, "%")
sw.sum.decision[8,2] = paste0(round(w.mean(male_decision$Edu_NoFormalSchooling, male_decision[,395]), 2)*100, "%")
sw.sum.decision[9,2] = paste0(round(w.mean(male_decision$Edu_UpTo5thStandard, male_decision[,395]), 2)*100, "%")
sw.sum.decision[10,2] = paste0(round(w.mean(male_decision$Edu_MoreThan5thStandard, male_decision[,395]), 2)*100, "%")
sw.sum.decision[11,2] = paste0(round(w.mean(male_decision$Caste_ScheduledCaste, male_decision[,395]), 2)*100, "%")
sw.sum.decision[12,2] = paste0(round(w.mean(male_decision$Caste_ScheduledTribe, male_decision[,395]), 2)*100, "%")
sw.sum.decision[13,2] = paste0(round(w.mean(male_decision$Caste_OtherBackwardClass, male_decision[,395]), 2)*100, "%")
sw.sum.decision[14,2] = paste0(round(w.mean(male_decision$Caste_General, male_decision[,395]), 2)*100, "%")

sw.sum.decision[1,3] = paste0(round(w.mean(both_decision$m4_q103_lpg, both_decision[,395]), 2)*100, "%")
sw.sum.decision[2,3] = round(w.mean(both_decision$m1_q32_month_expenditure_log, both_decision[,395]), 2)
sw.sum.decision[3,3] = round(w.mean(both_decision$m1_q27_no_adults, both_decision[,395]), 2)
sw.sum.decision[4,3] = round(w.mean(both_decision$m1_q29_no_children, both_decision[,395]), 2)
sw.sum.decision[5,3] = round(w.mean(both_decision$m1_q19_age, both_decision[,395]), 2)
sw.sum.decision[6,3] = paste0(round(w.mean(both_decision$Religion_Hindu, both_decision[,395]), 2)*100, "%")
sw.sum.decision[7,3] = paste0(round(w.mean(both_decision$Religion_Muslim, both_decision[,395]), 2)*100, "%")
sw.sum.decision[8,3] = paste0(round(w.mean(both_decision$Edu_NoFormalSchooling, both_decision[,395]), 2)*100, "%")
sw.sum.decision[9,3] = paste0(round(w.mean(both_decision$Edu_UpTo5thStandard, both_decision[,395]), 2)*100, "%")
sw.sum.decision[10,3] = paste0(round(w.mean(both_decision$Edu_MoreThan5thStandard, both_decision[,395]), 2)*100, "%")
sw.sum.decision[11,3] = paste0(round(w.mean(both_decision$Caste_ScheduledCaste, both_decision[,395]), 2)*100, "%")
sw.sum.decision[12,3] = paste0(round(w.mean(both_decision$Caste_ScheduledTribe, both_decision[,395]), 2)*100, "%")
sw.sum.decision[13,3] = paste0(round(w.mean(both_decision$Caste_OtherBackwardClass, both_decision[,395]), 2)*100, "%")
sw.sum.decision[14,3] = paste0(round(w.mean(both_decision$Caste_General, both_decision[,395]), 2)*100, "%")

# name rows and columns appropriately
rownames(sw.sum.decision) = c("LPG adoption (=1)",
                              "Monthly expenditure (log)",
                              "Number of adults",
                              "Number of children",
                              "Age (years)",
                              "Religion: Hindu", "Religion: Muslim", 
                              "Education: No Formal Schooling", "Education: Up To 5th Standard", "Education: More Than 5th Standard",
                              "Caste: Scheduled Caste", "Caste: Scheduled Tribe", "Caste: Other Backward Class", "Caste: General")
colnames(sw.sum.decision) = c("Female Household Head (n=486)", "Male Household Head (n=6678)", "Both (n=1251)")

sw.sum.decision # PREVIEW THE TABLE

# output the table to latex
# stargazer(sw.sum.decision, summary=FALSE, float=FALSE, digits = 2,
#           out = "~/Dropbox/Fuels (Carlos, Johannes)/Submission to Journal of Development Studies/Revisions/Tables/decisionsumstats.tex")

# output the table to html for quick viewing
# stargazer(sw.sum.decision, summary=FALSE, float=FALSE, digits=2,
#           type = "html", out = "/Users/Carlos/Desktop/decisionsumstats.html")

#########################
#####     Table 4   ####
#########################

sw.sum.nonadopters = data.frame(matrix(NA, 4, 4))

nolpg <- subset(RawDataHH, RawDataHH$No_LPG == 1) #6712
female_nonadopters <- subset(nolpg, nolpg$Decision_FemaleHouseholdHead == 1) #356
male_nonadopters <- subset(nolpg, nolpg$Decision_MaleHouseholdHead == 1) #5281
both_nonadopters <- subset(nolpg, nolpg$Decision_Both == 1) #989

sw.sum.nonadopters[1,1] = paste0(round(w.mean(female_nonadopters$m4_q105_1_nlpg_unavailable, female_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[2,1] = paste0(round(w.mean(female_nonadopters$m4_q105_2_nlpg_connect_expensive, female_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[3,1] = paste0(round(w.mean(female_nonadopters$m4_q105_3_nlpg_monthly_expensive, female_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[4,1] = paste0(round(w.mean(female_nonadopters$m4_q105_4_nlpg_dk, female_nonadopters[,395]), 2)*100, "%")

sw.sum.nonadopters[1,2] = paste0(round(w.mean(male_nonadopters$m4_q105_1_nlpg_unavailable, male_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[2,2] = paste0(round(w.mean(male_nonadopters$m4_q105_2_nlpg_connect_expensive, male_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[3,2] = paste0(round(w.mean(male_nonadopters$m4_q105_3_nlpg_monthly_expensive, male_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[4,2] = paste0(round(w.mean(male_nonadopters$m4_q105_4_nlpg_dk, male_nonadopters[,395]), 2)*100, "%")

sw.sum.nonadopters[1,3] = paste0(round(w.mean(both_nonadopters$m4_q105_1_nlpg_unavailable, both_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[2,3] = paste0(round(w.mean(both_nonadopters$m4_q105_2_nlpg_connect_expensive, both_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[3,3] = paste0(round(w.mean(both_nonadopters$m4_q105_3_nlpg_monthly_expensive, both_nonadopters[,395]), 2)*100, "%")
sw.sum.nonadopters[4,3] = paste0(round(w.mean(both_nonadopters$m4_q105_4_nlpg_dk, both_nonadopters[,395]), 2)*100, "%")

sw.sum.nonadopters[1,4] = paste0(round(w.mean(nolpg$m4_q105_1_nlpg_unavailable, nolpg[,395]), 2)*100, "%")
sw.sum.nonadopters[2,4] = paste0(round(w.mean(nolpg$m4_q105_2_nlpg_connect_expensive, nolpg[,395]), 2)*100, "%")
sw.sum.nonadopters[3,4] = paste0(round(w.mean(nolpg$m4_q105_3_nlpg_monthly_expensive, nolpg[,395]), 2)*100, "%")
sw.sum.nonadopters[4,4] = paste0(round(w.mean(nolpg$m4_q105_4_nlpg_dk, nolpg[,395]), 2)*100, "%")

rownames(sw.sum.nonadopters) = c("Unavailable", "Installation Cost", "Monthly Cost", "Lack of Information")

colnames(sw.sum.nonadopters) = c("Female Household Head (n=356)", "Male Household Head (n=5281)", "Both (n=989)", "All Non-Adopters (n=6712)")

sw.sum.nonadopters

# output to latex
# stargazer(sw.sum.nonadopters, summary=FALSE, float=FALSE, digits = 2,
#           out = "~/Dropbox/Fuels (Carlos, Johannes)/Submission to Journal of Development Studies/Revisions/Tables/nonadoptionsumstats.tex")

# output to html for quick viewing
# stargazer(sw.sum.nonadopters, summary=FALSE, float=FALSE, digits=2,
#           type = "html", out = "/Users/Carlos/Desktop/nonadoptionsumstats.html")


#########################
#####     Table 5   ####
#########################

female <- subset(RawDataHH, RawDataHH$m1_q20_gender == 0)
male <- subset(RawDataHH, RawDataHH$m1_q20_gender == 1)
female_lpg <- subset(female, female$m4_q103_lpg == 1)
female_no_lpg <- subset(female, female$m4_q103_lpg == 0)
male_lpg <- subset(male, male$m4_q103_lpg == 1)
male_no_lpg <- subset(male, male$m4_q103_lpg == 0)

female_lpg_primary <- subset(female, female$m5_q118_main_cookfuel==3)
female_nonlpg_primary <- subset(female, !(female$m5_q118_main_cookfuel==3))

male_lpg_primary <- subset(male, male$m5_q118_main_cookfuel==3)
male_nonlpg_primary <- subset(male, !(male$m5_q118_main_cookfuel==3))

gender_lpg_cookingsatisfaction <- as.data.frame(matrix(data=NA, nrow=8, ncol=2))
colnames(gender_lpg_cookingsatisfaction) <- c("Female Respondent", "Male Respondent")
row.names(gender_lpg_cookingsatisfaction) <- c("LPG User", "Satisfied ", "Neutral ", "Disatisfied ", "Non-LPG User", "Satisfied", "Neutral", "Disatisfied")

gender_lpg_cookingsatisfaction[1,1] <- paste(round(mean(female_lpg$m5_q122_cook_satisfy), 2), " (",  round(sd(female_lpg$m5_q122_cook_satisfy), 2), ")", sep = "")
gender_lpg_cookingsatisfaction[2,1] <- paste0(mean(female_lpg$m5_q122_cook_satisfy==3, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[3,1] <- paste0(mean(female_lpg$m5_q122_cook_satisfy==2, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[4,1] <- paste0(mean(female_lpg$m5_q122_cook_satisfy==1, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[5,1] <- paste(round(mean(female_no_lpg$m5_q122_cook_satisfy), 2), " (",  round(sd(female_no_lpg$m5_q122_cook_satisfy), 2), ")", sep = "")
gender_lpg_cookingsatisfaction[6,1] <- paste0(mean(female_no_lpg$m5_q122_cook_satisfy==3, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[7,1] <- paste0(mean(female_no_lpg$m5_q122_cook_satisfy==2, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[8,1] <- paste0(mean(female_no_lpg$m5_q122_cook_satisfy==1, na.rm=T)*100, "%")

gender_lpg_cookingsatisfaction[1,2] <- paste(round(mean(male_lpg$m5_q122_cook_satisfy), 2), " (",  round(sd(male_lpg$m5_q122_cook_satisfy), 2), ")", sep = "")
gender_lpg_cookingsatisfaction[2,2] <- paste0(mean(male_lpg$m5_q122_cook_satisfy==3, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[3,2] <- paste0(mean(male_lpg$m5_q122_cook_satisfy==2, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[4,2] <- paste0(mean(male_lpg$m5_q122_cook_satisfy==1, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[5,2] <- paste(round(mean(male_no_lpg$m5_q122_cook_satisfy), 2), " (",  round(sd(male_no_lpg$m5_q122_cook_satisfy), 2), ")", sep = "")
gender_lpg_cookingsatisfaction[6,2] <- paste0(mean(male_no_lpg$m5_q122_cook_satisfy==3, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[7,2] <- paste0(mean(male_no_lpg$m5_q122_cook_satisfy==2, na.rm=T)*100, "%")
gender_lpg_cookingsatisfaction[8,2] <- paste0(mean(male_no_lpg$m5_q122_cook_satisfy==1, na.rm=T)*100, "%")


#print cooking satisfaction matrix
gender_lpg_cookingsatisfaction$"P-Value" <- NA
gender_lpg_cookingsatisfaction[1,3] <- 0.001
gender_lpg_cookingsatisfaction[5,3] <- 0.011

# stargazer(gender_lpg_cookingsatisfaction, summary=FALSE, float=FALSE, digits=2,
#           out = "~/Dropbox/Fuels (Carlos, Johannes)/Submission to Journal of Development Studies/Revisions/Tables/genderlpgcooksatisfaction.tex")










#########################
#####     Table A3   ####
#########################


sdesign_Jharkhand <- subset(sdesign, sdesign$variables$m1_q8_state=="JHARKHAND")
sdesign_Madhya <- subset(sdesign, sdesign$variables$m1_q8_state=="MADHYA PRADESH")
sdesign_Odisha <- subset(sdesign, sdesign$variables$m1_q8_state=="ODISHA")
sdesign_Uttar <- subset(sdesign, sdesign$variables$m1_q8_state=="UTTAR PRADESH")
sdesign_WBengal <- subset(sdesign, sdesign$variables$m1_q8_state=="WEST BENGAL")
sdesign_Bihar <- subset(sdesign, sdesign$variables$m1_q8_state=="BIHAR")

Jharkhand <- subset(RawDataHH, RawDataHH$m1_q8_state=="JHARKHAND")
Madhya <- subset(RawDataHH, RawDataHH$m1_q8_state=="MADHYA PRADESH")
Odisha <- subset(RawDataHH, RawDataHH$m1_q8_state=="ODISHA")
Uttar <- subset(RawDataHH, RawDataHH$m1_q8_state=="UTTAR PRADESH")
WBengal <- subset(RawDataHH, RawDataHH$m1_q8_state=="WEST BENGAL")
Bihar <- subset(RawDataHH, RawDataHH$m1_q8_state=="BIHAR")

# WILL USE THESE VARIABLES
vars.decisions = c("m4_q103_lpg", 
                   "Decision_FemaleHouseholdHead", "Decision_MaleHouseholdHead", "Decision_Both",
                   "m1_q32_month_expenditure_log","m1_q27_no_adults", "m1_q29_no_children", 
                   "m1_q19_age", 
                   "Religion_Hindu", "Religion_Muslim", 
                   "Edu_NoFormalSchooling", "Edu_UpTo5thStandard", "Edu_MoreThan5thStandard",
                   "Caste_ScheduledCaste", "Caste_ScheduledTribe", "Caste_OtherBackwardClass", "Caste_General")

# CREATE THE DATAFRAME 
sw.sum.state = data.frame(matrix(NA, 17, 6))
rownames(sw.sum.state) = vars.decisions # APPLY VARIABLE NAMES

# USE SURVEY WEIGHTS WITHIN EACH SUBSET TO GENERATE DESCRIPTIVE STATISTICS 
sw.sum.state[1,1] = paste0(round(w.mean(Jharkhand$m4_q103_lpg, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[2,1] = paste0(round(w.mean(Jharkhand$Decision_FemaleHouseholdHead, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[3,1] = paste0(round(w.mean(Jharkhand$Decision_MaleHouseholdHead, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[4,1] = paste0(round(w.mean(Jharkhand$Decision_Both, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[5,1] = round(w.mean(Jharkhand$m1_q32_month_expenditure_log, Jharkhand[,395]), 2)
sw.sum.state[6,1] = round(w.mean(Jharkhand$m1_q27_no_adults, Jharkhand[,395]), 2)
sw.sum.state[7,1] = round(w.mean(Jharkhand$m1_q29_no_children, Jharkhand[,395]), 2)
sw.sum.state[8,1] = round(w.mean(Jharkhand$m1_q19_age, Jharkhand[,395]), 2)
sw.sum.state[9,1] = paste0(round(w.mean(Jharkhand$Religion_Hindu, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[10,1] = paste0(round(w.mean(Jharkhand$Religion_Muslim, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[11,1] = paste0(round(w.mean(Jharkhand$Edu_NoFormalSchooling, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[12,1] = paste0(round(w.mean(Jharkhand$Edu_UpTo5thStandard, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[13,1] = paste0(round(w.mean(Jharkhand$Edu_MoreThan5thStandard, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[14,1] = paste0(round(w.mean(Jharkhand$Caste_ScheduledCaste, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[15,1] = paste0(round(w.mean(Jharkhand$Caste_ScheduledTribe, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[16,1] = paste0(round(w.mean(Jharkhand$Caste_OtherBackwardClass, Jharkhand[,395]), 2)*100, "%")
sw.sum.state[17,1] = paste0(round(w.mean(Jharkhand$Caste_General, Jharkhand[,395]), 2)*100, "%")

sw.sum.state[1,2] = paste0(round(w.mean(Madhya$m4_q103_lpg, Madhya[,395]), 2)*100, "%")
sw.sum.state[2,2] = paste0(round(w.mean(Madhya$Decision_FemaleHouseholdHead, Madhya[,395]), 2)*100, "%")
sw.sum.state[3,2] = paste0(round(w.mean(Madhya$Decision_MaleHouseholdHead, Madhya[,395]), 2)*100, "%")
sw.sum.state[4,2] = paste0(round(w.mean(Madhya$Decision_Both, Madhya[,395]), 2)*100, "%")
sw.sum.state[5,2] = round(w.mean(Madhya$m1_q32_month_expenditure_log, Madhya[,395]), 2)
sw.sum.state[6,2] = round(w.mean(Madhya$m1_q27_no_adults, Madhya[,395]), 2)
sw.sum.state[7,2] = round(w.mean(Madhya$m1_q29_no_children, Madhya[,395]), 2)
sw.sum.state[8,2] = round(w.mean(Madhya$m1_q19_age, Madhya[,395]), 2)
sw.sum.state[9,2] = paste0(round(w.mean(Madhya$Religion_Hindu, Madhya[,395]), 2)*100, "%")
sw.sum.state[10,2] = paste0(round(w.mean(Madhya$Religion_Muslim, Madhya[,395]), 2)*100, "%")
sw.sum.state[11,2] = paste0(round(w.mean(Madhya$Edu_NoFormalSchooling, Madhya[,395]), 2)*100, "%")
sw.sum.state[12,2] = paste0(round(w.mean(Madhya$Edu_UpTo5thStandard, Madhya[,395]), 2)*100, "%")
sw.sum.state[13,2] = paste0(round(w.mean(Madhya$Edu_MoreThan5thStandard, Madhya[,395]), 2)*100, "%")
sw.sum.state[14,2] = paste0(round(w.mean(Madhya$Caste_ScheduledCaste, Madhya[,395]), 2)*100, "%")
sw.sum.state[15,2] = paste0(round(w.mean(Madhya$Caste_ScheduledTribe, Madhya[,395]), 2)*100, "%")
sw.sum.state[16,2] = paste0(round(w.mean(Madhya$Caste_OtherBackwardClass, Madhya[,395]), 2)*100, "%")
sw.sum.state[17,2] = paste0(round(w.mean(Madhya$Caste_General, Madhya[,395]), 2)*100, "%")

sw.sum.state[1,3] = paste0(round(w.mean(Odisha$m4_q103_lpg, Odisha[,395]), 2)*100, "%")
sw.sum.state[2,3] = paste0(round(w.mean(Odisha$Decision_FemaleHouseholdHead, Odisha[,395]), 2)*100, "%")
sw.sum.state[3,3] = paste0(round(w.mean(Odisha$Decision_MaleHouseholdHead, Odisha[,395]), 2)*100, "%")
sw.sum.state[4,3] = paste0(round(w.mean(Odisha$Decision_Both, Odisha[,395]), 2)*100, "%")
sw.sum.state[5,3] = round(w.mean(Odisha$m1_q32_month_expenditure_log, Odisha[,395]), 2)
sw.sum.state[6,3] = round(w.mean(Odisha$m1_q27_no_adults, Odisha[,395]), 2)
sw.sum.state[7,3] = round(w.mean(Odisha$m1_q29_no_children, Odisha[,395]), 2)
sw.sum.state[8,3] = round(w.mean(Odisha$m1_q19_age, Odisha[,395]), 2)
sw.sum.state[9,3] = paste0(round(w.mean(Odisha$Religion_Hindu, Odisha[,395]), 2)*100, "%")
sw.sum.state[10,3] = paste0(round(w.mean(Odisha$Religion_Muslim, Odisha[,395]), 2)*100, "%")
sw.sum.state[11,3] = paste0(round(w.mean(Odisha$Edu_NoFormalSchooling, Odisha[,395]), 2)*100, "%")
sw.sum.state[12,3] = paste0(round(w.mean(Odisha$Edu_UpTo5thStandard, Odisha[,395]), 2)*100, "%")
sw.sum.state[13,3] = paste0(round(w.mean(Odisha$Edu_MoreThan5thStandard, Odisha[,395]), 2)*100, "%")
sw.sum.state[14,3] = paste0(round(w.mean(Odisha$Caste_ScheduledCaste, Odisha[,395]), 2)*100, "%")
sw.sum.state[15,3] = paste0(round(w.mean(Odisha$Caste_ScheduledTribe, Odisha[,395]), 2)*100, "%")
sw.sum.state[16,3] = paste0(round(w.mean(Odisha$Caste_OtherBackwardClass, Odisha[,395]), 2)*100, "%")
sw.sum.state[17,3] = paste0(round(w.mean(Odisha$Caste_General, Odisha[,395]), 2)*100, "%")


sw.sum.state[1,4] = paste0(round(w.mean(Uttar$m4_q103_lpg, Uttar[,395]), 2)*100, "%")
sw.sum.state[2,4] = paste0(round(w.mean(Uttar$Decision_FemaleHouseholdHead, Uttar[,395]), 2)*100, "%")
sw.sum.state[3,4] = paste0(round(w.mean(Uttar$Decision_MaleHouseholdHead, Uttar[,395]), 2)*100, "%")
sw.sum.state[4,4] = paste0(round(w.mean(Uttar$Decision_Both, Uttar[,395]), 2)*100, "%")
sw.sum.state[5,4] = round(w.mean(Uttar$m1_q32_month_expenditure_log, Uttar[,395]), 2)
sw.sum.state[6,4] = round(w.mean(Uttar$m1_q27_no_adults, Uttar[,395]), 2)
sw.sum.state[7,4] = round(w.mean(Uttar$m1_q29_no_children, Uttar[,395]), 2)
sw.sum.state[8,4] = round(w.mean(Uttar$m1_q19_age, Uttar[,395]), 2)
sw.sum.state[9,4] = paste0(round(w.mean(Uttar$Religion_Hindu, Uttar[,395]), 2)*100, "%")
sw.sum.state[10,4] = paste0(round(w.mean(Uttar$Religion_Muslim, Uttar[,395]), 2)*100, "%")
sw.sum.state[11,4] = paste0(round(w.mean(Uttar$Edu_NoFormalSchooling, Uttar[,395]), 2)*100, "%")
sw.sum.state[12,4] = paste0(round(w.mean(Uttar$Edu_UpTo5thStandard, Uttar[,395]), 2)*100, "%")
sw.sum.state[13,4] = paste0(round(w.mean(Uttar$Edu_MoreThan5thStandard, Uttar[,395]), 2)*100, "%")
sw.sum.state[14,4] = paste0(round(w.mean(Uttar$Caste_ScheduledCaste, Uttar[,395]), 2)*100, "%")
sw.sum.state[15,4] = paste0(round(w.mean(Uttar$Caste_ScheduledTribe, Uttar[,395]), 2)*100, "%")
sw.sum.state[16,4] = paste0(round(w.mean(Uttar$Caste_OtherBackwardClass, Uttar[,395]), 2)*100, "%")
sw.sum.state[17,4] = paste0(round(w.mean(Uttar$Caste_General, Uttar[,395]), 2)*100, "%")

sw.sum.state[1,5] = paste0(round(w.mean(WBengal$m4_q103_lpg, WBengal[,395]), 2)*100, "%")
sw.sum.state[2,5] = paste0(round(w.mean(WBengal$Decision_FemaleHouseholdHead, WBengal[,395]), 2)*100, "%")
sw.sum.state[3,5] = paste0(round(w.mean(WBengal$Decision_MaleHouseholdHead, WBengal[,395]), 2)*100, "%")
sw.sum.state[4,5] = paste0(round(w.mean(WBengal$Decision_Both, WBengal[,395]), 2)*100, "%")
sw.sum.state[5,5] = round(w.mean(WBengal$m1_q32_month_expenditure_log, WBengal[,395]), 2)
sw.sum.state[6,5] = round(w.mean(WBengal$m1_q27_no_adults, WBengal[,395]), 2)
sw.sum.state[7,5] = round(w.mean(WBengal$m1_q29_no_children, WBengal[,395]), 2)
sw.sum.state[8,5] = round(w.mean(WBengal$m1_q19_age, WBengal[,395]), 2)
sw.sum.state[9,5] = paste0(round(w.mean(WBengal$Religion_Hindu, WBengal[,395]), 2)*100, "%")
sw.sum.state[10,5] = paste0(round(w.mean(WBengal$Religion_Muslim, WBengal[,395]), 2)*100, "%")
sw.sum.state[11,5] = paste0(round(w.mean(WBengal$Edu_NoFormalSchooling, WBengal[,395]), 2)*100, "%")
sw.sum.state[12,5] = paste0(round(w.mean(WBengal$Edu_UpTo5thStandard, WBengal[,395]), 2)*100, "%")
sw.sum.state[13,5] = paste0(round(w.mean(WBengal$Edu_MoreThan5thStandard, WBengal[,395]), 2)*100, "%")
sw.sum.state[14,5] = paste0(round(w.mean(WBengal$Caste_ScheduledCaste, WBengal[,395]), 2)*100, "%")
sw.sum.state[15,5] = paste0(round(w.mean(WBengal$Caste_ScheduledTribe, WBengal[,395]), 2)*100, "%")
sw.sum.state[16,5] = paste0(round(w.mean(WBengal$Caste_OtherBackwardClass, WBengal[,395]), 2)*100, "%")
sw.sum.state[17,5] = paste0(round(w.mean(WBengal$Caste_General, WBengal[,395]), 2)*100, "%")

sw.sum.state[1,6] = paste0(round(w.mean(Bihar$m4_q103_lpg, Bihar[,395]), 2)*100, "%")
sw.sum.state[2,6] = paste0(round(w.mean(Bihar$Decision_FemaleHouseholdHead, Bihar[,395]), 2)*100, "%")
sw.sum.state[3,6] = paste0(round(w.mean(Bihar$Decision_MaleHouseholdHead, Bihar[,395]), 2)*100, "%")
sw.sum.state[4,6] = paste0(round(w.mean(Bihar$Decision_Both, Bihar[,395]), 2)*100, "%")
sw.sum.state[5,6] = round(w.mean(Bihar$m1_q32_month_expenditure_log, Bihar[,395]), 2)
sw.sum.state[6,6] = round(w.mean(Bihar$m1_q27_no_adults, Bihar[,395]), 2)
sw.sum.state[7,6] = round(w.mean(Bihar$m1_q29_no_children, Bihar[,395]), 2)
sw.sum.state[8,6] = round(w.mean(Bihar$m1_q19_age, Bihar[,395]), 2)
sw.sum.state[9,6] = paste0(round(w.mean(Bihar$Religion_Hindu, Bihar[,395]), 2)*100, "%")
sw.sum.state[10,6] = paste0(round(w.mean(Bihar$Religion_Muslim, Bihar[,395]), 2)*100, "%")
sw.sum.state[11,6] = paste0(round(w.mean(Bihar$Edu_NoFormalSchooling, Bihar[,395]), 2)*100, "%")
sw.sum.state[12,6] = paste0(round(w.mean(Bihar$Edu_UpTo5thStandard, Bihar[,395]), 2)*100, "%")
sw.sum.state[13,6] = paste0(round(w.mean(Bihar$Edu_MoreThan5thStandard, Bihar[,395]), 2)*100, "%")
sw.sum.state[14,6] = paste0(round(w.mean(Bihar$Caste_ScheduledCaste, Bihar[,395]), 2)*100, "%")
sw.sum.state[15,6] = paste0(round(w.mean(Bihar$Caste_ScheduledTribe, Bihar[,395]), 2)*100, "%")
sw.sum.state[16,6] = paste0(round(w.mean(Bihar$Caste_OtherBackwardClass, Bihar[,395]), 2)*100, "%")
sw.sum.state[17,6] = paste0(round(w.mean(Bihar$Caste_General, Bihar[,395]), 2)*100, "%")


rownames(sw.sum.state) = c("LPG adoption (=1)",
                           "Decision Maker: Female Household Head", "Decision Maker: Male Household Head", "Decision Maker: Both",
                           "Monthly expenditure (log)",
                           "Number of adults",
                           "Number of children",
                           "Age (years)",
                           "Religion: Hindu", "Religion: Muslim", 
                           "Education: No Formal Schooling", "Education: Up To 5th Standard", "Education: More Than 5th Standard",
                           "Caste: Scheduled Caste", "Caste: Scheduled Tribe", "Caste: Other Backward Class", "Caste: General")
colnames(sw.sum.state) = c("Jharkhand (n=840)", "Madhya Pradesh (n=1680)", "Odisha (n=504)", "Uttar Pradesh (n=3023)", "West Bengal (n=1005)", "Bihar (n=1511)")

sw.sum.state # VIEW THE TABLES

# PRINT THE TABLES
# stargazer(sw.sum.state, summary=FALSE, float=FALSE, digits = 2,
#           out = "~/Dropbox/Fuels (Carlos, Johannes)/Submission to Journal of Development Studies/Revisions/Tables/sw_sum_state.tex")

#########################
#####     Table A6   ####
#########################


RawDataHH["NLPG_Reasons"] <- RawDataHH$m4_q105_1_nlpg_unavailable + RawDataHH$m4_q105_2_nlpg_connect_expensive +
  RawDataHH$m4_q105_3_nlpg_monthly_expensive + RawDataHH$m4_q105_4_nlpg_dk

nlpg <- crosstab(RawDataHH, col.vars = "m1_q38_decision_maker", row.vars = "NLPG_Reasons", type = "f")
nlpg <- as.data.frame(nlpg[["crosstab"]])

nlpg_table <- data.frame(matrix(NA,7,3))

row.names(nlpg_table) <- c("0", "1", "2", "3", "4", "Total Reasons", "Mean Reasons Per Household")
colnames(nlpg_table) <- c("Woman Household Head (n=356)", "Man Household Head (n=5281)", "Both (n=989)")

nlpg_table[1:6,2] <- nlpg[1:6,3] #paste in from table
nlpg_table[1:6,1] <- nlpg[7:12,3] #paste in from table
nlpg_table[1:6,3] <- nlpg[13:18,3] #paste in from table

nlpg_table[6,1] <- (0*nlpg_table[1,1] + 1*nlpg_table[2,1] + 2*nlpg_table[3,1] +
                      3*nlpg_table[4,1] + 4*nlpg_table[5,1])  #compute total reasons from table
nlpg_table[7,1] <- (0*nlpg_table[1,1] + 1*nlpg_table[2,1] + 2*nlpg_table[3,1] +
                      3*nlpg_table[4,1] + 4*nlpg_table[5,1]) / 356 # divide by "n" to get mean

nlpg_table[6,2] <- (0*nlpg_table[1,2] + 1*nlpg_table[2,2] + 2*nlpg_table[3,2] +
                      3*nlpg_table[4,2] + 4*nlpg_table[5,2])  #compute total reasons from table
nlpg_table[7,2] <- (0*nlpg_table[1,2] + 1*nlpg_table[2,2] + 2*nlpg_table[3,2] +
                      3*nlpg_table[4,2] + 4*nlpg_table[5,2]) / 5281 # divide by "n" to get mean

nlpg_table[6,3] <- (0*nlpg_table[1,3] + 1*nlpg_table[2,3] + 2*nlpg_table[3,3] +
                      3*nlpg_table[4,3] + 4*nlpg_table[5,3])  #compute total reasons from table
nlpg_table[7,3] <- (0*nlpg_table[1,3] + 1*nlpg_table[2,3] + 2*nlpg_table[3,3] +
                      3*nlpg_table[4,3] + 4*nlpg_table[5,3]) / 989 # divide by "n" to get mean


nlpg_table <- round(nlpg_table, digits=2) # need to fix rounding manually in latex

# html
# stargazer(nlpg_table, summary=FALSE, float=FALSE, digits=2,
#           notes = c("Note: No respondents in the described sample have LPG."),
#           type = "html", out = "/Users/Carlos/Desktop/nlpg.html")

# latex
# stargazer(nlpg_table, summary=FALSE, float=FALSE, digits=2,
#           notes = c("Note: No respondents in the described sample have LPG."), 
#           out = "/Users/Carlos/Dropbox/Fuels (Carlos, Johannes)/Manuscript/Tables/nlpg.tex")