########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS CODE (#7) RUNS REGRESSIONS FOUND IN THE APPENDIX





#########################
##### TABLE A1 ##########
#########################

#Model 1: only the two decision maker variables, no village FE
#asmod are linear models without survey weights

asmod1 = lm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both, data = RawDataHH)
summary(asmod1)

#Model 2: only the two decision maker variables, with village FE
asmod2 = lm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + as.factor(m1_q11_village), RawDataHH)
summary(asmod2)


#Model 3: all covariates EXCEPT decision maker, no village FE
asmod3 = lm(m4_q103_lpg ~ m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
              m1_q19_age + 
              Religion_Hindu + 
              Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
              Edu_UpTo5thStandard + Edu_MoreThan5thStandard, RawDataHH)
summary(asmod3)

#Model 4: all covariates EXCEPT decision maker, with village FE
asmod4 = lm(m4_q103_lpg ~ m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
              m1_q19_age + 
              Religion_Hindu + 
              Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
              Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
              as.factor(m1_q11_village), RawDataHH)
summary(asmod4)

#Model 5: all covariates with decision-maker vars on top, no village FE
asmod5 = lm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
              m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
              m1_q19_age + 
              Religion_Hindu + 
              Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
              Edu_UpTo5thStandard + Edu_MoreThan5thStandard, RawDataHH)
summary(asmod5)

#Model 6: all covariates with decision-maker vars on top, with village FE
asmod6 = lm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
              m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
              m1_q19_age + 
              Religion_Hindu + 
              Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
              Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
              as.factor(m1_q11_village), RawDataHH)
summary(asmod6)


#no survey weights

# stargazer(list(asmod1, asmod2, asmod3, asmod4, asmod5, asmod6), 
#           covariate.labels = covariates_all, dep.var.labels = "LPG Adoption",
#           digits=3, star.cutoffs = c(0.05, 0.01, 0.001),
#           omit=c("factor", "Constant"), no.space=TRUE,
#           add.lines = list(c("Village FE", "", "Yes", "", "Yes", "", "Yes")),
#           omit.stat = c("rsq", "adj.rsq", "f", "ser"),
#           float = FALSE, type="html", notes = "This analysis does not include survey weights.", 
#           out = "/Tables/appendixregcovs.html")

# stargazer(list(asmod1, asmod2, asmod3, asmod4, asmod5, asmod6), 
#           covariate.labels = covariates_all, dep.var.labels = "LPG Adoption",
#           digits=3, star.cutoffs = c(0.05, 0.01, 0.001),
#           omit=c("factor", "Constant"), no.space=TRUE,
#           add.lines = list(c("Village FE", "", "Yes", "", "Yes", "", "Yes")),
#           omit.stat = c("rsq", "adj.rsq", "f", "ser"),
#           float = FALSE, notes = "This analysis does not include survey weights.", 
#           out = "/Tables/appendixregcovs.tex")







#########################
##### TABLE A2 ##########
#########################

#Model 1: only woman household head
fhhmod1 = svyglm(m4_q103_lpg ~ FemaleHH,
                 sdesign, family=quasibinomial("logit"))


#Model 2: only woman hh head, with state FE
fhhmod2 = svyglm(m4_q103_lpg ~ FemaleHH + as.factor(m1_q8_state), 
                 sdesign, family=quasibinomial("logit"))


#Model 3: all covariates EXCEPT household head, no state FE
fhhmod3 = svyglm(m4_q103_lpg ~ m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard, 
                 sdesign, family=quasibinomial("logit"))


#Model 4: all covariates EXCEPT household head, with state FE
fhhmod4 = svyglm(m4_q103_lpg ~ m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                   as.factor(m1_q8_state),
                 sdesign, family=quasibinomial("logit"))


#Model 5: all covariates with decision-maker vars on top, no state FE
fhhmod5 = svyglm(m4_q103_lpg ~ FemaleHH + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard,
                 sdesign, family=quasibinomial("logit"))

#Model 6: all covariates with decision-maker vars on top, with state FE
fhhmod6 = svyglm(m4_q103_lpg ~ FemaleHH + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                   as.factor(m1_q8_state),
                 sdesign, family=quasibinomial("logit"))



fhhmmod1 <- summary(margins(fhhmod1, design = sdesign))
fit.svyglm(fhhmod1)
fhhmmod2 <- summary(margins(fhhmod2, design = sdesign))
fit.svyglm(fhhmod2)
fhhmmod3 <- summary(margins(fhhmod3, design = sdesign))
fit.svyglm(fhhmod3)
fhhmmod4 <- summary(margins(fhhmod4, design = sdesign))
fit.svyglm(fhhmod4)
fhhmmod5 <- summary(margins(fhhmod5, design = sdesign))
fit.svyglm(fhhmod5)
fhhmmod6 <- summary(margins(fhhmod6, design = sdesign))
fit.svyglm(fhhmod6)






#########################
##### TABLE A4 ##########
#########################


m6_Jharkhand <- svyglm(formula = m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                         m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                         m1_q19_age + 
                         Religion_Hindu + 
                         Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                         Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_Jharkhand, family=quasibinomial("logit"))

mm6_J <- summary(margins(m6_Jharkhand, design = sdesign_Jharkhand))
fit.svyglm(m6_Jharkhand)

m6_Madhya <- svyglm(formula = m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_Madhya, family=quasibinomial("logit"))


mm6_M <- summary(margins(m6_Madhya, design = sdesign_Madhya))
fit.svyglm(m6_Madhya)

m6_Odisha <- svyglm(formula = m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_Odisha, family=quasibinomial("logit"))

mm6_O <- summary(margins(m6_Odisha, design = sdesign_Odisha))
fit.svyglm(m6_Odisha)


m6_Uttar <- svyglm(formula = m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                     m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                     m1_q19_age + 
                     Religion_Hindu + 
                     Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                     Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_Uttar, family=quasibinomial("logit"))

mm6_U <- summary(margins(m6_Uttar, design = sdesign_Uttar))
fit.svyglm(m6_Uttar)

m6_WBengal <- svyglm(formula = m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                       m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                       m1_q19_age + 
                       Religion_Hindu + 
                       Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                       Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_WBengal, family=quasibinomial("logit"))

mm6_WB <- summary(margins(m6_WBengal, design = sdesign_WBengal))
fit.svyglm(m6_WBengal)

m6_Bihar <- svyglm(formula = m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                     m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                     m1_q19_age + 
                     Religion_Hindu + 
                     Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                     Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_Bihar, family=quasibinomial("logit"))

mm6_B <- summary(margins(m6_Bihar, design = sdesign_Bihar))
fit.svyglm(m6_Bihar)





#########################
##### TABLE A5 ##########
#########################

nmmod1 <- svyglm(formula = m4_q105_1_nlpg_unavailable ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod1 <- summary(margins(nmmod1, design = sdesign_noLPG))
fit.svyglm(nmmod1)

nmmod2 <- svyglm(formula = m4_q105_1_nlpg_unavailable ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                   as.factor(m1_q8_state), design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod2 <- summary(margins(nmmod2, design = sdesign_noLPG))
fit.svyglm(nmmod2)


nmmod3 <- svyglm(formula = m4_q105_2_nlpg_connect_expensive ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod3 <- summary(margins(nmmod3, design = sdesign_noLPG))
fit.svyglm(nmmod3)


nmmod4 <- svyglm(formula = m4_q105_2_nlpg_connect_expensive ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                   as.factor(m1_q8_state), design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod4 <- summary(margins(nmmod4, design = sdesign_noLPG))
fit.svyglm(nmmod4)


nmmod5 <- svyglm(formula = m4_q105_3_nlpg_monthly_expensive ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod5 <- summary(margins(nmmod5, design = sdesign_noLPG))
fit.svyglm(nmmod5)


nmmod6 <- svyglm(formula = m4_q105_3_nlpg_monthly_expensive ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                   as.factor(m1_q8_state), design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod6 <- summary(margins(nmmod6, design = sdesign_noLPG))
fit.svyglm(nmmod6)

nmmod7 <- svyglm(formula = m4_q105_4_nlpg_dk ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard, design = sdesign_noLPG, family=quasibinomial("logit"))

nmemod7 <- summary(margins(nmmod7, design = sdesign_noLPG))
fit.svyglm(nmmod7)


nmmod8 <- svyglm(formula = m4_q105_4_nlpg_dk ~ Decision_FemaleHouseholdHead + Decision_Both + 
                   m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                   m1_q19_age + 
                   Religion_Hindu + 
                   Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                   Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                   as.factor(m1_q8_state), design = sdesign_noLPG, family=quasibinomial("logit"))
fit.svyglm(nmmod8)


nmemod8 <- summary(margins(nmmod8, design = sdesign_noLPG))






######################
###### TABLE A6    ###
######################

pusage_covariates <- c("Large LPG Cylinder (100 INR)", 
                       "One-way distance to acquire LPG cylinder (km)", 
                       "Decision Maker: Female Household Head", "Decision Maker: Both",
                       "Log Monthly Expenditure", "Number of Adults", "Number of Children", 
                       "Age (year)", 
                       "Hindu", 
                       "Scheduled Caste", "Scheduled Tribe", "Other Backward Caste", 
                       "Up to 5th Standard", "More than 5th Standard")

pusagemod3 = svyglm(m5_q118_main_cookfuel==3 ~ LPG_LCYLC_PRICE_100a + as.factor(m1_q8_state), sdesign_lcylc, family=quasibinomial("logit"))
pusagemod4 = svyglm(m5_q118_main_cookfuel==3 ~ LPG_distance_km + as.factor(m1_q8_state), sdesign_lpgdistnace, family=quasibinomial("logit"))
pusagemod6 = svyglm(m5_q118_main_cookfuel==3 ~ LPG_LCYLC_PRICE_100a + LPG_distance_km + 
                      Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log +
                      m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                      as.factor(m1_q8_state), sdesign_LPG_lcylc_distance, family=quasibinomial("logit"))


pusagememod3 <- summary(margins(pusagemod3, design = sdesign_LPG_lcylc))
pusagememod4 <- summary(margins(pusagemod4, design = sdesign_LPG_lpgdistnace))
pusagememod6 <- summary(margins(pusagemod6, design = sdesign_LPG_lcylc_distance))

fit.svyglm(pusagemod3)
fit.svyglm(pusagemod4)
fit.svyglm(pusagemod6)


######################
###### TABLE A7    ###
######################

lusagemod1 = svyglm(log(kg_lpg) ~ LPG_LCYLC_PRICE_100a +
                      as.factor(m1_q8_state), sdesign_LPG_lcylc)
lusagemod2 = svyglm(log(kg_lpg) ~ LPG_distance_km +
                      as.factor(m1_q8_state), sdesign_LPG_lpgdistnace)
lusagemod3 = svyglm(log(kg_lpg) ~ LPG_LCYLC_PRICE_100a + LPG_distance_km + 
                      Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log + 
                      m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                      as.factor(m1_q8_state), sdesign_LPG_lcylc_distance)
fit.svyglm(lusagemod1)
fit.svyglm(lusagemod2)
fit.svyglm(lusagemod3)

lusagemmod1 <- summary(margins(lusagemod1, design = sdesign_LPG_lcylc))
lusagemmod1$AME_percent <- (exp(2+1*lusagemmod1$AME) / exp(2)) -1
lusagemmod1$SE_percent <- (exp(2+1*lusagemmod1$SE) / exp(2)) - 1
lusagemmod1 # print

lusagemmod2 <- summary(margins(lusagemod2, design = sdesign_LPG_lpgdistnace))
lusagemmod2$AME_percent <- (exp(2+1*lusagemmod2$AME) / exp(2)) -1
lusagemmod2$SE_percent <- (exp(2+1*lusagemmod2$SE) / exp(2)) - 1
lusagemmod2

lusagemmod3 <- summary(margins(lusagemod3, design = sdesign_LPG_lcylc_distance))
lusagemmod3$AME_percent <- (exp(2+1*lusagemmod3$AME) / exp(2)) -1
lusagemmod3$SE_percent <- (exp(2+1*lusagemmod3$SE) / exp(2)) - 1
lusagemmod3



######################
###### TABLE A8    ###
######################

firewood_use_mod1 = svyglm(log(m4_q109_1_firewood_quant) ~ Has_LPG +
                             as.factor(m1_q8_state), sdesign_firewood)

firewood_use_mod2 = svyglm(log(m4_q109_1_firewood_quant) ~ Has_LPG + 
                             Decision_FemaleHouseholdHead + Decision_Both + 
                             m1_q32_month_expenditure_log + 
                             m1_q27_no_adults + m1_q29_no_children + 
                             m1_q19_age + 
                             Religion_Hindu + 
                             Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                             Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                             as.factor(m1_q8_state), sdesign_firewood)

firewood_use_mod3 = svyglm(log(m4_q109_1_firewood_quant) ~ Decision_FemaleHouseholdHead + Decision_Both + 
                             m1_q32_month_expenditure_log + 
                             m1_q27_no_adults + m1_q29_no_children + 
                             m1_q19_age + 
                             Religion_Hindu + 
                             Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                             Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                             as.factor(m1_q8_state), sdesign_noLPG)

firewood_use_m_mod1 <- summary(margins(firewood_use_mod1, design = sdesign_firewood)) # model 1
firewood_use_m_mod1$AME_percent <- (exp(2+1*firewood_use_m_mod1$AME) / exp(2)) -1
firewood_use_m_mod1$SE_percent <- (exp(2+1*firewood_use_m_mod1$SE) / exp(2)) - 1
firewood_use_m_mod1

firewood_use_m_mod2 <- summary(margins(firewood_use_mod2, design = sdesign_firewood)) # model 2
firewood_use_m_mod2$AME_percent <- (exp(2+1*firewood_use_m_mod2$AME) / exp(2)) -1
firewood_use_m_mod2$SE_percent <- (exp(2+1*firewood_use_m_mod2$SE) / exp(2)) - 1
firewood_use_m_mod2

firewood_use_m_mod3 <- summary(margins(firewood_use_mod3, design = sdesign_noLPG)) # model 3
firewood_use_m_mod3$AME_percent <- (exp(2+1*firewood_use_m_mod3$AME) / exp(2)) -1
firewood_use_m_mod3$SE_percent <- (exp(2+1*firewood_use_m_mod3$SE) / exp(2)) - 1
firewood_use_m_mod3

fit.svyglm(firewood_use_mod1)
fit.svyglm(firewood_use_mod2)
fit.svyglm(firewood_use_mod3)



######################
###### TABLE A9    ###
######################

fusagemod3 = svyglm(log(m4_q109_1_firewood_quant) ~ LPG_LCYLC_PRICE_100a +
                      as.factor(m1_q8_state), sdesign_LPG_firewod_lcyl)
fusagemod4 = svyglm(log(m4_q109_1_firewood_quant) ~ LPG_distance_km +
                      as.factor(m1_q8_state), sdesign_LPG_firewod_ldistance)
fusagemod5 = svyglm(log(m4_q109_1_firewood_quant) ~ Firewood_Collect_Distance +
                      as.factor(m1_q8_state), sdesign_LPG_firewod_fdistance)
fusagemod6 = svyglm(log(m4_q109_1_firewood_quant) ~ Firewood_MostlyCollect +
                      as.factor(m1_q8_state), sdesign_LPG_firewod)
fusagemod7 = svyglm(log(m4_q109_1_firewood_quant) ~ LPG_LCYLC_PRICE_100a + LPG_distance_km + 
                      Firewood_MostlyCollect +
                      Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log + 
                      m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                      as.factor(m1_q8_state), sdesign_LPG_firewod_lcyl_distance)


fit.svyglm(fusagemod3)
fit.svyglm(fusagemod4)
fit.svyglm(fusagemod5)
fit.svyglm(fusagemod6)
fit.svyglm(fusagemod7)

fusagemmod3 <- summary(margins(fusagemod3, design = sdesign_LPG_firewod_lcyl))
fusagemmod3$AME_percent <- (exp(2+1*fusagemmod3$AME) / exp(2)) -1
fusagemmod3$SE_percent <- (exp(2+1*fusagemmod3$SE) / exp(2)) - 1
fusagemmod3

fusagemmod4 <- summary(margins(fusagemod4, design = sdesign_LPG_firewod_ldistance))
fusagemmod4$AME_percent <- (exp(2+1*fusagemmod4$AME) / exp(2)) -1
fusagemmod4$SE_percent <- (exp(2+1*fusagemmod4$SE) / exp(2)) - 1
fusagemmod4

fusagemmod5 <- summary(margins(fusagemod5, design = sdesign_LPG_firewod_fdistance))
fusagemmod5$AME_percent <- (exp(2+1*fusagemmod5$AME) / exp(2)) -1
fusagemmod5$SE_percent <- (exp(2+1*fusagemmod5$SE) / exp(2)) - 1
fusagemmod5

fusagemmod6 <- summary(margins(fusagemod6, design = sdesign_LPG_firewod))
fusagemmod6$AME_percent <- (exp(2+1*fusagemmod6$AME) / exp(2)) -1
fusagemmod6$SE_percent <- (exp(2+1*fusagemmod6$SE) / exp(2)) - 1
fusagemmod6

fusagemmod7 <- summary(margins(fusagemod7, design = sdesign_LPG_firewod_lcyl_distance))
fusagemmod7$AME_percent <- (exp(2+1*fusagemmod7$AME) / exp(2)) -1
fusagemmod7$SE_percent <- (exp(2+1*fusagemmod7$SE) / exp(2)) - 1
fusagemmod7




