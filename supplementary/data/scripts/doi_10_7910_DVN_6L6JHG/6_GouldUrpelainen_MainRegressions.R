########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS CODE (#6) RUNS THE REGRESSIONS FOR PRIMARY ANALYSES




#########################
#####     TABLE 3  ######
#########################


#Model 1: only the two decision maker variables, no state FE
fmod1 = svyglm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both,
               sdesign, family=quasibinomial("logit"))


#Model 2: only the two decision maker variables, with state FE
fmod2 = svyglm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                 as.factor(m1_q8_state), 
               sdesign, family=quasibinomial("logit"))


#Model 3: all covariates EXCEPT decision maker, no state FE
fmod3 = svyglm(m4_q103_lpg ~ m1_q32_month_expenditure_log + 
                 m1_q27_no_adults + m1_q29_no_children + 
                 m1_q19_age + 
                 Religion_Hindu + 
                 Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                 Edu_UpTo5thStandard + Edu_MoreThan5thStandard, 
               sdesign, family=quasibinomial("logit"))


#Model 4: all covariates EXCEPT decision maker, with state FE
fmod4 = svyglm(m4_q103_lpg ~ m1_q32_month_expenditure_log + 
                 m1_q27_no_adults + m1_q29_no_children + 
                 m1_q19_age + 
                 Religion_Hindu + 
                 Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                 Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                 as.factor(m1_q8_state),
               sdesign, family=quasibinomial("logit"))


#Model 5: all covariates with decision-maker vars on top, no state FE
fmod5 = svyglm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                 m1_q32_month_expenditure_log + 
                 m1_q27_no_adults + m1_q29_no_children + 
                 m1_q19_age + 
                 Religion_Hindu + 
                 Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                 Edu_UpTo5thStandard + Edu_MoreThan5thStandard,
               sdesign, family=quasibinomial("logit"))

#Model 6: all covariates with decision-maker vars on top, with state FE
fmod6 = svyglm(m4_q103_lpg ~ Decision_FemaleHouseholdHead + Decision_Both + 
                 m1_q32_month_expenditure_log + 
                 m1_q27_no_adults + m1_q29_no_children + 
                 m1_q19_age + 
                 Religion_Hindu + 
                 Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                 Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                 as.factor(m1_q8_state),
               sdesign, family=quasibinomial("logit"))


#### MARGINS MAIN RESULTS: LPG OWNERSHIP
# FOR THE MANUSCRIPT, THESE HAVE BEEN MANUALLY TRANSFERRED INTO LATEX

mmod1 <- summary(margins(fmod1, design = sdesign))
mmod2 <- summary(margins(fmod2, design = sdesign))
mmod3 <- summary(margins(fmod3, design = sdesign))
mmod4 <- summary(margins(fmod4, design = sdesign))
mmod5 <- summary(margins(fmod5, design = sdesign))
mmod6 <- summary(margins(fmod6, design = sdesign))

### R SQUARED FOR THE MANUSCRIPT TABLES

fit.svyglm(fmod1)
fit.svyglm(fmod2)
fit.svyglm(fmod3)
fit.svyglm(fmod4)
fit.svyglm(fmod5)
fit.svyglm(fmod6)




#########################
#####     FIGURE A1  ####
#########################


# ALL DECISION-MAKER SUBSETS:
decision_all <- summary(margins(fmod6, design = sdesign, at = list("Decision_FemaleHouseholdHead" = 0:1, "Decision_Both" = 0:1)))
decision_all <- subset(decision_all, !(decision_all$Decision_FemaleHouseholdHead==1 & decision_all$Decision_Both==1))
decision_all$DecisionMaker <- ifelse(decision_all$Decision_FemaleHouseholdHead==1, "Woman Household Head",
                                     ifelse(decision_all$Decision_Both==1, "Both", "Man Household Head"))

# DECISION-MAKER: WOMAN HOUSEHOLD HEAD
decision_fem <- summary(margins(fmod6, design = sdesign, at = list("Decision_FemaleHouseholdHead" = 0:1)))
decision_fem <- subset(decision_fem, decision_fem$factor=="Decision_FemaleHouseholdHead")
decision_fem$Decision_FemaleHouseholdHead <- ifelse(decision_fem$Decision_FemaleHouseholdHead==1, "Woman Household Head", "Not Woman Household Head")

decision_fem_order <- c("Not Woman Household Head", "Woman Household Head")

d_fem_fig <- ggplot(decision_fem, aes(x = Decision_FemaleHouseholdHead, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=decision_fem_order) +
  ggtitle("Decision-Maker: Woman Household Head") + 
  xlab("") + ylab("Predicted Value") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=12),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) + 
  coord_cartesian(ylim = c(-0.001, 0.15))

# DECISION-MAKER: BOTH
decision_both <- summary(margins(fmod6, design = sdesign, at = list("Decision_Both" = 0:1)))
decision_both <- subset(decision_both, decision_both$factor=="Decision_Both")
decision_both$Decision_Both <- ifelse(decision_both$Decision_Both==1, "Both", "Not Both")

decision_both_order <- c("Not Both", "Both")

d_both_fig <- ggplot(decision_both, aes(x = Decision_Both, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  scale_x_discrete(limits=decision_both_order) +
  ggtitle("Decision-Maker: Both Woman and Man") + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) +  
  coord_cartesian(ylim = c(-0.001, 0.15))


# Education: Up to 5th Standard
decision_edu5 <- summary(margins(fmod6, design = sdesign, at = list("Edu_UpTo5thStandard" = 0:1)))
decision_edu5 <- subset(decision_edu5, decision_edu5$factor=="Edu_UpTo5thStandard")
decision_edu5$Edu_UpTo5thStandard <- ifelse(decision_edu5$Edu_UpTo5thStandard==1, "Up To 5th Standard", "Not Up To 5th Standard")

education_order <- c("Not Up To 5th Standard", "Up To 5th Standard")

edu5_fig <- ggplot(decision_edu5, aes(x = Edu_UpTo5thStandard, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=education_order) +
  ggtitle("Education: Up To 5th Standard") + 
  xlab("") + ylab("Predicted Value") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) + 
  coord_cartesian(ylim = c(-0.001, 0.35))

# Education: More Than 5th Standard
decision_edm5 <- summary(margins(fmod6, design = sdesign, at = list("Edu_MoreThan5thStandard" = 0:1)))
decision_edm5 <- subset(decision_edm5, decision_edm5$factor=="Edu_MoreThan5thStandard")
decision_edm5$Edu_MoreThan5thStandard <- ifelse(decision_edm5$Edu_MoreThan5thStandard==1, "> 5th Standard", "Not > 5th Standard")

education5_order <- c("Not > 5th Standard", "> 5th Standard")

edm5_fig <- ggplot(decision_edm5, aes(x = Edu_MoreThan5thStandard, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=education5_order) +
  ggtitle("Education: More Than 5th Standard") + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) +  
  coord_cartesian(ylim = c(-0.001, 0.35))

# Religion: Hindu
religion_hindu <- summary(margins(fmod6, design = sdesign, at = list("Religion_Hindu" = 0:1)))
religion_hindu <- subset(religion_hindu, religion_hindu$factor=="Religion_Hindu")
religion_hindu$Religion_Hindu <- ifelse(religion_hindu$Religion_Hindu==1, "Hindu", "Not Hindu")

religion_order <- c("Not Hindu", "Hindu")

religion_hindu_fig <- ggplot(religion_hindu, aes(x = Religion_Hindu, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=religion_order) +
  ggtitle("Religion: Hindu") + 
  xlab("") + ylab("Predicted Value") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) +  
  coord_cartesian(ylim = c(-0.20, 0.05))


# Caste: Scheduled Caste
caste_sc <- summary(margins(fmod6, design = sdesign, at = list("Caste_ScheduledCaste" = 0:1)))
caste_sc <- subset(caste_sc, caste_sc$factor=="Caste_ScheduledCaste")
caste_sc$Caste_ScheduledCaste <- ifelse(caste_sc$Caste_ScheduledCaste==1, "Scheduled Caste", "Not Scheduled Caste")

caste_sc_order <- c("Not Scheduled Caste", "Scheduled Caste")

caste_sc_fig <- ggplot(caste_sc, aes(x = Caste_ScheduledCaste, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=caste_sc_order) +
  ggtitle("Caste: Scheduled Caste") + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) +  
  coord_cartesian(ylim = c(-0.20, 0.05))

# Caste: Scheduled Tribe
caste_st <- summary(margins(fmod6, design = sdesign, at = list("Caste_ScheduledTribe" = 0:1)))
caste_st <- subset(caste_st, caste_st$factor=="Caste_ScheduledTribe")
caste_st$Caste_ScheduledTribe <- ifelse(caste_st$Caste_ScheduledTribe==1, "Scheduled Tribe", "Not Scheduled Tribe")

caste_st_order <- c("Not Scheduled Tribe", "Scheduled Tribe")

caste_st_fig <- ggplot(caste_st, aes(x = Caste_ScheduledTribe, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=caste_st_order) +
  ggtitle("Caste: Scheduled Tribe") + 
  xlab("") + ylab("Predicted Value") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) +  
  coord_cartesian(ylim = c(-0.15, 0.0001))

# Caste: OBC
caste_obc <- summary(margins(fmod6, design = sdesign, at = list("Caste_OtherBackwardClass" = 0:1)))
caste_obc <- subset(caste_obc, caste_obc$factor=="Caste_OtherBackwardClass")
caste_obc$Caste_OtherBackwardClass <- ifelse(caste_obc$Caste_OtherBackwardClass==1, "OBC", "Not OBC")

caste_obc_order <- c("Not OBC", "OBC")

caste_obc_fig <- ggplot(caste_obc, aes(x = Caste_OtherBackwardClass, y = AME, ymin = lower, ymax = upper)) + 
  geom_point(position = position_dodge(width = .5), size=3) +
  geom_errorbar(position = position_dodge(width = .5), size=1.5, width = 0) + 
  geom_hline(yintercept=0, colour="grey60", linetype=2) + 
  ggtitle("") +
  scale_x_discrete(limits=caste_obc_order) +
  ggtitle("Caste: Other Backward Class") + 
  xlab("") + ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=20),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) +  
  coord_cartesian(ylim = c(-0.15, 0.0001))


# MONTHLY EXPENDITURE
monthly_exp <- data.frame(cplot(fmod6, "m1_q32_month_expenditure_log"))

monthly_exp_fig <- ggplot(monthly_exp, aes(x = xvals)) + 
  geom_line(aes(y = yvals), size = 1.15) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  scale_color_manual(values=c('#e41a1c','#377eb8')) +
  ggtitle("Monthly Expenditure") +
  xlab("Monthly Expenditure (Logarithmized)") + ylab("Predicted Value") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) + 
  coord_cartesian(ylim = c(-0.001, 0.90))

# AGE OF RESPONDENT

age <- data.frame(cplot(fmod6, "m1_q19_age"))

age_fig <- ggplot(age, aes(x = xvals)) + 
  geom_line(aes(y = yvals), size = 1.15) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Age of Respondent") +
  xlab("Year") + ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) + 
  coord_cartesian(ylim = c(-0.001, 0.90)) 

# NUMBER ADULTS

adults <- data.frame(cplot(fmod6, "m1_q27_no_adults"))

adults_fig <- ggplot(adults, aes(x = xvals)) + 
  geom_line(aes(y = yvals), size = 1.15) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Number of Adults") +
  xlab("Number of Adults") + ylab("Predicted Value") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) + 
  coord_cartesian(ylim = c(-0.001, 0.90))

# NUMBER children

children <- data.frame(cplot(fmod6, "m1_q29_no_children"))

children_fig <- ggplot(children, aes(x = xvals)) + 
  geom_line(aes(y = yvals), size = 1.15) +
  geom_line(aes(y = upper), linetype = 2) +
  geom_line(aes(y = lower), linetype = 2) +
  geom_hline(yintercept = 0) +
  ggtitle("Number of Children") +
  xlab("Number of Children") + ylab("") + 
  theme_bw() + 
  theme(plot.title = element_text(size=15),
        plot.subtitle = element_text(size=18),
        axis.text = element_text(size=15),
        axis.title = element_text(size=15),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size=18),
        legend.key.width = unit(1.5,"cm")) + 
  coord_cartesian(ylim = c(-0.001, 0.90))

# png("~//Figures/MarginalEffects_Covs.png",
#     height = 1400, width = 800)
# grid.arrange(d_fem_fig, d_both_fig,
#              edu5_fig, edm5_fig,
#              religion_hindu_fig, caste_sc_fig,
#              caste_st_fig, caste_obc_fig,
#              monthly_exp_fig, age_fig, 
#              adults_fig, children_fig,
#              ncol=2)
# dev.off()









#########################
#####     TABLE 6  ######
#########################


######### LPG AS A PRIMARY COOKING FUEL
# model 1
pusagemod1 <- svyglm(m5_q118_main_cookfuel==3  ~ Decision_FemaleHouseholdHead + Decision_Both + 
                       m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                       m1_q19_age + 
                       Religion_Hindu + 
                       Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                       Edu_UpTo5thStandard + Edu_MoreThan5thStandard,
                     sdesign_LPG, family=quasibinomial())

pusageme1 <- summary(margins(pusagemod1, design = sdesign_LPG))

fit.svyglm(pusagemod1) # R2

# model 2
pusagemod2 <- svyglm(m5_q118_main_cookfuel==3  ~ Decision_FemaleHouseholdHead + Decision_Both + 
                       m1_q32_month_expenditure_log + m1_q27_no_adults + m1_q29_no_children + 
                       m1_q19_age + 
                       Religion_Hindu + 
                       Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                       Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                       as.factor(m1_q8_state),
                     sdesign_LPG, family=quasibinomial(link="logit"))

summary(pusagemod2)

pusageme2 <- summary(margins(pusagemod2, design = sdesign_LPG))

fit.svyglm(pusagemod2) # R2

######### KG LPG PER MONTH
# model 3
usagemod1 = svyglm(log(kg_lpg) ~ Decision_FemaleHouseholdHead + Decision_Both + 
                     m1_q32_month_expenditure_log + 
                     m1_q27_no_adults + m1_q29_no_children + 
                     m1_q19_age + 
                     Religion_Hindu + 
                     Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                     Edu_UpTo5thStandard + Edu_MoreThan5thStandard, sdesign_LPG)

usageme1 <- summary(margins(usagemod1, design = sdesign_LPG))

usageme1$AME_percent <- (exp(2+1*usageme1$AME) / exp(2)) -1
usageme1$SE_percent <- (exp(2+1*usageme1$SE) / exp(2)) - 1

fit.svyglm(usagemod1)

# model 4
usagemod2 = svyglm(log(kg_lpg) ~ Decision_FemaleHouseholdHead + Decision_Both + 
                     m1_q32_month_expenditure_log +
                     m1_q27_no_adults + m1_q29_no_children + 
                     m1_q19_age + 
                     Religion_Hindu + 
                     Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                     Edu_UpTo5thStandard + Edu_MoreThan5thStandard +
                     as.factor(m1_q8_state), sdesign_LPG)

usageme2 <- summary(margins(usagemod2, design = sdesign_LPG))
usageme2$AME_percent <- (exp(2+1*usageme2$AME) / exp(2)) -1
usageme2$SE_percent <- (exp(2+1*usageme2$SE) / exp(2)) - 1

fit.svyglm(usagemod2)

######### KG FIREWOOD PER WEEK
# model 5
fusagemod1 = svyglm(log(m4_q109_1_firewood_quant) ~ Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log + 
                      m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard, sdesign_LPG_firewod)

fusageme1 <- summary(margins(fusagemod1, design = sdesign_LPG_firewod))
fusageme1$AME_percent <- (exp(2+1*fusageme1$AME) / exp(2)) -1
fusageme1$SE_percent <- (exp(2+1*fusageme1$SE) / exp(2)) - 1

fit.svyglm(fusagemod1)

# model 6
fusagemod2 = svyglm(log(m4_q109_1_firewood_quant) ~ Decision_FemaleHouseholdHead + Decision_Both + 
                      m1_q32_month_expenditure_log + 
                      m1_q27_no_adults + m1_q29_no_children + 
                      m1_q19_age + 
                      Religion_Hindu + 
                      Caste_ScheduledCaste + Caste_ScheduledTribe + Caste_OtherBackwardClass + 
                      Edu_UpTo5thStandard + Edu_MoreThan5thStandard + 
                      as.factor(m1_q8_state), sdesign_LPG_firewod)

fusageme2 <- summary(margins(fusagemod2, design = sdesign_LPG_firewod))
fusageme2$AME_percent <- (exp(2+1*fusageme2$AME) / exp(2)) -1
fusageme2$SE_percent <- (exp(2+1*fusageme2$SE) / exp(2)) - 1

fit.svyglm(fusagemod2)
