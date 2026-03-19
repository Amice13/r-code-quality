########### MANUSCRIPT: The Gendered Nature of Liquefied Petroleum Gas Stove Adoption and Use in Rural India
########### JOURNAL: JOURNAL OF DEVELOPMENT STUDIES
########### AUTHORS: CARLOS F. GOULD/1 AND JOHANNES URPELAINEN/2
########### AFFILIATIONS: 1/COLUMBIA UNIVERSITY MAILMAN SCHOOL OF PUBLIC HEALTH AND 2/JOHNS HOPKINS SCHOOL OF ADVANCED INTERNATIONAL STUDIES
########### PURPOSE: THIS CODE (#4) LOADS THE DATA AND DOES SOME VARIABLE PROCESSING


#########################
#####     Figure 1  ####
#########################

# make summary tables first, with error bars
# then make figures based on these tables

# religion
religion <- RawDataHH %>% 
  group_by(m1_q24_religion) %>%
  summarise(mean = w.mean(Has_LPG, weight),
            sd = w.sd(Has_LPG, weight),
            n = n())
religion <- religion[-c(3:4),]
religion$religion_cat <- c("Hindu", "Muslim")
religion$lpg <- c(1624, 224)
religion$se <- religion$sd / (religion$n^0.5)


# Caste
caste_order <- c("General", "Scheduled Caste", "Scheduled Tribe", "Other Backward Class")
caste <- RawDataHH %>% 
  group_by(m1_q25_caste) %>%
  summarise(mean = w.mean(Has_LPG, weight),
            sd = w.sd(Has_LPG, weight),
            n = n())
caste <- caste[-5,]
caste$m1_q25_caste <- c("Scheduled Caste", "Scheduled Tribe", "Other Backward Class", "General")
caste$lpg <- c(197, 70, 857, 727)
caste$se <- caste$sd / (caste$n^0.5)

# Education
education <- RawDataHH %>% 
  group_by(Edu_ThreeCategories) %>%
  summarise(mean = w.mean(Has_LPG, weight),
            sd = w.sd(Has_LPG, weight),
            n = n())
education$Edu_ThreeCategories <- c(">5th Standard","No Formal Edu", "≤5th Standard")
education$lpg <- c(243, 478, 1130)
education$se <- education$sd / (education$n^0.5)

education_order <- c("No Formal Edu", "≤5th Standard", ">5th Standard")


# Decision making
decision <- RawDataHH %>%
  group_by(m1_q38_decision_maker) %>%
  summarise(mean = w.mean(Has_LPG, weight),
            sd = w.sd(Has_LPG, weight),
            n = n())

decision <- decision[-c(4),] # drop "other" category we omit

decision$Decision_Maker <- c("Man", "Woman", "Both")
decision$se <- decision$sd / (decision$n^.5)
decision$lpg <- c(1397,130,262)
decision_order <- c("Man", "Woman", "Both")




# make figures

education_p <- ggplot(data=education, aes(x=education$Edu_ThreeCategories, y=education$mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(data=education, aes(ymin=mean, ymax=mean+se), width = 0.08, size = 1.50, color="grey35")+
  geom_text(aes(label=education$lpg), color="white", vjust=1.6, size=8) + ylim(c(0,0.4)) +
  xlab("") + ylab("Proportion LPG Adoption") + ggtitle("(A) Education") + 
  scale_color_grey() + theme_classic() + scale_x_discrete(limits = education_order) + 
  theme(title = element_text(size=28),
        axis.text.y = element_text(size=24),
        axis.text.x = element_text(size=20, angle = 20, hjust=1),
        axis.title = element_text(size=26)) 

caste_p<-ggplot(data=caste, aes(x=caste$m1_q25_caste, y=caste$mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(data=caste, aes(ymin=mean, ymax=mean+se), width = 0.08, size = 1.50, color="grey35")+
  geom_text(aes(label=caste$lpg), color="white", vjust=1.6, size = 8) + ylim(c(0,0.4)) +
  xlab("") + ylab("") + ggtitle("(B) Caste") + 
  scale_color_grey() + theme_classic() + scale_x_discrete(limits = caste_order) + 
  theme(title = element_text(size=28),
        axis.text.y = element_text(size=24),
        axis.text.x = element_text(size=20, angle = 20, hjust=1),
        axis.title = element_text(size=26)) 

religion_p <- ggplot(data=religion, aes(x=religion$religion_cat, y=religion$mean)) +
  geom_bar(stat = "identity") + ylim(c(0,0.4)) +
  geom_errorbar(data=religion, aes(ymin=religion$mean, ymax=religion$mean+religion$se), width = 0.08, size = 1.50, color="grey35")+
  geom_text(data=religion, aes(label=religion$lpg), color="white", vjust=1.6, size = 8) +
  xlab("") + ylab("Proportion LPG Adoption") + ggtitle("(C) Religion") +
  scale_color_grey() + theme_classic() + 
  theme(title = element_text(size=28),
        axis.text.y = element_text(size=24),
        axis.text.x = element_text(size=28),
        axis.title = element_text(size=26)) 

decision_p <- ggplot(data=decision, aes(x=decision$Decision_Maker, y=decision$mean)) +
  geom_bar(stat = "identity") +
  geom_errorbar(data=decision, aes(ymin=mean, ymax=mean+se), width = 0.08, size = 1.50, color="grey35")+
  geom_text(aes(label=decision$lpg), color="white", vjust=1.6, size = 8) + ylim(c(0,0.4)) +
  xlab("") + ylab("") + ggtitle("(D) Household Decision Maker") + 
  scale_color_grey() + theme_classic() + scale_x_discrete(limits = decision_order) +
  theme(title = element_text(size=28),
        axis.text.y = element_text(size=24),
        axis.text.x = element_text(size=28),
        axis.title = element_text(size=26)) 


fullbar <- plot_grid(education_p, caste_p, religion_p, decision_p, ncol=2, align="h") # package figures together


# save figures
# png("~/Figures/FullBar.png", 
#     width = 1200, height = 1000)

fullbar

# dev.off()




#########################
#####   Figure 2     ####
#########################


# Monthly expenditure

exp_p <- ggplot(RawDataHH, aes(x=m1_q32_month_expenditure_log, y=Has_LPG)) + 
  geom_point(alpha=0.4) + 
  geom_smooth(color="grey50", method = "loess") +
  xlab("Monthly Expenditure (log)") + ylab("") + ggtitle("(A) Monthly Expenditure") +
  scale_y_continuous(breaks = c(0,1), labels = c("No LPG", "Has LPG")) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_classic() +
  theme(title = element_text(size=28),
        axis.text = element_text(size=24),
        axis.title = element_text(size=26)) 

exp_p_m <- ggExtra::ggMarginal(exp_p,type="histogram",margins="x")


# Adults

adults_p <- ggplot(RawDataHH, aes(x=m1_q27_no_adults, y=Has_LPG)) + 
  geom_point(alpha=0.4) + 
  geom_smooth(color="grey50", method = "loess") +
  xlab("Number of Adults") + ylab("") + ggtitle("\n(C) Number of Adults") +
  scale_y_continuous(breaks = c(0,1), labels = c("No LPG", "Has LPG")) + 
  coord_cartesian(ylim=c(0,1), xlim=c(0,20)) +
  theme_classic() +
  theme(title = element_text(size=28),
        axis.text = element_text(size=24),
        axis.title = element_text(size=26)) 

adults_p_m <- ggExtra::ggMarginal(adults_p,type="histogram",margins="x")


# Children

child_p <- ggplot(RawDataHH, aes(x=m1_q29_no_children, y=Has_LPG)) + 
  geom_point(alpha=0.4) + 
  geom_smooth(color="grey50", method = "loess") +
  xlab("Number of Children") + ylab("") + ggtitle("\n(D) Number of Children") +
  scale_y_continuous(breaks = c(0,1), labels = c("No LPG", "Has LPG")) + 
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12)) + 
  coord_cartesian(ylim=c(0,1), xlim =c(0,12)) +
  theme_classic() +
  theme(title = element_text(size=28),
        axis.text = element_text(size=24),
        axis.title = element_text(size=26)) 

child_p_m <- ggExtra::ggMarginal(child_p,type="histogram",margins="x")

# nb: cut plot at n=12 children

# Age

age_p <- ggplot(RawDataHH, aes(x=m1_q19_age, y=Has_LPG)) + 
  geom_point(alpha=0.4) + 
  geom_smooth(color="grey50", method = "loess") +
  xlab("Years") + ylab("") + ggtitle("(B) Age of Respondent") +
  scale_y_continuous(breaks = c(0,1), labels = c("No LPG", "Has LPG")) + 
  #scale_x_continuous(breaks=c(0,2,4,6,8,10,12), labels=c("0", "2","4","6","8","10","12")) + 
  coord_cartesian(ylim=c(0,1)) +
  theme_classic() +
  theme(title = element_text(size=28),
        axis.text = element_text(size=24),
        axis.title = element_text(size=26)) 

age_p_m <- ggExtra::ggMarginal(age_p,type="histogram",margins="x")


fullscatter <- plot_grid(exp_p_m, age_p_m, adults_p_m, child_p_m, ncol=2, align="h")


# png("~/Figures/FullScatter.png", 
#     width = 1200, height = 1000)

fullscatter


# dev.off()




#########################
#####   Figure A3    ####
#########################

# Makes Panel A
lpg_percapita_fig <- ggplot(RawDataHH, aes(x=RawDataHH$kg_per_capita)) + 
  geom_density(aes(y = ..count..), fill="black") + theme_classic() + 
  ggtitle("(A) Distribution of monthly per capita LPG use") +
  xlab("LPG Use (kg/person/month)") + ylab("Number of Households") + 
  theme(plot.title=element_text(size=24),
        plot.subtitle = element_text(size=24),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22),
        legend.title = element_blank(),
        legend.position = c(0.85,0.95),
        legend.text = element_text(size=14)) +
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9)) + 
  coord_cartesian(xlim=c(0,9)) + 
  geom_segment(x=1.29, y=0, xend=1.29, yend=820, color="white", linetype=2) +
  annotate(geom="text",x=2, y = 20, label = "median=1.29", color="white", size = 6)

# Makes Panel B
density_firewood_LPG <- ggplot(RawDataHH_firewood_LPG, aes(x=kg_firewood_per_capita)) + 
  geom_density(aes(y=..count..), fill = "black") + theme_classic() + 
  ggtitle("(B) Distribution of weekly per capita firewood use: Has LPG") +
  xlab("Firewood Use (kg/person/week)") + ylab("Number of Households") + 
  theme(plot.title=element_text(size=24),
        plot.subtitle = element_text(size=24),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22),
        legend.title = element_blank(),
        legend.position = c(0.85,0.95),
        legend.text = element_text(size=14),
        strip.text = element_text(size=16)) +
  coord_cartesian(xlim = c(0,30), ylim = c(0,600)) + 
  geom_segment(x=4.67, y=0, xend=4.67, yend=580, color="white", linetype=2) +
  annotate(geom="text",x=7.5, y = 20, label = "median=4.67", color="white", size = 6)

# Makes Panel C
density_firewood_NoLPG <- ggplot(RawDataHH_firewood_NoLPG, aes(x=kg_firewood_per_capita)) + 
  geom_density(aes(y=..count..), fill = "black") + theme_classic() + 
  ggtitle("(C) Distribution of weekly per capita firewood use: No LPG") +
  xlab("Firewood Use (kg/person/week)") + ylab("Number of Households") + 
  theme(plot.title=element_text(size=24),
        plot.subtitle = element_text(size=24),
        axis.text = element_text(size=18),
        axis.title = element_text(size=22),
        legend.title = element_blank(),
        legend.position = c(0.85,0.95),
        legend.text = element_text(size=14),
        strip.text = element_text(size=16)) +
  coord_cartesian(xlim = c(0,30), ylim = c(0,600)) + 
  geom_segment(x=7.14, y=0, xend=7.14, yend=600, color="white", linetype=2) +
  annotate(geom="text",x=10, y = 20, label = "median=7.14", color="white", size = 6)


firewood_fig <- plot_grid(density_firewood_LPG, density_firewood_NoLPG, nrow = 2)

combined_lpguse_fig <- plot_grid(
  
  lpg_percapita_fig, firewood_fig, ncol=2
  
)

# png("~/Figures/lpg_dists.png", 
#     width = 1200, height = 1000)

combined_lpguse_fig # preview figure

# dev.off()

