#--------------------------------#

# Replication code for " Restrictive policies make highly skilled immigrants more welcome" 

# Authors: Siuyau Lee & Wilson Lai

# R version: 2022.12.0+353 (MacOS)

# This version : 28 May 2024

#--------------------------------#

# Uncomment lines below to install packages if needed

#install.packages("readxl")
#install.packages("dplyr")
#install.packages("naniar")
#install.packages("ggplot2")
#install.packages("cregg")
#install.packages("FindIt")


# load required packages ####
library("readxl")
library("dplyr")
library("naniar")
library("ggplot2")
library("cregg")
library("FindIt")



# set your own working directory by replacing ~/mypath/ 
# with the path to your working directory 


setwd("~/mypath/")



## read in data from the "Values" sheet of the "ReplicationData.xlsx" file
mf <- read_excel("ReplicationData.xlsx", sheet = "Values")

####### DATA CLEANING / RECODING #######

#remove NAs ####
mf1 <- mf
mf1[mf1 == -99] <- NA
mf1[mf1 == 8888] <- NA
mf1[mf1 == 8881] <- NA

# Recode variable for education
mf1$college_edu <- ifelse(mf1$DM3 %in% c("1", "2", "3", "4"), "0", ifelse(mf1$DM3 %in% c("5", "6", "7"), "1", NA ))

# Create variable for economic threat
mf1$econthreat_gp_sum <- rowSums(mf1[, c("W1_Q9", "W1_Q10")], na.rm = T)
mf1$Economic_threat <- dplyr::ntile(mf1$econthreat_gp_sum,2)

# Recode variable for household income
mf1$Household_Income <- with(mf1, ifelse(DM7>= 1 & DM7<= 10, "0", "1"))

# Recode variable for individual income
mf1$Individual_Income <- with(mf1,ifelse(DM8>=1 & DM8<=7, "0", "1" ))

# Recode variable for social class
mf1$Social_class <- mf1$DM9gp

# Recode variable for identity (forced choice) 
mf1$Group_identity <- ifelse(mf1$W1_Q16 %in% c("1"), "1", ifelse(mf1$W1_Q16 %in% c("2", "3", "4"), "2", NA ))

# Recode variable for identity (rating)
mf1$nat_diff_1 <- (mf1$W1_Q17 - mf1$W1_Q18)
mf1$Nativism <-with (mf1, ifelse(nat_diff_1>=0 & nat_diff_1<=76, "0", "1"))

# Recode variable for Ethnocentrism
mf1$Ethnocentrism <- ifelse(mf1$W1_Q4 %in% c("1", "2", "3", "4"), "0", ifelse(mf1$W1_Q4 %in% c("5", "6", "7"), "1", NA ))

# Create variable for egalitarianism
mf1$egl_sum <- rowSums(mf1[, c("W2_Q4_a", "W2_Q4_b")], na.rm = T)
mf1$Egalitarianism<- dplyr::ntile(-mf1$egl_sum,2)

# Create variable for General trust 
mf1$W2_Q4_d_recoded <- ifelse(mf1$W2_Q4_d == "5", 1, ifelse(mf1$W2_Q4_d == "4", 2, ifelse(mf1$W2_Q4_d == "3", 3, ifelse(mf1$W2_Q4_d == "2", 4, ifelse(mf1$W2_Q4_d == "1", 5, NA )))))

mf1$trust_sum <- rowSums(mf1[, c("W2_Q4_d_recoded", "W2_Q4_e")], na.rm = T)

mf1$Trust <- dplyr::ntile(mf1$trust_sum,2)


# Recode variable for pre-existing attitudes 
mf1$Pre_Attitude <- ifelse(mf1$W1_Q11 %in% c("1", "2"), "2", 
                           ifelse(mf1$W1_Q11 %in% 3, "1", 
                                  ifelse(mf1$W1_Q11 %in% c("4", "5"), "0", NA)))

# Recode variable for % of Highly skilled labor in the industry 
mf1$HSlabor_pct <- with(mf1, ifelse(DM5>=0 & DM5<= 19, "0", "1"))

# Recode variable for Disease threat 
mf1$W1_Q2_n <- ifelse(mf1$W1_Q2 %in% c("1", "2", "3") & (!mf1$W1_Q2 %in% c("4", "5", "6", "7")), "0", "1")

# Relevel and factorize policy items
mf1$Eligibility <- factor(mf1$Policy_Eligibility, levels = rev(unique(mf1$Policy_Eligibility)), ordered=FALSE, labels = c("No labor market test", "Stringent labor market test" ))
attr(mf1$Eligibility, "label") <- "Eligibility"
mf1$Eligibility =relevel(mf1$Eligibility, ref= "Stringent labor market test")

mf1$Employment <- factor(mf1$Policy_Employment, levels = rev(unique(mf1$Policy_Employment)), ordered= FALSE, labels = c("Allowed", "Not allowed"))
attr(mf1$Employment, "label") <- "Employment"
mf1$Employment =relevel(mf1$Employment, ref = "Not allowed")

mf1$Nationality <- factor(mf1$Policy_Nationality, levels = rev(unique(mf1$Policy_Nationality)),ordered= FALSE, labels =c("Mainland Chinese citizens only", "Non-mainland Chinese citizens only", "All non-Hong Kong citizens" ))
attr(mf1$Nationality, "label") <- "Nationality"

mf1$Language <- factor(mf1$Policy_Language, levels = rev(unique(mf1$Policy_Language)),ordered= FALSE,labels = c("Cantonese, Putonghua, or English","No requirement", "Cantonese or English") )
attr(mf1$Language, "label") <- "Language"
mf1$Language =relevel(mf1$Language, ref = "Cantonese or English")

mf1$Welfare <- factor(mf1$Policy_Welfare, levels = rev(unique(mf1$Policy_Welfare)),ordered= FALSE, labels = c("After one year of residence", "Immediate","After seven years of residence"))
attr(mf1$Welfare, "label") <- "Welfare"
mf1$Welfare =relevel(mf1$Welfare, ref = "After seven years of residence")

mf1$Tax <- factor(mf1$Policy_Tax, levels = rev(unique(mf1$Policy_Tax)),ordered= FALSE, labels = c("25% reduction in first year","50% reduction in first year","Pay the same taxes as natives"))
attr(mf1$Tax, "label") <- "Tax"
mf1$Tax =relevel(mf1$Tax, ref = "Pay the same taxes as natives")

mf1$Quota <- factor(mf1$Policy_Quota, levels = rev(unique(mf1$Policy_Quota)), ordered=FALSE, labels = c("40,000","25,000","10,000"))
attr(mf1$Quota, "label") <- "Quota"
mf1$Quota = factor(mf1$Quota, levels=c("10,000","25,000","40,000"))




# factorize remaining variables

mf1$College_education<- factor(mf1$college_edu)

mf1$Economic_threat<- factor(mf1$Economic_threat)

mf1$Household_Income <- factor(mf1$Household_Income)

mf1$Individual_Income <- factor(mf1$Individual_Income)

mf1$Social_class <- factor(mf1$Social_class)

mf1$Group_identity <- factor(mf1$Group_identity)

mf1$Nativism <- factor(mf1$Nativism)

mf1$Ethnocentrism <- factor(mf1$Ethnocentrism)

mf1$Egalitarianism <- factor(mf1$Egalitarianism)

mf1$Trust <- factor(mf1$Trust)

mf1$Pre_Attitude <- factor(mf1$Pre_Attitude)

mf1$HSlabor_pct <- factor(mf1$HSlabor_pct)

mf1$PolicyPairNo <- factor(mf1$PolicyPairNo)

mf1$W1_Q2_n <- factor(mf1$W1_Q2_n)


### Divide dataset into probability and non-probability sample sets ###

mfnp <- subset(mf1, PanelMemberType == "2")
mfp <- subset(mf1, PanelMemberType == "1")
mfnp2 <- filter(mfnp, !is.na(W2_Q1))
mfnp3 <- filter(mfnp, !is.na(W2_Q2))
mfp2 <- filter(mfp, !is.na(W2_Q1))


####### ANALYSIS #######


## Figure 2 (Nonprobability Panel, AMCE)

resultfg2 <- cregg::cj(
  mfnp2,
  W2_Q1 ~ Eligibility + Employment  + Language + Nationality*Quota + Tax + Welfare,
  id = ~ SurveyCaseid, 
  estimate = "amce",
  feature_order = c("Eligibility", "Employment", "Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


plot(resultfg2, 
     vline = 0, 
     xlab = "",
     h0= 0.5, 
     header_fmt = "%s",
     size = 0.5
) +
  ggplot2::scale_color_manual(values = rep("Black", 9)) +
  ggplot2::theme(axis.ticks=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", 
                     "bold","plain","plain","plain", "bold","plain","plain","plain", "bold","plain","plain", "bold","plain","plain", "bold"
                   )),
                 axis.text.x=element_blank(),
                 legend.position="none") + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "0.00", sprintf("%0.2f", estimate))),
    colour = "black", 
    size = 2,
    position = position_nudge(y = .5)
  )+ 
  geom_point(size = 0.3)



## Table A6 - Effects of policy liberalization on attitudes toward
##            highly skilled immigrants: numerical results (AMCE) ####  

resultfg2



#### Table 2 - Formal tests of effect heterogeneity #### 

#Subgroup identifier : College education 

cregg::cj_anova(subset(mfnp,!is.na(College_education)),
                W2_Q1 ~ Eligibility + Employment,
                id = ~ SurveyCaseid, by = ~College_education)


#Subgroup identifier : Economic threat 

cregg::cj_anova(subset(mfnp,!is.na(Economic_threat)),
                W2_Q1 ~ Eligibility + Employment,
                id = ~ SurveyCaseid, by = ~Economic_threat)

#Subgroup identifier : Hong Kong identity 

cregg::cj_anova(subset(mfnp,!is.na(Group_identity)),
                W2_Q1 ~ Language + Nationality,
                id = ~ SurveyCaseid, by = ~Group_identity)

#Subgroup identifier : Egalitarianism 

cregg::cj_anova(subset(mfnp,!is.na(Egalitarianism)),
                W2_Q1 ~ Welfare + Tax,
                id = ~ SurveyCaseid, by = ~Egalitarianism)


#Formal test of interaction effect - Social identity
f_2 <- cregg::cj_anova(
  subset(mfnp,!is.na(Group_identity)),
  W2_Q1 ~ Language + Nationality,
  id = ~ SurveyCaseid, 
  by = ~Group_identity)

f_2


#### Figure 3 / A7c Attitudinal effects of policy liberalization by social identity
#Fig 3 / A7c: CMM plot

mm_by_id_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Group_identity)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Group_identity), 
  vline = 0.5,
  group = "Group_identity",
  header_fmt = "%s",
  size = 0.5
)


mm_by_id_original_3 <- mm_by_id_original_2 +
  ggplot2::scale_color_grey(name = "Group Identity", labels=c("Hong Konger", "Chinese or Mixed Identities"), breaks=c("1","2")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm"),
        legend.justification = c("center", "top"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"),
        plot.margin= margin(t = 2, b=2, r = 10, unit = "mm"))+
  ggplot2::scale_x_continuous(
    breaks = c(0.3,0.4,0.5,0.6,0.7)
  ) 


mm_by_id_original_3



# Fig 3 / A7c: Plot showing estimated difference in MM

mm_by_cultural_id <- cregg::cj(
  subset(mfnp,!is.na(Group_identity)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0 = 0.5, 
  by = ~Group_identity,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


mm_by_cultural_og <- plot(mm_by_cultural_id, 
                          vline = 0, 
                          xlab = "Estimated Difference in Marginal Means \n by Group Identity",
                          size = 0.5
) +
  scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::scale_x_continuous(
    breaks = c(-0.2,-0.1,0.0,0.1,0.2)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        plot.margin= margin(t = 2, b = 8, l = 2, r = 4, unit = "mm")
  )

# Fig 3 / A7c: Stitch cmm and diff 

cowplot::plot_grid(
  mm_by_id_original_3, mm_by_cultural_og,
  ncol = 2, nrow = 1, rel_widths = c(1.6,1)
)





### Figure 4 /Figure A15: Effects of policy liberalization by pre-existing immigration attitudes  ####
#fig 4 / A15: CMM plot

plot(
  cregg::cj(
    subset(mfnp,!is.na(Pre_Attitude)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Pre_Attitude), 
  vline = 0.5,
  group = "Pre_Attitude",
  header_fmt = "%s",
  size = 0.5
) +
  ggplot2::scale_color_grey(name = "Pre Attitude", labels=c("Negative","Neutral","Positive"), breaks=c("0","1","2")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm")) +
  ggplot2::scale_size(range = c(0.3,0.3)) 


# fig 4 / A15: Formal test of interaction effect

f_a4 <- cregg::cj_anova(
  subset(mfnp,!is.na(Pre_Attitude)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Pre_Attitude)

f_a4



#### Figure A5
causalAnova <- CausalANOVA(W2_Q1 ~ Eligibility + Employment + Language + Nationality + Welfare + Tax + Quota,
                           data = mfnp,
                           pair.id=mfnp$SurveyCaseid,
                           nway=2,
                           select.prob=TRUE,
                           boot=500)


plot_cond.effect1 <- plot(causalAnova,
                          type = "ConditionalEffect",
                          fac.name = c("Eligibility" , "Nationality"),
                          treat.ind = 1,
                          interactive = TRUE)




##### Figure A6
# A6: Randomization -Education

mfnp_education <- mfnp %>% 
  filter(College_education=="1" | College_education=="0") 
mfnp_education$College_education <- as.factor(mfnp_education$College_education)

table(mfnp_education$College_education)

highedu <- mfnp_education %>% filter(College_education=="1")

f_highedu <- cj_freqs(highedu, ~ Eligibility+ Employment + Language + Nationality + Tax + Welfare + Quota, 
                      id = ~ SurveyCaseid,
                      level_order = c("descending"),
                      feature_order = c("Quota", "Welfare","Tax", "Nationality", "Language", "Employment", "Eligibility"))

f_highedu <- f_highedu %>% mutate(College_education="1")  %>% mutate(estimate=estimate/nrow(highedu)*100)

lowedu <- mfnp_education %>% filter(College_education=="0") 

f_lowedu <- cj_freqs(lowedu, ~  Eligibility+ Employment + Language + Nationality + Tax + Welfare + Quota, 
                     id = ~ SurveyCaseid,
                     level_order = c("descending"),
                     feature_order = c("Quota", "Welfare","Tax", "Nationality", "Language", "Employment", "Eligibility"))

f_lowedu <- f_lowedu %>% mutate(College_education="0")  %>% mutate(estimate=estimate/nrow(lowedu)*100)

f_edu <- bind_rows(f_highedu, f_lowedu)

p <- ggplot(f_edu, aes(x = level, 
                       y = estimate, 
                       group = feature,
                       fill = College_education))


p + geom_col(position = "dodge2") +
  coord_flip() +
  theme_bw() +
  labs(x = "Levels",y = "Percent") +
  scale_fill_manual(values = c("orange", "blue4"), name = "Education", labels = c("High school or below","College or above")) +
  theme(legend.position="bottom")

#ggsave("randomness_edu.png",
#       width = 10, 
#       height = 10, 
#       dpi = 150, 
#       units = "in", 
#       device='png',
#       scale=0.8)




#A6: Randomization check - Gender

mfnp_gender <- mfnp %>% filter(DM1=="1" | DM1=="2") 
mfnp_gender$DM1 <- as.factor(mfnp_gender$DM1)
mfnp_gender$gender <- factor(mfnp_gender$DM1, levels= c("1", "2"))
mfnp_gender %>% dplyr::group_by(DM1) %>% dplyr::summarise(mean_outcome=mean(Policy_Language))

males <- mfnp_gender %>% filter(DM1=="1")

f_m <- cj_freqs(males, ~ Eligibility+ Employment + Language + Nationality + Tax + Welfare + Quota, 
                id = ~ SurveyCaseid,
                level_order = c("descending"),
                feature_order = c("Quota", "Welfare","Tax", "Nationality", "Language", "Employment", "Eligibility"))

f_m <- f_m %>% mutate(DM1="1") %>% mutate(estimate=estimate/nrow(males)*100)

females <- mfnp_gender %>% filter(DM1=="2") 

f_f <- cj_freqs(females, ~ Eligibility+ Employment + Language + Nationality + Tax + Welfare + Quota, 
                id = ~ SurveyCaseid,
                level_order = c("descending"),
                feature_order = c("Quota", "Welfare","Tax", "Nationality", "Language", "Employment", "Eligibility"))

f_f <- f_f %>% mutate(DM1="2") %>% mutate(estimate=estimate/nrow(females)*100)

f_gender <- bind_rows(f_m, f_f)

p <- ggplot2::ggplot(f_gender, aes(x = level, 
                                   y = estimate,
                                   group = feature,
                                   fill = DM1))


p + geom_col(position = "dodge2") +
  coord_flip() +
  theme_bw() + 
  labs(x = "Levels",y = "Percent") +
  scale_fill_manual(values = c("blue4", "orange"), name = "Sex", labels = c("Male","Female")) +
  theme(legend.position="bottom")

#ggsave("randomness_gender.png",
#       width = 10, 
#       height = 10, 
#       dpi = 150, 
#       units = "in", 
#       device='png',
#       scale=0.8)





### Table A5: Effects of policy liberalization on attitudes toward 
###           highly skilled immigrants: numerical results (marginal means) ####

print(xtable::xtable(
  cregg::cj(
    mfnp, 
    W2_Q1 ~ Eligibility + Employment + Nationality + Language + Welfare + Tax + Quota + Quota*Nationality,
    estimate = "mm",
    id = ~ SurveyCaseid,
    h0 = 0.5
  )[c("feature", "level", "estimate", "std.error", "z", "p")],
  digits = 6, align = c("l", "l", "p{3in}", "r", "r", "r", "r")
), include.rownames = FALSE, size = "scriptsize")


mm(mfnp, 
   W2_Q1 ~ Eligibility + Employment + Nationality + Language + Welfare + Tax + Quota + Quota*Nationality,
   id = ~ SurveyCaseid,
   h0 = 0.5)




##### Figure A7

#### Figure A7a 
# A7a: CMM plot
mm_by_college_original_200 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(College_education)),
    W2_Q1 ~ Eligibility + Employment + Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    feature_headers = TRUE, 
    by = ~College_education), 
  vline = 0.5,
  group = "College_education",
  header_fmt = "%s",
  size = 0.8
)


mm_by_college_original_300 <-
  mm_by_college_original_200 + 
  ggplot2::scale_color_grey(name = "College Education", breaks=c("0","1"), 
                            labels=c("High school or below", "College or above")) + 
  ggplot2::scale_shape_manual(values=c(16,4))+
  ggplot2::theme(axis.ticks.y=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain", "plain", "bold",
                     "plain", "plain", "plain", "bold",
                     "plain", "plain", "plain", "bold",
                     "plain", "plain", "plain", "bold",
                     "plain", "plain", "plain", "bold",
                     "plain", "plain", "bold",
                     "plain", "plain", "bold"
                   )),
                 legend.box.spacing = unit(0, "mm"),
                 legend.justification = "center",
                 legend.text = element_text(size = 7),
                 legend.title = element_text(size = 8, face = "bold"),
                 plot.margin= margin(t = 2, b=2, r = 10, unit = "mm"))



# A7a: Formal test of interaction effect

f_a7a <- cregg::cj_anova(
  subset(mfnp,!is.na(College_education)),
  W2_Q1 ~ Eligibility + Employment + Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~College_education)

f_a7a


# A7a: Plot showing estimated difference in MM

mm_by_college_2 <- cregg::cj(
  subset(mfnp,!is.na(College_education)),
  W2_Q1 ~ Eligibility + Employment + Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0= 0.5,
  by = ~College_education,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


mm_by_college_3 <- plot(mm_by_college_2, 
                        vline = 0, 
                        xlab = "Estimated Difference in Marginal Means \n by College Education",
                        size = 0.5
) +
  ggplot2::scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::scale_x_continuous(
    breaks = c(-0.04,0.00,0.04)
  ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        plot.margin= margin(t = 2, b = 8, l = 2, r = 4, unit = "mm"))


#Figure A7a: Stitch mm and diff (full)

cowplot::plot_grid(
  mm_by_college_original_300, mm_by_college_3,
  ncol = 2, nrow = 1, rel_widths = c(1.6,1)
)



##Figure A7b (mm) 

# A7b: CMM plot
mm_by_et_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Economic_threat)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Economic_threat), 
  vline = 0.5,
  group = "Economic_threat",
  header_fmt = "%s",
  size = 0.5
)


mm_by_et_original_3 <- mm_by_et_original_2 +
  ggplot2::scale_color_grey(name = "Economic Threat", labels=c("Low Threat", "High Threat"), breaks=c("1","2")) +
  ggplot2::theme(axis.ticks.y=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
                   )),
                 legend.box.spacing = unit(0,"mm"),
                 legend.justification = c("center", "top"),
                 legend.text = element_text(size = 7),
                 legend.title = element_text(size = 8, face = "bold"),
                 plot.margin= margin(t = 2, b=2, r = 10, unit = "mm")) 


# A7b: Formal test of interaction effects

f_a7b <- cregg::cj_anova(
  subset(mfnp,!is.na(Economic_threat)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Economic_threat)

f_a7b


# A7b: Plot showing estimated difference in MM

mm_by_et <- cregg::cj(
  subset(mfnp,!is.na(Economic_threat)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0= 0.5,
  by = ~Economic_threat,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


mm_by_et_og <- plot(mm_by_et, 
                    vline = 0, 
                    xlab = "Estimated Difference in Marginal Means \n by Economic Threat",
                    size = 0.5
) +
  scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::scale_x_continuous(
    breaks = c(-0.04,0.00,0.04)
  ) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="none",
        plot.margin = margin(t=2, l=2, b=8, r=4, unit="mm"))

#Figure A7b: Stitch mm and diff (full)

cowplot::plot_grid(
  mm_by_et_original_3, mm_by_et_og,
  ncol = 2, nrow = 1, rel_widths = c(1.6,1)
)


#### Figure A7d 

#A7d: CMM plot

mm_by_egal_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Egalitarianism)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Egalitarianism), 
  vline = 0.5,
  group = "Egalitarianism",
  header_fmt = "%s",
  size = 0.5
)


mm_by_egal_original_3 <- mm_by_egal_original_2 +
  ggplot2::scale_color_grey(name = "Egalitarianism", labels=c("Low", "High"), breaks=c("1","2")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm"),
        legend.justification = c("center", "top"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"),
        plot.margin= margin(t = 2, b=2, r = 10, unit = "mm")) 


#A7d: Formal test of interactions effects

f_a7d <- cregg::cj_anova(
  subset(mfnp,!is.na(Egalitarianism)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Egalitarianism) 

f_a7d


#a7d: Plot showing estimated difference in MM 

mm_by_egal <- cregg::cj(
  subset(mfnp,!is.na(Egalitarianism)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0= 0.5, 
  by = ~Egalitarianism,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)



mm_by_egal2 <- plot(mm_by_egal, 
                    vline = 0, 
                    xlab = "Estimated Difference in Marginal Means \n by Egalitarianism",
                    size = 0.5
) +
  scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::scale_x_continuous(
    limits = c(-0.06, 0.06), 
    breaks = c(-0.06,-0.03,0.00,0.03,0.06)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        plot.margin= margin(t = 2, b = 8, l = 2, r = 4, unit = "mm")
  )


#Figure A7d: Stitch fig. mm and diff

cowplot::plot_grid(
  mm_by_egal_original_3, mm_by_egal2,
  ncol = 2, nrow = 1,
  rel_widths = c(1.6,1))







### Figure A8: Effects of policy liberalization by individual and household income ####


# A8: CMM Plot by Individual_Income 

mm_by_II_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Individual_Income)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Individual_Income), 
  vline = 0.5,
  group = "Individual_Income",
  header_fmt = "%s",
  size = 0.5
)


mm_by_II_original_3 <- mm_by_II_original_2 +
  ggplot2::scale_color_grey(name = "Individual_Income", labels=c("Low Income", "High Income"), breaks=c("0","1")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm")) 

# A8: Interaction effects-individual income

f_a8_II <- cregg::cj_anova(
  subset(mfnp,!is.na(Individual_Income)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Individual_Income)

f_a8_II


# A8: CMM Plot by Household_Income 

mm_by_HH_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Household_Income)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Household_Income), 
  vline = 0.5,
  group = "Household_Income",
  size = 0.5
)


mm_by_HH_original_3 <- mm_by_HH_original_2 +
  ggplot2::scale_color_grey(name = "Household_Income", labels=c("Low Income", "High Income"), breaks=c("0","1")) +
  theme(axis.ticks.y=element_blank(),
        axis.text.y =element_blank(),
        legend.box.spacing = unit(0, "mm")
  ) 

# Interaction effects- Household_Income 

f_a8_HH <- cregg::cj_anova(
  subset(mfnp,!is.na(Household_Income)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Household_Income)

f_a8_HH


#Figure A8: Stitch Individual_Income mm and Household_Income mm (full)

cowplot::plot_grid(
  mm_by_II_original_3, mm_by_HH_original_3,
  ncol = 2, nrow = 1,
  rel_widths = c(1.6,1)
)






### Figure A9: Effects of policy liberalization by perceived percentage of ####
###            highly skilled immigrants in the respondents' industry ####
###            and self-reported social class ####

# A9: CMM plot - est. % of HSI 

mm_by_HS_L_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(HSlabor_pct)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~HSlabor_pct), 
  vline = 0.5,
  group = "HSlabor_pct",
  header_fmt = "%s",
  size = 0.5
)


mm_by_HS_L_original_3 <- mm_by_HS_L_original_2 +
  ggplot2::scale_color_grey(name = "HSlabor_pct", labels=c("Low", "High"), breaks=c("0","1")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm")) 


# A9: Interaction effects - est. % of HSI

f_a9_HS <- cregg::cj_anova(
  subset(mfnp,!is.na(HSlabor_pct)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid,
  by = ~HSlabor_pct)

f_a9_HS


# A9: CMM plot- Social class 

mm_by_SC_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Social_class)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Social_class), 
  vline = 0.5,
  group = "Social_class",
  size = 0.5
)


mm_by_SC_original_3 <- mm_by_SC_original_2 +
  ggplot2::scale_color_grey(name = "Social class", labels=c("Upper class", "Middle class", "Lower class"), breaks=c("1","2","3")) +
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        legend.box.spacing = unit(0, "mm")
  ) 


# A9: Interaction effects - Social class

f_a9_SC <- cregg::cj_anova(
  subset(mfnp,!is.na(Social_class)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Social_class)

f_a9_SC


#Figure A9: Stitch HSlabor_pct mm mm and Social class mm (full)

cowplot::plot_grid(
  mm_by_HS_L_original_3, mm_by_SC_original_3,
  ncol = 2, nrow = 1,
  rel_widths = c(1.6,1)
)







### Figure A10: Effects of policy liberalization by social identity (rating) ####
## Identity (rating) is labelled as "Nativism" here

#A10: CMM plot - Social identity (rating)  

mm_by_nat_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Nativism)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Nativism), 
  vline = 0.5,
  group = "Nativism",
  header_fmt = "%s",
  size = 0.5
)


mm_by_nat_original_3 <- mm_by_nat_original_2 +
  ggplot2::scale_color_grey(name = "Nativism" , labels=c("Low", "High"), breaks=c("0","1")) + 
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm"),
        legend.justification = c("center", "top"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"),
        plot.margin= margin(t = 2, b=2, r = 10, unit = "mm")) 


# A10: Formal test of interactions effect



f_a10_2 <- cregg::cj_anova(
  subset(mfnp,!is.na(Nativism)),
  W2_Q1 ~ Language + Nationality,
  id = ~ SurveyCaseid, 
  by = ~Nativism
)

f_a10_2


#A10: Plot showing estimated difference in MM

mm_by_nativism <- cregg::cj(
  subset(mfnp,!is.na(Nativism)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0= 0.5, 
  by = ~Nativism,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


mm_by_nativism2 <- plot(mm_by_nativism, 
                        vline = 0, 
                        xlab = "Estimated Difference in Marginal Means \n by Nativism",
                        size = 0.5
) +
  scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::theme(legend.position = "none")+  
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin= margin(t = 2, b = 8, l=2, r = 4, unit = "mm")
  )

# Figure A10: Stitch CMM plot and mm diff 

cowplot::plot_grid(
  mm_by_nat_original_3, mm_by_nativism2,
  ncol = 2, nrow = 1,
  rel_widths = c(1.6,1)
)








#### Figure A11 : Effects of policy liberalization by ethnocentrism ####

#A11: CMM plot

mm_by_ethno_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Ethnocentrism)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Ethnocentrism), 
  vline = 0.5,
  group = "Ethnocentrism",
  header_fmt = "%s",
  size = 0.5
)


mm_by_ethno_original_3 <- mm_by_ethno_original_2 +
  ggplot2::scale_color_grey(name = "Ethnocentrism", labels=c("Low to Medium", "High"), breaks=c("0","1")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm"),
        legend.justification = c("center", "top"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"),
        plot.margin= margin(t = 2, b=2, r = 10, unit = "mm")) 

# A11: Formal test of interaction effect
f_a11_ethno <- cregg::cj_anova(
  subset(mfnp,!is.na(Ethnocentrism)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~Ethnocentrism) 

f_a11_ethno


f_a11_ethno_2 <- cregg::cj_anova(
  subset(mfnp,!is.na(Ethnocentrism)),
  W2_Q1 ~ Language + Nationality,
  id = ~ SurveyCaseid, 
  by = ~Ethnocentrism) 

f_a11_ethno_2


# A11: Plot showing estimated difference in MM   

mm_by_ethno <- cregg::cj(
  subset(mfnp,!is.na(Ethnocentrism)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0= 0.5, 
  by = ~Ethnocentrism,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


mm_by_ethno2 <- plot(mm_by_ethno, 
                     vline = 0, 
                     xlab = "Estimated Difference in Marginal Means \n by Ethnocentrism",
                     size = 0.5
) +
  scale_colour_manual("feature", values = rep("Black", 9))+
  ggplot2::scale_x_continuous(
    breaks = c(-0.06,-0.03,0.00,0.03,0.06)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        plot.margin= margin(t = 2, b = 8, l=2, r = 4, unit = "mm")
  )


#Figure A11: Stitch mm and diff (full)

cowplot::plot_grid(
  mm_by_ethno_original_3, mm_by_ethno2,
  ncol = 2, nrow = 1,
  rel_widths = c(1.6,1)
)






#### Figure A12: Effects of policy liberalization by generalized trust ####

#A12: CMM Plot

mm_by_trust_original_2 <- plot(
  cregg::cj(
    subset(mfnp,!is.na(Trust)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~Trust), 
  vline = 0.5,
  group = "Trust",
  header_fmt = "%s",
  size = 0.5
)


mm_by_trust_original_3 <- mm_by_trust_original_2 +
  ggplot2::scale_color_grey(name = "Trust", labels=c("Low", "High"), breaks=c("1","2")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm"),
        legend.justification = c("center", "top"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"),
        plot.margin= margin(t = 2, b=2, r = 10, unit = "mm"))


#A12: Formal test of interaction effects


f_a12_2 <- cregg::cj_anova(
  subset(mfnp,!is.na(Trust)),
  W2_Q1 ~ Welfare + Tax,
  id = ~ SurveyCaseid, 
  by = ~Trust)

f_a12_2


#Fig A12: Plot showing estimated difference in MM

mm_by_trust <- cregg::cj(
  subset(mfnp,!is.na(Trust)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  estimate = "mm_diff",
  h0= 0.5, 
  by = ~Trust,
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


mm_by_trust2 <- plot(mm_by_trust, 
                     vline = 0, 
                     xlab = "Estimated Difference in Marginal Means \n by Trust",
                     size = 0.5
) +
  scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::scale_colour_manual("feature", values = rep("Black", 9)) +
  ggplot2::scale_x_continuous(
    breaks = c(-0.04,0.00,0.04)
  ) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        plot.margin= margin(t = 2, b = 8, l=2, r = 4, unit = "mm")
  )


# Figure A12: Stitch mm and diff (full)

cowplot::plot_grid(
  mm_by_trust_original_3, mm_by_trust2,
  ncol = 2, nrow = 1,
  rel_widths = c(1.6,1)
)





#### Figure A13: Effects of policy liberalization
#### (DV - rating of impression toward immigrants)

#Without specifying the group variable
# A13: MM
mm_nogroup_2 <- plot(
  cregg::cj(
    subset(mfnp3,!is.na(Group_identity)),
    W2_Q2 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5), 
  header_fmt = "%s",
  size = 0.8
)


mm_nogroup_3 <- mm_nogroup_2 +
  ggplot2::scale_color_manual(values = c("black","black", "black","black","black","black","black"), guide = FALSE) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm"),
        legend.justification = c("center", "top"),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8, face = "bold"),
        legend.position = "none",
        plot.margin= margin(t = 2, b=2, r = 5, unit = "mm"))+
  ggplot2::scale_x_continuous(limits = c(1,5))

# A13: AMCE
amce_nogroup_2 <- cregg::cj(
  mfnp3,
  W2_Q2 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality*Quota,
  id = ~ SurveyCaseid, 
  estimate = "amce",
  feature_order = c("Eligibility", "Employment", "Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)



amce_nogroup_3<- plot(amce_nogroup_2, 
                      vline = 0, 
                      xlab = "Change in attitude",
                      h0= 0.5, 
                      header_fmt = "%s",
                      size = 0.8
) +
  ggplot2::scale_color_manual(values = rep("Black", 9)) +
  ggplot2::theme(axis.ticks=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", 
                     "bold","plain","plain","plain", "bold","plain","plain","plain", "bold","plain","plain", "bold","plain","plain", "bold"
                   )),
                 legend.position="none",
                 legend.text = element_text(size = 8),
                 axis.text.y = element_blank()) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "0.00", sprintf("%0.2f", estimate))),
    colour = "black", 
    size = 2,
    position = position_nudge(y = .5)
  )+
  geom_point(size=0.3)



# Fig A13: Stitch MM and AMCE 
cowplot::plot_grid(
  mm_nogroup_3, amce_nogroup_3,
  ncol = 2, nrow = 1, rel_widths = c(1.6,1)
)





### Figure A14: Effects of policy liberalization by task number ####
# A14: CMM plot

plot(
  cregg::cj(
    subset(mfnp,!is.na(PolicyPairNo)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5, 
    by = ~PolicyPairNo), 
  vline = 0.5,
  group = "PolicyPairNo",
  header_fmt = "%s",
  size = 0.5
)+
  ggplot2::scale_color_grey(breaks=c("1","2","3","4","5")) +
  ggplot2::scale_size(range = c(0.3,0.3))+
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )),
        legend.box.spacing = unit(0, "mm")) 


# A14: Formal test of interaction effect

f_a14 <-  cregg::cj_anova(
  subset(mfnp,!is.na(PolicyPairNo)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~PolicyPairNo)

f_a14










### Figure A16: Effects of policy liberalization by perceived COVID-19 vulnerability####
#A16: CMM plot

mm_by_W1<- plot(
  cregg::cj(
    subset(mfnp,!is.na(W1_Q2_n)),
    W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
    id = ~ SurveyCaseid, 
    estimate = "mm",
    level_order = c("descending"),
    feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
    h0= 0.5,
    by = ~W1_Q2_n), 
  vline = 0.5,
  group = "W1_Q2_n",
  header_fmt = "%s",
  size = 0.5
)


plot(mm_by_W1) +
  ggplot2::scale_color_grey(name = "Social W1_Q2_n", labels=c("Low threat", "High threat"), breaks=c("0","1")) +
  theme(axis.ticks.y=element_blank(),
        text = element_text(
          size = 12,
          face = c(
            "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain", "bold","plain", "plain", "bold"
          )
        ))+
  guides(color = guide_legend(title = "Disease threat"))


# A16: Formal test of interaction effect

f_a16 <- cregg::cj_anova(
  subset(mfnp,!is.na(W1_Q2_n)),
  W2_Q1 ~ Eligibility + Employment+ Language + Welfare + Tax + Nationality:Quota,
  id = ~ SurveyCaseid, 
  by = ~W1_Q2_n)

f_a16




### Figure A17: Effects of policy liberalization on attitudes toward highly 
###skilled immigrants (Probability Panel)####

#A17: AMCE plot

fga17 <- cregg::cj(
  mfp2,
  W2_Q1 ~ Eligibility + Employment  + Language + Nationality*Quota + Tax + Welfare,
  id = ~ SurveyCaseid, 
  estimate = "amce",
  level_order = c("descending")
)


plot(fga17, 
     vline = 0, 
     xlab = "",
     h0= 0.5, 
     header_fmt = "%s",
     size = 0.5
) +
  ggplot2::scale_color_manual(values = rep("Black", 9)) +
  ggplot2::theme(axis.ticks=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", 
                     "bold","plain","plain","plain", "bold","plain","plain","plain", "bold","plain","plain", "bold","plain","plain", "bold"
                   )),
                 axis.text.x=element_blank(),
                 legend.position="none") + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "0.00", sprintf("%0.2f", estimate))),
    colour = "black", 
    size = 2,
    position = position_nudge(y = .5)
  )+
  ggplot2::geom_point(size=0.3)




### Figure A 18 (Left): Effects of policy liberalization on attitudes toward highly skilled immigrants
###(data weighted by gender, age, educational attainment, and economic activity status) ####

#A18-amce plot (Left)
fga18_1 <- cregg::cj(
  mfnp2,
  W2_Q1 ~ Eligibility + Employment  + Language + Nationality*Quota + Tax + Welfare,
  id = ~ SurveyCaseid, 
  weights = ~ weight,
  estimate = "amce",
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)

plot(fga18_1, 
     vline = 0, 
     xlab = "",
     h0= 0.5, 
     header_fmt = "%s",
     size = 0.5
) +
  ggplot2::scale_color_manual(values = rep("Black", 9)) +
  ggplot2::theme(axis.ticks=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", 
                     "bold","plain","plain","plain", "bold","plain","plain","plain", "bold","plain","plain", "bold","plain","plain", "bold"
                   )),
                 axis.text.x=element_blank(),
                 legend.position="none") + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "0.00", sprintf("%0.2f", estimate))),
    colour = "black", 
    size = 2,
    position = position_nudge(y = .5)
  )+
  ggplot2::geom_point(size = 0.3)



### Figure A18 (Right): Effects of policy liberalization on attitudes toward highly skilled immigrants 
###(data weighted by gender, age, educational attainment, economic activity status, and Chinese 
###identification) ####  

#A18-amce plot (Right)

fga18_2 <- cregg::cj(
  mfnp2,
  W2_Q1 ~ Eligibility + Employment  + Language + Nationality*Quota + Tax + Welfare,
  id = ~ SurveyCaseid, 
  weights = ~ weight2,
  estimate = "amce",
  feature_order = c("Eligibility", "Employment","Language", "Nationality", "Tax", "Welfare", "Quota"),
  level_order = c("descending")
)


plot(fga18_2, 
     vline = 0, 
     xlab = "",
     h0= 0.5, 
     header_fmt = "%s",
     size = 0.5
) +
  ggplot2::scale_color_manual(values = rep("Black", 9)) +
  ggplot2::theme(axis.ticks=element_blank(),
                 text = element_text(
                   face = c(
                     "plain", "plain","plain", "bold","plain", "plain","plain", "bold","plain", "plain","plain", 
                     "bold","plain","plain","plain", "bold","plain","plain","plain", "bold","plain","plain", "bold","plain","plain", "bold"
                   )),
                 axis.text.x=element_blank(),
                 legend.position="none") + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "0.00", sprintf("%0.2f", estimate))),
    colour = "black", 
    size = 2,
    position = position_nudge(y = .5)
  )+
  ggplot2::geom_point(size = 0.3)









