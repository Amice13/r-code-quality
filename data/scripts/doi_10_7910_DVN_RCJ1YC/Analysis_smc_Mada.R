# Please set your working directory below and put the data files there.

setwd("")

rm (list = ls(all=TRUE))

install.packages("ggplot2")
install.packages("dplyr")
install.packages("estimatr")

library(ggplot2)
library(dplyr)
library(estimatr)

smc <- read.csv("Data_MadagascarIE_smc.csv", header=T, stringsAsFactors = F)
smc_b <- read.csv("Data_MadagascarIE_smc_b.csv", header=T, stringsAsFactors = F)

smc$size <- as.factor(smc$size)
smc_b$size <- as.factor(smc_b$size)

#Table 7#####

#2017-2018 school year

smc_b_t <- subset(smc_b, 
                  treatment==1)

smc_b_c <- subset(smc_b, 
                  treatment==0)

#Percent of schools that organized general assembly to develop school action plan

mean(smc_b_t$ga_elaboration_school_action_plan_PEC_b)*100-
  mean(smc_b_c$ga_elaboration_school_action_plan_PEC_b)*100

reg1 <- lm_robust(ga_elaboration_school_action_plan_PEC_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Percent of schools that shared assessment results in the assembly

mean(smc_b_t$Shared_result_3_b)*100-
  mean(smc_b_c$Shared_result_3_b)*100

reg1 <- lm_robust(Shared_result_3_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Percent of schools that developed school action plan with local community

mean(smc_b_t$Existence_school_action_plan_PEC_b)*100-
  mean(smc_b_c$Existence_school_action_plan_PEC_b)*100

reg1 <- lm_robust(Existence_school_action_plan_PEC_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

#Average number of the type of activities implemented in school action plan

mean(smc_b_t$num_activity_realized_b)-
  mean(smc_b_c$num_activity_realized_b)

reg1 <- lm_robust(num_activity_realized_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Average number of the type of activities implemented in school action plan 
#with voluntary contributions

mean(smc_b_t$num_activity_realized_voluntary_b)-
  mean(smc_b_c$num_activity_realized_voluntary_b)

reg1 <- lm_robust(num_activity_realized_voluntary_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Percent of schools that implemented supplementary class: T3

#T3
mean(smc_b_t$T3_remedial_activity_b)*100-
  mean(smc_b_c$T3_remedial_activity_b)*100

reg1 <- lm_robust(T3_remedial_activity_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#T4
mean(smc_b_t$T4_remedial_activity_b, na.rm = T)*100-
  mean(smc_b_c$T4_remedial_activity_b, na.rm = T)*100

reg1 <- lm_robust(T4_remedial_activity_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

#T5
mean(smc_b_t$T5_remedial_activity_b, na.rm = T)*100-
  mean(smc_b_c$T5_remedial_activity_b, na.rm = T)*100

reg1 <- lm_robust(T5_remedial_activity_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#2018-2019 school year

smc_t <- subset(smc, 
                treatment==1)

smc_c <- subset(smc, 
                treatment==0)

#Percent of schools that organized general assembly to develop school action plan

mean(smc_t$ga_elaboration_school_action_plan_PEC)*100-
  mean(smc_c$ga_elaboration_school_action_plan_PEC)*100

reg1 <- lm_robust(ga_elaboration_school_action_plan_PEC~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Percent of schools that shared assessment results in the assembly

mean(smc_t$Shared_result_3)*100-
  mean(smc_c$Shared_result_3)*100

reg1 <- lm_robust(Shared_result_3~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Percent of schools that developed school action plan with local community

mean(smc_t$Existence_school_action_plan_PEC)*100-
  mean(smc_c$Existence_school_action_plan_PEC)*100

reg1 <- lm_robust(Existence_school_action_plan_PEC~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

#Average number of the type of activities implemented in school action plan

mean(smc_t$num_activity_realized)-
  mean(smc_c$num_activity_realized)

reg1 <- lm_robust(num_activity_realized~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Average number of the type of activities implemented in school action plan 
#with voluntary contributions

mean(smc_t$num_activity_realized_voluntary)-
  mean(smc_c$num_activity_realized_voluntary)

reg1 <- lm_robust(num_activity_realized_voluntary~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Percent of schools that implemented supplementary class: T3

#T3
mean(smc_t$T3_remedial_activity)*100-
  mean(smc_c$T3_remedial_activity)*100

reg1 <- lm_robust(T3_remedial_activity~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#T4
mean(smc_t$T4_remedial_activity, na.rm = T)*100-
  mean(smc_c$T4_remedial_activity, na.rm = T)*100

reg1 <- lm_robust(T4_remedial_activity~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

#T5
mean(smc_t$T5_remedial_activity, na.rm = T)*100-
  mean(smc_c$T5_remedial_activity, na.rm = T)*100

reg1 <- lm_robust(T5_remedial_activity~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Appendix 6#####

#2017-2018 school year

#Supplementary class (remedial activities) #1

mean(smc_b_t$activities_realized_1_b)*100-
  mean(smc_b_c$activities_realized_1_b)*100

reg1 <- lm_robust(activities_realized_1_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Preparation of certification exam of primary education #2

mean(smc_b_t$activities_realized_4_b)*100-
  mean(smc_b_c$activities_realized_4_b)*100

reg1 <- lm_robust(activities_realized_4_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Monitoring of classroom attendance #3

mean(smc_b_t$activities_realized_19_b)*100-
  mean(smc_b_c$activities_realized_19_b)*100

reg1 <- lm_robust(activities_realized_19_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Purchase of teaching and learning materials #4

mean(smc_b_t$activities_realized_2_b)*100-
  mean(smc_b_c$activities_realized_2_b)*100

reg1 <- lm_robust(activities_realized_2_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Purchase of student stationery items #5

mean(smc_b_t$activities_realized_20_b)*100-
  mean(smc_b_c$activities_realized_20_b)*100

reg1 <- lm_robust(activities_realized_20_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Construction of classrooms #6

mean(smc_b_t$activities_realized_6_b)*100-
  mean(smc_b_c$activities_realized_6_b)*100

reg1 <- lm_robust(activities_realized_6_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Repair of classrooms #7

mean(smc_b_t$activities_realized_7_b)*100-
  mean(smc_b_c$activities_realized_7_b)*100

reg1 <- lm_robust(activities_realized_7_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Manufacturing school furniture such as desk and chair #8

mean(smc_b_t$activities_realized_8_b)*100-
  mean(smc_b_c$activities_realized_8_b)*100

reg1 <- lm_robust(activities_realized_8_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Repair of school furniture #9

mean(smc_b_t$activities_realized_9_b)*100-
  mean(smc_b_c$activities_realized_9_b)*100

reg1 <- lm_robust(activities_realized_9_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Activities related to provision of water #10

mean(smc_b_t$activities_realized_11_b)*100-
  mean(smc_b_c$activities_realized_11_b)*100

reg1 <- lm_robust(activities_realized_11_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Construction of toilets #11

mean(smc_b_t$activities_realized_12_b)*100-
  mean(smc_b_c$activities_realized_12_b)*100

reg1 <- lm_robust(activities_realized_12_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Construction of school wall or/and contract with security guard #12

mean(smc_b_t$activities_realized_14_b)*100-
  mean(smc_b_c$activities_realized_14)*100

reg1 <- lm_robust(activities_realized_14_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#School garden (e.g., growing vegetables in school) #13

mean(smc_b_t$activities_realized_16_b)*100-
  mean(smc_b_c$activities_realized_16_b)*100

reg1 <- lm_robust(activities_realized_16_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#School feeding #14

mean(smc_b_t$activities_realized_15_b)*100-
  mean(smc_b_c$activities_realized_15_b)*100

reg1 <- lm_robust(activities_realized_15_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#School health (e.g., purchase of medicines) #15

mean(smc_b_t$activities_realized_17_b)*100-
  mean(smc_b_c$activities_realized_17_b)*100

reg1 <- lm_robust(activities_realized_17_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Contribution for the salary for non-regular teachers #16

mean(smc_b_t$activities_realized_10_b)*100-
  mean(smc_b_c$activities_realized_10_b)*100

reg1 <- lm_robust(activities_realized_10_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Purchase of materials for SMC office #17

mean(smc_b_t$activities_realized_3_b)*100-
  mean(smc_b_c$activities_realized_3_b)*100

reg1 <- lm_robust(activities_realized_3_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Travel expenses for school principal and SMC members #18

mean(smc_b_t$activities_realized_5_b)*100-
  mean(smc_b_c$activities_realized_5_b)*100

reg1 <- lm_robust(activities_realized_5_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Organization of school sports #19

mean(smc_b_t$activities_realized_18_b)*100-
  mean(smc_b_c$activities_realized_18_b)*100

reg1 <- lm_robust(activities_realized_18_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Cleaning school #20

mean(smc_b_t$activities_realized_13_b)*100-
  mean(smc_b_c$activities_realized_13_b)*100

reg1 <- lm_robust(activities_realized_13_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#Income generating activities #21

mean(smc_b_t$activities_realized_21_b)*100-
  mean(smc_b_c$activities_realized_21_b)*100

reg1 <- lm_robust(activities_realized_21_b~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_b, se_type = "stata")

summary(reg1)

#2018-2019 school year

#Supplementary class (remedial activities) #1

mean(smc_t$activities_realized_1)*100-
  mean(smc_c$activities_realized_1)*100

reg1 <- lm_robust(activities_realized_1~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Preparation of certification exam of primary education #2

mean(smc_t$activities_realized_4)*100-
  mean(smc_c$activities_realized_4)*100

reg1 <- lm_robust(activities_realized_4~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Monitoring of classroom attendance #3

mean(smc_t$activities_realized_19)*100-
  mean(smc_c$activities_realized_19)*100

reg1 <- lm_robust(activities_realized_19~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of teaching and learning materials #4

mean(smc_t$activities_realized_2)*100-
  mean(smc_c$activities_realized_2)*100

reg1 <- lm_robust(activities_realized_2~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of student stationery items #5

mean(smc_t$activities_realized_20)*100-
  mean(smc_c$activities_realized_20)*100

reg1 <- lm_robust(activities_realized_20~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of classrooms #6

mean(smc_t$activities_realized_6)*100-
  mean(smc_c$activities_realized_6)*100

reg1 <- lm_robust(activities_realized_6~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Repair of classrooms #7

mean(smc_t$activities_realized_7)*100-
  mean(smc_c$activities_realized_7)*100

reg1 <- lm_robust(activities_realized_7~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Manufacturing school furniture such as desk and chair #8

mean(smc_t$activities_realized_8)*100-
  mean(smc_c$activities_realized_8)*100

reg1 <- lm_robust(activities_realized_8~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Repair of school furniture #9

mean(smc_t$activities_realized_9)*100-
  mean(smc_c$activities_realized_9)*100

reg1 <- lm_robust(activities_realized_9~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Activities related to provision of water #10

mean(smc_t$activities_realized_11)*100-
  mean(smc_c$activities_realized_11)*100

reg1 <- lm_robust(activities_realized_11~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of toilets #11

mean(smc_t$activities_realized_12)*100-
  mean(smc_c$activities_realized_12)*100

reg1 <- lm_robust(activities_realized_12~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of school wall or/and contract with security guard #12

mean(smc_t$activities_realized_14)*100-
  mean(smc_c$activities_realized_14)*100

reg1 <- lm_robust(activities_realized_14~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School garden (e.g., growing vegetables in school) #13

mean(smc_t$activities_realized_16)*100-
  mean(smc_c$activities_realized_16)*100

reg1 <- lm_robust(activities_realized_16~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School feeding #14

mean(smc_t$activities_realized_15)*100-
  mean(smc_c$activities_realized_15)*100

reg1 <- lm_robust(activities_realized_15~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School health (e.g., purchase of medicines) #15

mean(smc_t$activities_realized_17)*100-
  mean(smc_c$activities_realized_17)*100

reg1 <- lm_robust(activities_realized_17~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Contribution for the salary for non-regular teachers #16

mean(smc_t$activities_realized_10)*100-
  mean(smc_c$activities_realized_10)*100

reg1 <- lm_robust(activities_realized_10~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of materials for SMC office #17

mean(smc_t$activities_realized_3)*100-
  mean(smc_c$activities_realized_3)*100

reg1 <- lm_robust(activities_realized_3~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Travel expenses for school principal and SMC members #18

mean(smc_t$activities_realized_5)*100-
  mean(smc_c$activities_realized_5)*100

reg1 <- lm_robust(activities_realized_5~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Organization of school sports #19

mean(smc_t$activities_realized_18)*100-
  mean(smc_c$activities_realized_18)*100

reg1 <- lm_robust(activities_realized_18~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Cleaning school #20

mean(smc_t$activities_realized_13)*100-
  mean(smc_c$activities_realized_13)*100

reg1 <- lm_robust(activities_realized_13~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Income generating activities #21

mean(smc_t$activities_realized_21)*100-
  mean(smc_c$activities_realized_21)*100

reg1 <- lm_robust(activities_realized_21~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#DD: arrange dataframe

smc_r <- subset(smc,
                select=c(treatment,
                         CISCO,
                         CATEGORIE_COMMUNE,
                         size,
                         activities_realized_1,
                         activities_realized_2,
                         activities_realized_3,
                         activities_realized_4,
                         activities_realized_5,
                         activities_realized_6,
                         activities_realized_7,
                         activities_realized_8,
                         activities_realized_9,
                         activities_realized_10,
                         activities_realized_11,
                         activities_realized_12,
                         activities_realized_13,
                         activities_realized_14,
                         activities_realized_15,
                         activities_realized_16,
                         activities_realized_17,
                         activities_realized_18,
                         activities_realized_19,
                         activities_realized_20,
                         activities_realized_21))

smc_b_r <- subset(smc_b,
                  select=c(treatment,
                           CISCO,
                           CATEGORIE_COMMUNE,
                           size,
                           activities_realized_1_b,
                           activities_realized_2_b,
                           activities_realized_3_b,
                           activities_realized_4_b,
                           activities_realized_5_b,
                           activities_realized_6_b,
                           activities_realized_7_b,
                           activities_realized_8_b,
                           activities_realized_9_b,
                           activities_realized_10_b,
                           activities_realized_11_b,
                           activities_realized_12_b,
                           activities_realized_13_b,
                           activities_realized_14_b,
                           activities_realized_15_b,
                           activities_realized_16_b,
                           activities_realized_17_b,
                           activities_realized_18_b,
                           activities_realized_19_b,
                           activities_realized_20_b,
                           activities_realized_21_b))

names(smc_b_r) <- c("treatment",
                    "CISCO",
                    "CATEGORIE_COMMUNE",
                    "size",
                    "activities_realized_1",
                    "activities_realized_2",
                    "activities_realized_3",
                    "activities_realized_4",
                    "activities_realized_5",
                    "activities_realized_6",
                    "activities_realized_7",
                    "activities_realized_8",
                    "activities_realized_9",
                    "activities_realized_10",
                    "activities_realized_11",
                    "activities_realized_12",
                    "activities_realized_13",
                    "activities_realized_14",
                    "activities_realized_15",
                    "activities_realized_16",
                    "activities_realized_17",
                    "activities_realized_18",
                    "activities_realized_19",
                    "activities_realized_20",
                    "activities_realized_21")

smc_b_r$timing <- c(0)
smc_r$timing <- c(1)

smc_r2 <- rbind.data.frame(smc_b_r,
                           smc_r)

smc_r2$treatment_end <-
  smc_r2$treatment*smc_r2$timing

smc_r_t <- subset(smc_r,
                  treatment==1)

smc_r_c <- subset(smc_r,
                  treatment==0)

smc_b_r_t <- subset(smc_b_r,
                    treatment==1)

smc_b_r_c <- subset(smc_b_r,
                    treatment==0)

#DD: Supplementary class (remedial activities) #1

(mean(smc_r_t$activities_realized_1)*100-
    mean(smc_r_c$activities_realized_1)*100)-
  (mean(smc_b_r_t$activities_realized_1)*100-
     mean(smc_b_r_c$activities_realized_1)*100)

reg1 <- lm_robust(activities_realized_1~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#Preparation of certification exam of primary education #2

(mean(smc_r_t$activities_realized_4)*100-
    mean(smc_r_c$activities_realized_4)*100)-
  (mean(smc_b_r_t$activities_realized_4)*100-
     mean(smc_b_r_c$activities_realized_4)*100)

reg1 <- lm_robust(activities_realized_4~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Monitoring of classroom attendance #3

(mean(smc_r_t$activities_realized_19)*100-
    mean(smc_r_c$activities_realized_19)*100)-
  (mean(smc_b_r_t$activities_realized_19)*100-
     mean(smc_b_r_c$activities_realized_19)*100)

reg1 <- lm_robust(activities_realized_19~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Purchase of teaching and learning materials #4

(mean(smc_r_t$activities_realized_2)*100-
    mean(smc_r_c$activities_realized_2)*100)-
  (mean(smc_b_r_t$activities_realized_2)*100-
     mean(smc_b_r_c$activities_realized_2)*100)

reg1 <- lm_robust(activities_realized_2~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Purchase of student stationery items #5

(mean(smc_r_t$activities_realized_20)*100-
    mean(smc_r_c$activities_realized_20)*100)-
  (mean(smc_b_r_t$activities_realized_20)*100-
     mean(smc_b_r_c$activities_realized_20)*100)

reg1 <- lm_robust(activities_realized_20~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Construction of classrooms #6

(mean(smc_r_t$activities_realized_6)*100-
    mean(smc_r_c$activities_realized_6)*100)-
  (mean(smc_b_r_t$activities_realized_6)*100-
     mean(smc_b_r_c$activities_realized_6)*100)

reg1 <- lm_robust(activities_realized_6~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Repair of classrooms #7

(mean(smc_r_t$activities_realized_7)*100-
    mean(smc_r_c$activities_realized_7)*100)-
  (mean(smc_b_r_t$activities_realized_7)*100-
     mean(smc_b_r_c$activities_realized_7)*100)

reg1 <- lm_robust(activities_realized_7~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Manufacturing school furniture such as desk and chair #8

(mean(smc_r_t$activities_realized_8)*100-
    mean(smc_r_c$activities_realized_8)*100)-
  (mean(smc_b_r_t$activities_realized_8)*100-
     mean(smc_b_r_c$activities_realized_8)*100)

reg1 <- lm_robust(activities_realized_8~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Repair of school furniture #9

(mean(smc_r_t$activities_realized_9)*100-
    mean(smc_r_c$activities_realized_9)*100)-
  (mean(smc_b_r_t$activities_realized_9)*100-
     mean(smc_b_r_c$activities_realized_9)*100)

reg1 <- lm_robust(activities_realized_9~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Activities related to provision of water #10

(mean(smc_r_t$activities_realized_11)*100-
    mean(smc_r_c$activities_realized_11)*100)-
  (mean(smc_b_r_t$activities_realized_11)*100-
     mean(smc_b_r_c$activities_realized_11)*100)

reg1 <- lm_robust(activities_realized_11~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Construction of toilets #11

(mean(smc_r_t$activities_realized_12)*100-
    mean(smc_r_c$activities_realized_12)*100)-
  (mean(smc_b_r_t$activities_realized_12)*100-
     mean(smc_b_r_c$activities_realized_12)*100)

reg1 <- lm_robust(activities_realized_12~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Construction of school wall or/and contract with security guard #12

(mean(smc_r_t$activities_realized_14)*100-
    mean(smc_r_c$activities_realized_14)*100)-
  (mean(smc_b_r_t$activities_realized_14)*100-
     mean(smc_b_r_c$activities_realized_14)*100)

reg1 <- lm_robust(activities_realized_14~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: School garden (e.g., growing vegetables in school) #13

(mean(smc_r_t$activities_realized_16)*100-
    mean(smc_r_c$activities_realized_16)*100)-
  (mean(smc_b_r_t$activities_realized_16)*100-
     mean(smc_b_r_c$activities_realized_16)*100)

reg1 <- lm_robust(activities_realized_16~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: School feeding #14

(mean(smc_r_t$activities_realized_15)*100-
    mean(smc_r_c$activities_realized_15)*100)-
  (mean(smc_b_r_t$activities_realized_15)*100-
     mean(smc_b_r_c$activities_realized_15)*100)

reg1 <- lm_robust(activities_realized_15~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: School health (e.g., purchase of medicines) #15

(mean(smc_r_t$activities_realized_17)*100-
    mean(smc_r_c$activities_realized_17)*100)-
  (mean(smc_b_r_t$activities_realized_17)*100-
     mean(smc_b_r_c$activities_realized_17)*100)

reg1 <- lm_robust(activities_realized_17~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Contribution for the salary for non-regular teachers #16

(mean(smc_r_t$activities_realized_10)*100-
    mean(smc_r_c$activities_realized_10)*100)-
  (mean(smc_b_r_t$activities_realized_10)*100-
     mean(smc_b_r_c$activities_realized_10)*100)

reg1 <- lm_robust(activities_realized_10~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Purchase of materials for SMC office #17

(mean(smc_r_t$activities_realized_3)*100-
    mean(smc_r_c$activities_realized_3)*100)-
  (mean(smc_b_r_t$activities_realized_3)*100-
     mean(smc_b_r_c$activities_realized_3)*100)

reg1 <- lm_robust(activities_realized_3~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#Travel expenses for school principal and SMC members #18

(mean(smc_r_t$activities_realized_5)*100-
    mean(smc_r_c$activities_realized_5)*100)-
  (mean(smc_b_r_t$activities_realized_5)*100-
     mean(smc_b_r_c$activities_realized_5)*100)

reg1 <- lm_robust(activities_realized_5~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Organization of school sports #19

(mean(smc_r_t$activities_realized_18)*100-
    mean(smc_r_c$activities_realized_18)*100)-
  (mean(smc_b_r_t$activities_realized_18)*100-
     mean(smc_b_r_c$activities_realized_18)*100)

reg1 <- lm_robust(activities_realized_18~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Cleaning school #20

(mean(smc_r_t$activities_realized_13)*100-
    mean(smc_r_c$activities_realized_13)*100)-
  (mean(smc_b_r_t$activities_realized_13)*100-
     mean(smc_b_r_c$activities_realized_13)*100)

reg1 <- lm_robust(activities_realized_13~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#DD: Income generating activities #21

(mean(smc_r_t$activities_realized_21)*100-
    mean(smc_r_c$activities_realized_21)*100)-
  (mean(smc_b_r_t$activities_realized_21)*100-
     mean(smc_b_r_c$activities_realized_21)*100)

reg1 <- lm_robust(activities_realized_21~
                    treatment+
                    timing+
                    treatment_end+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc_r2, se_type = "stata")

reg1_r <- tidy(reg1, conf.int = F)
reg1_r[4,]

#Appendix 7#### 

#2018-2019 school year

#voluntary contributions

#Supplementary class (remedial activities) #1
mean(smc_t$voluntary_resource_moblized_for_activity_1_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_1_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_1_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Preparation of certification exam of primary education #2
mean(smc_t$voluntary_resource_moblized_for_activity_4_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_4_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_4_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Monitoring of classroom attendance #3
mean(smc_t$voluntary_resource_moblized_for_activity_19_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_19_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_19_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of teaching and learning materials #4
mean(smc_t$voluntary_resource_moblized_for_activity_2_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_2_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_2_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of student stationery items #5
mean(smc_t$voluntary_resource_moblized_for_activity_20_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_20_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_20_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of classrooms #6
mean(smc_t$voluntary_resource_moblized_for_activity_6_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_6_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_6_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Repair of classrooms #7
mean(smc_t$voluntary_resource_moblized_for_activity_7_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_7_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_7_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Manufacturing school furniture such as desk and chair #8
mean(smc_t$voluntary_resource_moblized_for_activity_8_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_8_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_8_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Repair of school furniture #9
mean(smc_t$voluntary_resource_moblized_for_activity_9_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_9_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_9_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Activities related to provision of water #10
mean(smc_t$voluntary_resource_moblized_for_activity_11_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_11_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_11_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of toilets #11
mean(smc_t$voluntary_resource_moblized_for_activity_12_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_12_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_12_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of school wall or/and contract with security guard #12
mean(smc_t$voluntary_resource_moblized_for_activity_14_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_14_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_14_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School garden (e.g., growing vegetables in school) #13
mean(smc_t$voluntary_resource_moblized_for_activity_16_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_16_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_16_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School feeding #14
mean(smc_t$voluntary_resource_moblized_for_activity_15_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_15_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_15_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School health (e.g., purchase of medicines) #15
mean(smc_t$voluntary_resource_moblized_for_activity_17_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_17_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_17_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Contribution for the salary for non-regular teachers #16
mean(smc_t$voluntary_resource_moblized_for_activity_10_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_10_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_10_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of materials for SMC office #17
mean(smc_t$voluntary_resource_moblized_for_activity_3_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_3_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_3_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Travel expenses for school principal and SMC members #18
mean(smc_t$voluntary_resource_moblized_for_activity_5_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_5_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_5_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Organization of school sports #19
mean(smc_t$voluntary_resource_moblized_for_activity_18_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_18_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_18_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Cleaning school #20
mean(smc_t$voluntary_resource_moblized_for_activity_13_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_13_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_13_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Income generating activities #21
mean(smc_t$voluntary_resource_moblized_for_activity_21_r_total)-
  mean(smc_c$voluntary_resource_moblized_for_activity_21_r_total)

reg1 <- lm_robust(voluntary_resource_moblized_for_activity_21_r_total~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#external resources

#Supplementary class (remedial activities) #1
mean(smc_t$amount_used_external_resource_1)-
  mean(smc_c$amount_used_external_resource_1)

reg1 <- lm_robust(amount_used_external_resource_1~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Preparation of certification exam of primary education #2
mean(smc_t$amount_used_external_resource_4)-
  mean(smc_c$amount_used_external_resource_4)

reg1 <- lm_robust(amount_used_external_resource_4~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Monitoring of classroom attendance #3
mean(smc_t$amount_used_external_resource_19)-
  mean(smc_c$amount_used_external_resource_19)

reg1 <- lm_robust(amount_used_external_resource_19~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of teaching and learning materials #4
mean(smc_t$amount_used_external_resource_2)-
  mean(smc_c$amount_used_external_resource_2)

reg1 <- lm_robust(amount_used_external_resource_2~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of student stationery items #5
mean(smc_t$amount_used_external_resource_20)-
  mean(smc_c$amount_used_external_resource_20)

reg1 <- lm_robust(amount_used_external_resource_20~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of classrooms #6
mean(smc_t$amount_used_external_resource_6_r)-
  mean(smc_c$amount_used_external_resource_6_r)

reg1 <- lm_robust(amount_used_external_resource_6_r~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Repair of classrooms #7
mean(smc_t$amount_used_external_resource_7)-
  mean(smc_c$amount_used_external_resource_7)

reg1 <- lm_robust(amount_used_external_resource_7~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Manufacturing school furniture such as desk and chair #8
mean(smc_t$amount_used_external_resource_8_r)-
  mean(smc_c$amount_used_external_resource_8_r)

reg1 <- lm_robust(amount_used_external_resource_8_r~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Repair of school furniture #9
mean(smc_t$amount_used_external_resource_9)-
  mean(smc_c$amount_used_external_resource_9)

reg1 <- lm_robust(amount_used_external_resource_9~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Activities related to provision of water #10
mean(smc_t$amount_used_external_resource_11)-
  mean(smc_c$amount_used_external_resource_11)

reg1 <- lm_robust(amount_used_external_resource_11~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of toilets #11
mean(smc_t$amount_used_external_resource_12)-
  mean(smc_c$amount_used_external_resource_12)

reg1 <- lm_robust(amount_used_external_resource_12~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Construction of school wall or/and contract with security guard #12
mean(smc_t$amount_used_external_resource_14)-
  mean(smc_c$amount_used_external_resource_14)

reg1 <- lm_robust(amount_used_external_resource_14~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School garden (e.g., growing vegetables in school) #13
mean(smc_t$amount_used_external_resource_16)-
  mean(smc_c$amount_used_external_resource_16)

reg1 <- lm_robust(amount_used_external_resource_16~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School feeding #14
mean(smc_t$amount_used_external_resource_15_r)-
  mean(smc_c$amount_used_external_resource_15_r)

reg1 <- lm_robust(amount_used_external_resource_15_r~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#School health (e.g., purchase of medicines) #15
mean(smc_t$amount_used_external_resource_17)-
  mean(smc_c$amount_used_external_resource_17)

reg1 <- lm_robust(amount_used_external_resource_17~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Contribution for the salary for non-regular teachers #16
mean(smc_t$amount_used_external_resource_10)-
  mean(smc_c$amount_used_external_resource_10)

reg1 <- lm_robust(amount_used_external_resource_10~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Purchase of materials for SMC office #17
mean(smc_t$amount_used_external_resource_3)-
  mean(smc_c$amount_used_external_resource_3)

reg1 <- lm_robust(amount_used_external_resource_3~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Travel expenses for school principal and SMC members #18
mean(smc_t$amount_used_external_resource_5)-
  mean(smc_c$amount_used_external_resource_5)

reg1 <- lm_robust(amount_used_external_resource_5~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Organization of school sports #19
mean(smc_t$amount_used_external_resource_18_r)-
  mean(smc_c$amount_used_external_resource_18_r)

reg1 <- lm_robust(amount_used_external_resource_18_r~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Cleaning school #20
mean(smc_t$amount_used_external_resource_13)-
  mean(smc_c$amount_used_external_resource_13)

reg1 <- lm_robust(amount_used_external_resource_13~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

summary(reg1)

#Income generating activities #21
mean(smc_t$amount_used_external_resource_21)-
  mean(smc_c$amount_used_external_resource_21)

reg1 <- lm_robust(amount_used_external_resource_21~
                    treatment+
                    CISCO*
                    CATEGORIE_COMMUNE*
                    size,
                  data=smc, se_type = "stata")

#Table 8#####

#coding variables

smc$size <- as.factor(smc$size)

#percent of regular teachers

smc$Teacher_functionnaire_1 <- 
  ifelse(smc$Teacher_1_type==1,1,0)
smc$Teacher_functionnaire_2 <- 
  ifelse(smc$Teacher_2_type==1,1,0)
smc$Teacher_functionnaire_3 <- 
  ifelse(smc$Teacher_3_type==1,1,0)
smc$Teacher_functionnaire_4 <- 
  ifelse(smc$Teacher_4_type==1,1,0)
smc$Teacher_functionnaire_5 <- 
  ifelse(smc$Teacher_5_type==1,1,0)
smc$Teacher_functionnaire_6 <- 
  ifelse(smc$Teacher_6_type==1,1,0)
smc$Teacher_functionnaire_7 <- 
  ifelse(smc$Teacher_7_type==1,1,0)
smc$Teacher_functionnaire_8 <- 
  ifelse(smc$Teacher_8_type==1,1,0)
smc$Teacher_functionnaire_9 <- 
  ifelse(smc$Teacher_9_type==1,1,0)
smc$Teacher_functionnaire_10 <- 
  ifelse(smc$Teacher_10_type==1,1,0)
smc$Teacher_functionnaire_11 <- 
  ifelse(smc$Teacher_11_type==1,1,0)
smc$Teacher_functionnaire_12 <- 
  ifelse(smc$Teacher_12_type==1,1,0)
smc$Teacher_functionnaire_13 <- 
  ifelse(smc$Teacher_13_type==1,1,0)
smc$Teacher_functionnaire_14 <- 
  ifelse(smc$Teacher_14_type==1,1,0)
smc$Teacher_functionnaire_15 <- 
  ifelse(smc$Teacher_15_type==1,1,0)
smc$Teacher_functionnaire_16 <- 
  ifelse(smc$Teacher_16_type==1,1,0)
smc$Teacher_functionnaire_17 <- 
  ifelse(smc$Teacher_17_type==1,1,0)

smc$Teacher_functionnaire_1[is.na(smc$Teacher_functionnaire_1)] <- 0
smc$Teacher_functionnaire_2[is.na(smc$Teacher_functionnaire_2)] <- 0
smc$Teacher_functionnaire_3[is.na(smc$Teacher_functionnaire_3)] <- 0
smc$Teacher_functionnaire_4[is.na(smc$Teacher_functionnaire_4)] <- 0
smc$Teacher_functionnaire_5[is.na(smc$Teacher_functionnaire_5)] <- 0
smc$Teacher_functionnaire_6[is.na(smc$Teacher_functionnaire_6)] <- 0
smc$Teacher_functionnaire_7[is.na(smc$Teacher_functionnaire_7)] <- 0
smc$Teacher_functionnaire_8[is.na(smc$Teacher_functionnaire_8)] <- 0
smc$Teacher_functionnaire_9[is.na(smc$Teacher_functionnaire_9)] <- 0
smc$Teacher_functionnaire_10[is.na(smc$Teacher_functionnaire_10)] <- 0
smc$Teacher_functionnaire_11[is.na(smc$Teacher_functionnaire_11)] <- 0
smc$Teacher_functionnaire_12[is.na(smc$Teacher_functionnaire_12)] <- 0
smc$Teacher_functionnaire_13[is.na(smc$Teacher_functionnaire_13)] <- 0
smc$Teacher_functionnaire_14[is.na(smc$Teacher_functionnaire_14)] <- 0
smc$Teacher_functionnaire_15[is.na(smc$Teacher_functionnaire_15)] <- 0
smc$Teacher_functionnaire_16[is.na(smc$Teacher_functionnaire_16)] <- 0
smc$Teacher_functionnaire_17[is.na(smc$Teacher_functionnaire_17)] <- 0

smc$Number_Teacher_functionnaire <- c(smc$Teacher_functionnaire_1+
                                        smc$Teacher_functionnaire_2+
                                        smc$Teacher_functionnaire_3+
                                        smc$Teacher_functionnaire_4+
                                        smc$Teacher_functionnaire_5+
                                        smc$Teacher_functionnaire_6+
                                        smc$Teacher_functionnaire_7+
                                        smc$Teacher_functionnaire_8+
                                        smc$Teacher_functionnaire_9+
                                        smc$Teacher_functionnaire_10+
                                        smc$Teacher_functionnaire_11+
                                        smc$Teacher_functionnaire_12+
                                        smc$Teacher_functionnaire_13+
                                        smc$Teacher_functionnaire_14+
                                        smc$Teacher_functionnaire_15+
                                        smc$Teacher_functionnaire_16+
                                        smc$Teacher_functionnaire_17)

smc$Teacher_functionnaire_rate <- c(smc$Number_Teacher_functionnaire/
                                      smc$Teacher_number)

#percentage of fram teachers

smc$Teacher_fram_1 <- 
  ifelse(smc$Teacher_1_type==3|
           smc$Teacher_1_type==4,1,0)
smc$Teacher_fram_2 <- 
  ifelse(smc$Teacher_2_type==3|
           smc$Teacher_2_type==4,1,0)
smc$Teacher_fram_3 <- 
  ifelse(smc$Teacher_3_type==3|
           smc$Teacher_3_type==4,1,0)
smc$Teacher_fram_4 <- 
  ifelse(smc$Teacher_4_type==3|
           smc$Teacher_4_type==4,1,0)
smc$Teacher_fram_5 <- 
  ifelse(smc$Teacher_5_type==3|
           smc$Teacher_5_type==4,1,0)
smc$Teacher_fram_6 <- 
  ifelse(smc$Teacher_6_type==3|
           smc$Teacher_6_type==4,1,0)
smc$Teacher_fram_7 <- 
  ifelse(smc$Teacher_7_type==3|
           smc$Teacher_7_type==4,1,0)
smc$Teacher_fram_8 <- 
  ifelse(smc$Teacher_8_type==3|
           smc$Teacher_8_type==4,1,0)
smc$Teacher_fram_9 <- 
  ifelse(smc$Teacher_9_type==3|
           smc$Teacher_9_type==4,1,0)
smc$Teacher_fram_10 <- 
  ifelse(smc$Teacher_10_type==3|
           smc$Teacher_10_type==4,1,0)
smc$Teacher_fram_11 <- 
  ifelse(smc$Teacher_11_type==3|
           smc$Teacher_11_type==4,1,0)
smc$Teacher_fram_12 <- 
  ifelse(smc$Teacher_12_type==3|
           smc$Teacher_12_type==4,1,0)
smc$Teacher_fram_13 <- 
  ifelse(smc$Teacher_13_type==3|
           smc$Teacher_13_type==4,1,0)
smc$Teacher_fram_14 <- 
  ifelse(smc$Teacher_14_type==3|
           smc$Teacher_14_type==4,1,0)
smc$Teacher_fram_15 <- 
  ifelse(smc$Teacher_15_type==3|
           smc$Teacher_15_type==4,1,0)
smc$Teacher_fram_16 <- 
  ifelse(smc$Teacher_16_type==3|
           smc$Teacher_16_type==4,1,0)
smc$Teacher_fram_17 <- 
  ifelse(smc$Teacher_17_type==3|
           smc$Teacher_17_type==4,1,0)

smc$Teacher_fram_1[is.na(smc$Teacher_fram_1)] <- 0
smc$Teacher_fram_2[is.na(smc$Teacher_fram_2)] <- 0
smc$Teacher_fram_3[is.na(smc$Teacher_fram_3)] <- 0
smc$Teacher_fram_4[is.na(smc$Teacher_fram_4)] <- 0
smc$Teacher_fram_5[is.na(smc$Teacher_fram_5)] <- 0
smc$Teacher_fram_6[is.na(smc$Teacher_fram_6)] <- 0
smc$Teacher_fram_7[is.na(smc$Teacher_fram_7)] <- 0
smc$Teacher_fram_8[is.na(smc$Teacher_fram_8)] <- 0
smc$Teacher_fram_9[is.na(smc$Teacher_fram_9)] <- 0
smc$Teacher_fram_10[is.na(smc$Teacher_fram_10)] <- 0
smc$Teacher_fram_11[is.na(smc$Teacher_fram_11)] <- 0
smc$Teacher_fram_12[is.na(smc$Teacher_fram_12)] <- 0
smc$Teacher_fram_13[is.na(smc$Teacher_fram_13)] <- 0
smc$Teacher_fram_14[is.na(smc$Teacher_fram_14)] <- 0
smc$Teacher_fram_15[is.na(smc$Teacher_fram_15)] <- 0
smc$Teacher_fram_16[is.na(smc$Teacher_fram_16)] <- 0
smc$Teacher_fram_17[is.na(smc$Teacher_fram_17)] <- 0

smc$Number_Teacher_fram <- c(smc$Teacher_fram_1+
                               smc$Teacher_fram_2+
                               smc$Teacher_fram_3+
                               smc$Teacher_fram_4+
                               smc$Teacher_fram_5+
                               smc$Teacher_fram_6+
                               smc$Teacher_fram_7+
                               smc$Teacher_fram_8+
                               smc$Teacher_fram_9+
                               smc$Teacher_fram_10+
                               smc$Teacher_fram_11+
                               smc$Teacher_fram_12+
                               smc$Teacher_fram_13+
                               smc$Teacher_fram_14+
                               smc$Teacher_fram_15+
                               smc$Teacher_fram_16+
                               smc$Teacher_fram_17)

smc$Teacher_fram_rate <- c(smc$Number_Teacher_fram/
                             smc$Teacher_number)

#activities related to extra-curricular remedial activities

smc$activities_planned_quality_learning_1 <- grepl("1", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_1 <- 
  ifelse(smc$activities_planned_quality_learning_1=="TRUE", 1,0)

smc$activities_planned_quality_learning_2 <- grepl("2", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_2 <- 
  ifelse(smc$activities_planned_quality_learning_2=="TRUE", 1,0)

smc$activities_planned_quality_learning_3 <- grepl("3", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_3 <- 
  ifelse(smc$activities_planned_quality_learning_3=="TRUE", 1,0)

smc$activities_planned_quality_learning_4 <- grepl("4", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_4 <- 
  ifelse(smc$activities_planned_quality_learning_4=="TRUE", 1,0)

smc$activities_planned_quality_learning_6 <- grepl("6", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_6 <- 
  ifelse(smc$activities_planned_quality_learning_6=="TRUE", 1,0)

smc$activities_planned_quality_learning_8 <- grepl("8", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_8 <- 
  ifelse(smc$activities_planned_quality_learning_8=="TRUE", 1,0)

smc$activities_planned_quality_learning_15 <- grepl("15", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_15 <- 
  ifelse(smc$activities_planned_quality_learning_15=="TRUE", 1,0)

smc$activities_planned_quality_learning_19 <- grepl("19", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_19 <- 
  ifelse(smc$activities_planned_quality_learning_19=="TRUE", 1,0)

smc$activities_planned_quality_learning_20 <- grepl("20", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_20 <- 
  ifelse(smc$activities_planned_quality_learning_20=="TRUE", 1,0)

smc$activities_planned_quality_learning_22 <- grepl("22", smc$activities_planned_quality_learning)
smc$activities_planned_quality_learning_22 <- 
  ifelse(smc$activities_planned_quality_learning_22=="TRUE", 1,0)

#

smc$vr_quality_learning_1 <- 
  ifelse(smc$activities_planned_quality_learning_1==1, 
         smc$voluntary_resource_moblized_for_activity_1_r,0)

smc$vr_quality_learning_2 <- 
  ifelse(smc$activities_planned_quality_learning_2==1, 
         smc$voluntary_resource_moblized_for_activity_2_r,0)

smc$vr_quality_learning_3 <- 
  ifelse(smc$activities_planned_quality_learning_3==1, 
         smc$voluntary_resource_moblized_for_activity_3_r,0)

smc$vr_quality_learning_4 <- 
  ifelse(smc$activities_planned_quality_learning_4==1, 
         smc$voluntary_resource_moblized_for_activity_4_r,0)

smc$vr_quality_learning_6 <- 
  ifelse(smc$activities_planned_quality_learning_6==1, 
         smc$voluntary_resource_moblized_for_activity_6_r,0)

smc$vr_quality_learning_8 <- 
  ifelse(smc$activities_planned_quality_learning_8==1, 
         smc$voluntary_resource_moblized_for_activity_8_r,0)

smc$vr_quality_learning_15 <- 
  ifelse(smc$activities_planned_quality_learning_15==1, 
         smc$voluntary_resource_moblized_for_activity_15_r,0)

smc$vr_quality_learning_19 <- 
  ifelse(smc$activities_planned_quality_learning_19==1, 
         smc$voluntary_resource_moblized_for_activity_19_r,0)

smc$vr_quality_learning_20 <- 
  ifelse(smc$activities_planned_quality_learning_20==1, 
         smc$voluntary_resource_moblized_for_activity_20_r,0)

smc$vr_quality_learning_22 <- 
  ifelse(smc$activities_planned_quality_learning_22==1, 
         smc$voluntary_resource_moblized_for_activity_22_r,0)

#

smc$cp_quality_learning_1 <- 
  ifelse(smc$activities_planned_quality_learning_1==1, 
         smc$Contribution_physique_amount_1,0)

smc$cp_quality_learning_2 <- 
  ifelse(smc$activities_planned_quality_learning_2==1, 
         smc$Contribution_physique_amount_2,0)

smc$cp_quality_learning_3 <- 
  ifelse(smc$activities_planned_quality_learning_3==1, 
         smc$Contribution_physique_amount_3,0)

smc$cp_quality_learning_4 <- 
  ifelse(smc$activities_planned_quality_learning_4==1, 
         smc$Contribution_physique_amount_4,0)

smc$cp_quality_learning_6 <- 
  ifelse(smc$activities_planned_quality_learning_6==1, 
         smc$Contribution_physique_amount_6,0)

smc$cp_quality_learning_8 <- 
  ifelse(smc$activities_planned_quality_learning_8==1, 
         smc$Contribution_physique_amount_8,0)

smc$cp_quality_learning_15 <- 
  ifelse(smc$activities_planned_quality_learning_15==1, 
         smc$Contribution_physique_amount_15,0)

smc$cp_quality_learning_19 <- 
  ifelse(smc$activities_planned_quality_learning_19==1, 
         smc$Contribution_physique_amount_19,0)

smc$cp_quality_learning_20 <- 
  ifelse(smc$activities_planned_quality_learning_20==1, 
         smc$Contribution_physique_amount_20,0)

smc$cp_quality_learning_22 <- 
  ifelse(smc$activities_planned_quality_learning_22==1, 
         smc$Contribution_physique_amount_22,0)

#

smc$cm_quality_learning_1 <- 
  ifelse(smc$activities_planned_quality_learning_1==1, 
         smc$Contribution_materielle_1,0)

smc$cm_quality_learning_2 <- 
  ifelse(smc$activities_planned_quality_learning_2==1, 
         smc$Contribution_materielle_2,0)

smc$cm_quality_learning_3 <- 
  ifelse(smc$activities_planned_quality_learning_3==1, 
         smc$Contribution_materielle_3,0)

smc$cm_quality_learning_4 <- 
  ifelse(smc$activities_planned_quality_learning_4==1, 
         smc$Contribution_materielle_4,0)

smc$cm_quality_learning_6 <- 
  ifelse(smc$activities_planned_quality_learning_6==1, 
         smc$Contribution_materielle_6,0)

smc$cm_quality_learning_8 <- 
  ifelse(smc$activities_planned_quality_learning_8==1, 
         smc$Contribution_materielle_8,0)

smc$cm_quality_learning_15 <- 
  ifelse(smc$activities_planned_quality_learning_15==1, 
         smc$Contribution_materielle_15,0)

smc$cm_quality_learning_19 <- 
  ifelse(smc$activities_planned_quality_learning_19==1, 
         smc$Contribution_materielle_19,0)

smc$cm_quality_learning_20 <- 
  ifelse(smc$activities_planned_quality_learning_20==1, 
         smc$Contribution_materielle_20,0)

smc$cm_quality_learning_22 <- 
  ifelse(smc$activities_planned_quality_learning_22==1, 
         smc$Contribution_materielle_22,0)

#

smc$er_quality_learning_1 <- 
  ifelse(smc$activities_planned_quality_learning_1==1, 
         smc$amount_used_external_resource_1,0)

smc$er_quality_learning_2 <- 
  ifelse(smc$activities_planned_quality_learning_2==1, 
         smc$amount_used_external_resource_2,0)

smc$er_quality_learning_3 <- 
  ifelse(smc$activities_planned_quality_learning_3==1, 
         smc$amount_used_external_resource_3,0)

smc$er_quality_learning_4 <- 
  ifelse(smc$activities_planned_quality_learning_4==1, 
         smc$amount_used_external_resource_4,0)

smc$er_quality_learning_6 <- 
  ifelse(smc$activities_planned_quality_learning_6==1, 
         smc$amount_used_external_resource_6,0)

smc$er_quality_learning_8 <- 
  ifelse(smc$activities_planned_quality_learning_8==1, 
         smc$amount_used_external_resource_8,0)

smc$er_quality_learning_15 <- 
  ifelse(smc$activities_planned_quality_learning_15==1, 
         smc$amount_used_external_resource_15,0)

smc$er_quality_learning_19 <- 
  ifelse(smc$activities_planned_quality_learning_19==1, 
         smc$amount_used_external_resource_19,0)

smc$er_quality_learning_20 <- 
  ifelse(smc$activities_planned_quality_learning_20==1, 
         smc$amount_used_external_resource_20,0)

smc$er_quality_learning_22 <- 
  ifelse(smc$activities_planned_quality_learning_22==1, 
         smc$amount_used_external_resource_22,0)

#

smc$vc_quality_learning_total <-
  c(smc$vr_quality_learning_1+
      smc$vr_quality_learning_2+
      smc$vr_quality_learning_3+
      smc$vr_quality_learning_4+
      smc$vr_quality_learning_6+
      smc$vr_quality_learning_8+
      smc$vr_quality_learning_15+
      smc$vr_quality_learning_19+
      smc$vr_quality_learning_20+
      smc$vr_quality_learning_22)

smc$cp_quality_learning_total <-
  c(smc$cp_quality_learning_1+
      smc$cp_quality_learning_2+
      smc$cp_quality_learning_3+
      smc$cp_quality_learning_4+
      smc$cp_quality_learning_6+
      smc$cp_quality_learning_8+
      smc$cp_quality_learning_15+
      smc$cp_quality_learning_19+
      smc$cp_quality_learning_20+
      smc$cp_quality_learning_22)

smc$cm_quality_learning_total <-
  c(smc$cm_quality_learning_1+
      smc$cm_quality_learning_2+
      smc$cm_quality_learning_3+
      smc$cm_quality_learning_4+
      smc$cm_quality_learning_6+
      smc$cm_quality_learning_8+
      smc$cm_quality_learning_15+
      smc$cm_quality_learning_19+
      smc$cm_quality_learning_20+
      smc$cm_quality_learning_22)

smc$mobilized_resource_quality_learning_total <-
  c(smc$vc_quality_learning_total+
      smc$cp_quality_learning_total+
      smc$cm_quality_learning_total)

smc$er_quality_learning_total <-
  c(smc$er_quality_learning_1+
      smc$er_quality_learning_2+
      smc$er_quality_learning_3+
      smc$er_quality_learning_4+
      smc$er_quality_learning_6+
      smc$er_quality_learning_8+
      smc$er_quality_learning_15+
      smc$er_quality_learning_19+
      smc$er_quality_learning_20+
      smc$er_quality_learning_22)

#

smc$Caisse_Ecole_Montant[is.na(smc$Caisse_Ecole_Montant)]<-0
smc$SAE_Montant[is.na(smc$SAE_Montant)]<-0
smc$Autres_montant_external[is.na(smc$Autres_montant_external)]<-0

#

smc$voluntary_resource_moblized_for_activity_total_r_exc_19 <-
  c(smc$voluntary_resource_moblized_for_activity_total_r-
      smc$Contribution_physique_amount_19)

smc$voluntary_resource_moblized_for_activity_total_r_exc_19_r<-
  c(smc$voluntary_resource_moblized_for_activity_total_r_exc_19/1000)

smc$voluntary_resource_moblized_for_activity_total_r_exc_19_r2 <- 
  c(smc$voluntary_resource_moblized_for_activity_total_r_exc_19_r/
      smc$Number_total_Students_201819_r)

#

smc$mobilized_resource_quality_learning_total_exc_19 <-
  c(smc$mobilized_resource_quality_learning_total-
      smc$Contribution_physique_amount_19)

#coding covariates for athey and imbens

smc$amont_used_voluntary_ressource_total_b_2 <- 
  smc$amont_used_voluntary_ressource_total_b-
  mean(smc$amont_used_voluntary_ressource_total_b)

smc$amont_used_voluntary_ressource_total_b_2_t <-
  smc$amont_used_voluntary_ressource_total_b_2*
  smc$treatment

smc$amont_used_external_ressource_total_b_2 <- 
  smc$amont_used_external_ressource_total_b-
  mean(smc$amont_used_external_ressource_total_b)

smc$amont_used_external_ressource_total_b_2_t <-
  smc$amont_used_external_ressource_total_b_2*
  smc$treatment

smc$Number_total_Students_b_2 <- 
  smc$Number_total_Students_b-
  mean(smc$Number_total_Students_b)

smc$Number_total_Students_b_2_t <-
  smc$Number_total_Students_b_2*
  smc$treatment

smc$Teacher_status_HM_fonctionnaire_2 <- 
  smc$Teacher_status_HM_fonctionnaire-
  mean(smc$Teacher_status_HM_fonctionnaire)

smc$Teacher_status_HM_fonctionnaire_2_t <-
  smc$Teacher_status_HM_fonctionnaire_2*
  smc$treatment

smc$Year_HM_experience_2 <- 
  smc$Year_HM_experience-
  mean(smc$Year_HM_experience)

smc$Year_HM_experience_2_t <-
  smc$Year_HM_experience_2*
  smc$treatment

smc$HM_charg..de.cours_2 <- 
  smc$HM_charg..de.cours-
  mean(smc$HM_charg..de.cours)

smc$HM_charg..de.cours_2_t <-
  smc$HM_charg..de.cours_2*
  smc$treatment

smc$Teacher_number_2 <- 
  smc$Teacher_number-
  mean(smc$Teacher_number)

smc$Teacher_number_2_t <-
  smc$Teacher_number_2*
  smc$treatment

smc$Teacher_functionnaire_rate_2 <- 
  smc$Teacher_functionnaire_rate-
  mean(smc$Teacher_functionnaire_rate)

smc$Teacher_functionnaire_rate_2_t <-
  smc$Teacher_functionnaire_rate_2*
  smc$treatment

#

smc_t <- subset(smc, treatment==1)
smc_c <- subset(smc, treatment==0)

mean(smc_c$voluntary_resource_moblized_for_activity_total_r_exc_19_r)
mean(smc_c$voluntary_resource_moblized_for_activity_total_r_exc_19_r2)

#column 1

reg_mobilized_resource_total <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_total, conf.int = F)
reg_r2 <- reg_r[2,]

#column 2

reg_mobilized_resource_total_pers <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r2 ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_total_pers, conf.int = F)
reg_r2 <- reg_r[2,]

#column 3

reg_mobilized_resource_ra_vc <- 
  lm_robust(mobilized_resource_quality_learning_total_exc_19~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_ra_vc, conf.int = F)
reg_r2 <- reg_r[2,]

#column 4

reg_mobilized_resource_ra_er <- 
  lm_robust(er_quality_learning_total~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_ra_er, conf.int = F)
reg_r2 <- reg_r[2,]

#Table C-8 Appendix 5#####

#robustness check

smc_robust <- subset(smc,
                     !(smc$School_Code==323050012)&
                       !(smc$School_Code==323060004)&
                       !(smc$School_Code==323030014)&
                       !(smc$School_Code==323070003)&
                       !(smc$School_Code==304090005)&
                       !(smc$School_Code==323070006)&
                       !(smc$School_Code==323050026)&
                       !(smc$School_Code==304010010)&
                       !(smc$School_Code==306220014)&
                       !(smc$School_Code==306180006)&
                       !(smc$School_Code==306040025)&
                       !(smc$School_Code==306110006)&
                       !(smc$School_Code==306010012)&
                       !(smc$School_Code==306040010)&
                       !(smc$School_Code==306050009)&
                       !(smc$School_Code==304010036)&
                       !(smc$School_Code==304010008)&
                       !(smc$School_Code==304010041)&
                       !(smc$School_Code==323070012)&
                       !(smc$School_Code==306190003)&
                       !(smc$School_Code==308060009))

smc_exc <- subset(smc,
                  (smc$School_Code==323050012)|
                    (smc$School_Code==323060004)|
                    (smc$School_Code==323030014)|
                    (smc$School_Code==323070003)|
                    (smc$School_Code==304090005)|
                    (smc$School_Code==323070006)|
                    (smc$School_Code==323050026)|
                    (smc$School_Code==304010010)|
                    (smc$School_Code==306220014)|
                    (smc$School_Code==306180006)|
                    (smc$School_Code==306040025)|
                    (smc$School_Code==306110006)|
                    (smc$School_Code==306010012)|
                    (smc$School_Code==306040010)|
                    (smc$School_Code==306050009)|
                    (smc$School_Code==304010036)|
                    (smc$School_Code==304010008)|
                    (smc$School_Code==304010041)|
                    (smc$School_Code==323070012)|
                    (smc$School_Code==306190003)|
                    (smc$School_Code==308060009))

smc_exc <- subset(smc_exc,
                  select=c(CISCO,
                           CATEGORIE_COMMUNE,
                           size))

write.csv(smc_exc,
          "smc_exc.csv",
          row.names = F)

smc_robust2 <- subset(smc,
                      !((smc$CISCO=="AMBATOFINANDRAHANA")&
                          (smc$CATEGORIE_COMMUNE=="RUR")&
                          (smc$size==0))&
                        !((smc$CISCO=="AMBATOFINANDRAHANA")&
                            (smc$CATEGORIE_COMMUNE=="URB")&
                            (smc$size==1))&
                        !((smc$CISCO=="AMBATOFINANDRAHANA")&
                            (smc$CATEGORIE_COMMUNE=="URB")&
                            (smc$size==0))&
                        !((smc$CISCO=="AMBOSITRA")&
                            (smc$CATEGORIE_COMMUNE=="RUR")&
                            (smc$size==1))&
                        !((smc$CISCO=="AMBOSITRA")&
                            (smc$CATEGORIE_COMMUNE=="RUR")&
                            (smc$size==0))&
                        !((smc$CISCO=="FANDRIANA")&
                            (smc$CATEGORIE_COMMUNE=="RUR")&
                            (smc$size==1))&
                        !((smc$CISCO=="MANANDRIANA")&
                            (smc$CATEGORIE_COMMUNE=="RUR")&
                            (smc$size==1))&
                        !((smc$CISCO=="MANANDRIANA")&
                            (smc$CATEGORIE_COMMUNE=="RUR")&
                            (smc$size==0)))

#

smc_robust_t <- subset(smc_robust, treatment==1)
smc_robust_c <- subset(smc_robust, treatment==0)

mean(smc_robust_c$voluntary_resource_moblized_for_activity_total_r_exc_19_r)
mean(smc_robust_c$voluntary_resource_moblized_for_activity_total_r_exc_19_r2)

#Panel A: excluding 21 schools
#column 1

reg_mobilized_resource_total <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_total, conf.int = F)
reg_r2 <- reg_r[2,]

#column 2

reg_mobilized_resource_total_pers <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r2 ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_total_pers, conf.int = F)
reg_r2 <- reg_r[2,]

#column 3

reg_mobilized_resource_ra_vc <- 
  lm_robust(mobilized_resource_quality_learning_total_exc_19~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_ra_vc, conf.int = F)
reg_r2 <- reg_r[2,]

#column 4

reg_mobilized_resource_ra_er <- 
  lm_robust(er_quality_learning_total~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_ra_er, conf.int = F)
reg_r2 <- reg_r[2,]

#Panel B: excluding strata of 21 schools
#column 1

reg_mobilized_resource_total <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b+
              amont_used_external_ressource_total_b+
              Number_total_Students_b+
              Teacher_status_HM_fonctionnaire+
              Year_HM_experience+
              HM_charg..de.cours+
              Teacher_number+
              Teacher_functionnaire_rate,
            data=smc_robust2, se_type = "stata")

reg_mobilized_resource_total <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust2, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_total, conf.int = F)
reg_r2 <- reg_r[2,]

#column 2

reg_mobilized_resource_total_pers <- 
  lm_robust(voluntary_resource_moblized_for_activity_total_r_exc_19_r2 ~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust2, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_total_pers, conf.int = F)
reg_r2 <- reg_r[2,]

#column 3

reg_mobilized_resource_ra_vc <- 
  lm_robust(mobilized_resource_quality_learning_total_exc_19~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust2, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_ra_vc, conf.int = F)
reg_r2 <- reg_r[2,]

#column 4

reg_mobilized_resource_ra_er <- 
  lm_robust(er_quality_learning_total~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b+
              amont_used_external_ressource_total_b+
              Number_total_Students_b+
              Teacher_status_HM_fonctionnaire+
              Year_HM_experience+
              HM_charg..de.cours+
              Teacher_number+
              Teacher_functionnaire_rate,
            data=smc_robust2, se_type = "stata")

reg_mobilized_resource_ra_er <- 
  lm_robust(er_quality_learning_total~
              treatment+
              CISCO*
              CATEGORIE_COMMUNE*
              size+
              amont_used_voluntary_ressource_total_b_2+
              amont_used_voluntary_ressource_total_b_2_t+
              amont_used_external_ressource_total_b_2+
              amont_used_external_ressource_total_b_2_t+
              Number_total_Students_b_2+
              Number_total_Students_b_2_t+
              Teacher_status_HM_fonctionnaire_2+
              Teacher_status_HM_fonctionnaire_2_t+
              Year_HM_experience_2+
              Year_HM_experience_2_t+
              HM_charg..de.cours_2+
              HM_charg..de.cours_2_t+
              Teacher_number_2+
              Teacher_number_2_t+
              Teacher_functionnaire_rate_2+
              Teacher_functionnaire_rate_2_t,
            data=smc_robust2, se_type = "stata")

reg_r <- tidy(reg_mobilized_resource_ra_er, conf.int = F)
reg_r2 <- reg_r[2,]
