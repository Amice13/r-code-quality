# A3_Menipulation_Check.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2019-9-24 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("dplyr")
require("tidyr")
require("coefplot")
require("ggplot2")
theme_set(theme_classic(base_size = 12))


# attention check
# which media reported the news that you read?
## Table A.9
aggregate(Q16_1 ~ Control + TV, exp, FUN = table)

# whether the news commentator praise Prime Minister Abe's diplomacy or not
## Table A.10
aggregate(Q16_2 ~ Control + TV, exp, FUN = table)


# split sample
## succeed a manipulation check
exp_correct <- filter(exp, 
                      (Group1 == 1 & Q16_1 == 1 & Q16_2 == 2) | 
                      (Group2 == 1 & Q16_1 == 2 & Q16_2 == 2) | 
                      (Group3 == 1 & Q16_1 == 3 & Q16_2 == 2) | 
                      (Group4 == 1 & Q16_1 == 1 & Q16_2 == 1) | 
                      (Group5 == 1 & Q16_1 == 2 & Q16_2 == 1) | 
                      (Group6 == 1 & Q16_1 == 3 & Q16_2 == 1))
## fail a manipulation check
exp_fail <- filter(exp, !nid %in% exp_correct$nid)


# distrust level for report
## succeed manipulation checks
## Table A.11a
## EX
tableA_11_1 <- exp_correct[exp_correct$TV == "EX",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_11_1)[2:3] <- c("Control", "Pro-government bias")
tableA_11_1
## NHK
tableA_11_2 <- exp_correct[exp_correct$TV == "NHK",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_11_2)[2:3] <- c("Control", "Pro-government bias")
tableA_11_2
## NTV
tableA_11_3 <- exp_correct[exp_correct$TV == "NTV",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_8_3)[2:3] <- c("Control", "Pro-government bias")
tableA_11_3

## fail manipulation checks
## Table A.11b
## EX
tableA_11_4 <- exp_fail[exp_fail$TV == "EX",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_11_4)[2:3] <- c("Control", "Pro-government bias")
tableA_11_4
## NHK
tableA_11_5 <- exp_fail[exp_fail$TV == "NHK",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_11_5)[2:3] <- c("Control", "Pro-government bias")
tableA_11_5
## NTV
tableA_11_6 <- exp_fail[exp_fail$TV == "NTV",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_11_6)[2:3] <- c("Control", "Pro-government bias")
tableA_11_6


# critical attitude toward Abe's diplomacy
## Figure A.6 & A.7
## NHK
## succeed a manipulation check
reg1_1 <- lm(Q13 ~ Group4 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, 
             exp_correct[exp_correct$TV == "NHK",])
summary(reg1_1)
reg1_1_plot <- coefplot::coefplot(
  reg1_1, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "NHK", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", Q2 = "Age", College = "College Education", 
               Q6_1 = "Political Interest", Q17 = "Frequency of Viewing TV", 
               Group4 = "Pro-government bias", Q10 = "Distrust for the Report", 
               `Group4:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = paste0(rev(round(reg1_1$coefficients[-1], 2)), "\n(", 
                               rev(round(coef(summary(reg1_1))[, 2][-1], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-2.5, 2.5) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg1_1_plot)
## fail a manipulation check
reg1_2 <- lm(Q13 ~ Group4 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, 
             exp_fail[exp_fail$TV == "NHK",])
summary(reg1_2)
reg1_2_plot <- coefplot::coefplot(
  reg1_2, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "NHK", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", Q2 = "Age", College = "College Education", 
               Q6_1 = "Political Interest", Q17 = "Frequency of Viewing TV", 
               Group4 = "Pro-government bias", Q10 = "Distrust for the Report", 
               `Group4:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = paste0(rev(round(reg1_2$coefficients[-1], 2)), "\n(", 
                               rev(round(coef(summary(reg1_2))[, 2][-1], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-2.5, 2.5) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg1_2_plot)

## EX
## succeed a manipulation check
reg2_1 <- lm(Q13 ~ Group5 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, 
             exp_correct[exp_correct$TV == "EX",])
summary(reg2_1)
reg2_1_plot <- coefplot::coefplot(
  reg2_1, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "EX", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", Q2 = "Age", College = "College Education", 
               Q6_1 = "Political Interest", Q17 = "Frequency of Viewing TV", 
               Group5 = "Pro-government bias", Q10 = "Distrust for the Report", 
               `Group5:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = paste0(rev(round(reg2_1$coefficients[-1], 2)), "\n(", 
                               rev(round(coef(summary(reg2_1))[, 2][-1], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-2.5, 2.5) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg2_1_plot)
## fail a manipulation check
reg2_2 <- lm(Q13 ~ Group5 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, 
             exp_fail[exp_fail$TV == "EX",])
summary(reg2_2)
reg2_2_plot <- coefplot::coefplot(
  reg2_2, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "EX", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", Q2 = "Age", College = "College Education", 
               Q6_1 = "Political Interest", Q17 = "Frequency of Viewing TV", 
               Group5 = "Pro-government bias", Q10 = "Distrust for the Report", 
               `Group5:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = paste0(rev(round(reg2_2$coefficients[-1], 2)), "\n(", 
                               rev(round(coef(summary(reg2_2))[, 2][-1], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-2.5, 2.5) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg2_2_plot)

## NTV
## succeed a manipulation check
reg3_1 <- lm(Q13 ~ Group6 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, 
             exp_correct[exp_correct$TV == "NTV",])
summary(reg3_1)
reg3_1_plot <- coefplot::coefplot(
  reg3_1, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "NTV", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", Q2 = "Age", College = "College Education", 
               Q6_1 = "Political Interest", Q17 = "Frequency of Viewing TV", 
               Group6 = "Pro-government bias", Q10 = "Distrust for the Report", 
               `Group6:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = paste0(rev(round(reg3_1$coefficients[-1], 2)), "\n(", 
                               rev(round(coef(summary(reg3_1))[, 2][-1], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-2.5, 2.5) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg3_1_plot)
## fail a manipulation check
reg3_2 <- lm(Q13 ~ Group6 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, 
             exp_fail[exp_fail$TV == "NTV",])
summary(reg3_2)
reg3_2_plot <- coefplot::coefplot(
  reg3_2, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "NTV", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", Q2 = "Age", College = "College Education", 
               Q6_1 = "Political Interest", Q17 = "Frequency of Viewing TV", 
               Group6 = "Pro-government bias", Q10 = "Distrust for the Report", 
               `Group6:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = paste0(rev(round(reg3_2$coefficients[-1], 2)), "\n(", 
                               rev(round(coef(summary(reg3_2))[, 2][-1], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-2.5, 2.5) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg3_2_plot)
