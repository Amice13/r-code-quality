# 03_Hypothesis1_1.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2019-9-6 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("dplyr")
require("tidyr")
require("ggplot2")
require("ggpubr")
require("coefplot")
require("interplot")
theme_set(theme_classic(base_size = 12))


# neutrality of TV
# Table 1a
## EX
table1_1 <- exp[exp$TV == "EX",] %>% 
  dplyr::select(Q16_4, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q16_4 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q16_4 ~ "t.test")) %>% 
  as_tibble()
colnames(table1_1)[2:3] <- c("Control", "Pro-government bias")
table1_1
## NHK
table1_2 <- exp[exp$TV == "NHK",] %>% 
  dplyr::select(Q16_4, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q16_4 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q16_4 ~ "t.test")) %>% 
  as_tibble()
colnames(table1_2)[2:3] <- c("Control", "Pro-government bias")
table1_2
## EX
table1_3 <- exp[exp$TV == "NTV",] %>% 
  dplyr::select(Q16_4, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q16_4 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q16_4 ~ "t.test")) %>% 
  as_tibble()
colnames(table1_3)[2:3] <- c("Control", "Pro-government bias")
table1_3


# distrust level for report
# Table 1b
## EX
table1_4 <- exp[exp$TV == "EX",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(table1_4)[2:3] <- c("Control", "Pro-government bias")
table1_4
## NHK
table1_5 <- exp[exp$TV == "NHK",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(table1_5)[2:3] <- c("Control", "Pro-government bias")
table1_5
## EX
table1_6 <- exp[exp$TV == "NTV",] %>% 
  dplyr::select(Q10, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q10 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q10 ~ "t.test")) %>% 
  as_tibble()
colnames(table1_6)[2:3] <- c("Control", "Pro-government bias")
table1_6


# distrust level for overall media
# Table A.8
## EX
tableA_8_1 <- exp[exp$TV == "EX",] %>% 
  dplyr::select(Q11_4, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q11_4 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q11_4 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_8_1)[2:3] <- c("Control", "Pro-government bias")
tableA_8_1
## NHK
tableA_8_2 <- exp[exp$TV == "NHK",] %>% 
  dplyr::select(Q11_4, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q11_4 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q11_4 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_8_2)[2:3] <- c("Control", "Pro-government bias")
tableA_8_2
## NTV
tableA_8_3 <- exp[exp$TV == "NTV",] %>% 
  dplyr::select(Q11_4, Control) %>% 
  na.omit() %>%
  tbl_summary(by = Control, missing = "no", 
              type = list(Q11_4 ~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p(list(Q11_4 ~ "t.test")) %>% 
  as_tibble()
colnames(tableA_8_3)[2:3] <- c("Control", "Pro-government bias")
tableA_8_3


# Hypothesis 1-1
# neutrality of TV
## NHK
reg1 <- lm(Q10 ~ Group4 * Q16_4 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_1, exp[exp$TV == "NHK",])
summary(reg1)

## EX
reg2 <- lm(Q10 ~ Group5 * Q16_4 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_3, exp[exp$TV == "EX",])
summary(reg2)

## NTV
reg3 <- lm(Q10 ~ Group6 * Q16_4 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_2, exp[exp$TV == "NTV",])
summary(reg3)

## Figure 2a
reg1_2_3 <- data.frame(
  TV = c(rep("NHK", 3), rep("EX", 3), rep("NTV", 3)), 
  Variable = rep(c("Pro-government bias", "Neutrality", "Pro-government bias * \nNeutrality"), times = 3), 
  Estimate = c(coef(reg1)[c("Group4", "Q16_4", "Group4:Q16_4")], 
               coef(reg2)[c("Group5", "Q16_4", "Group5:Q16_4")], 
               coef(reg3)[c("Group6", "Q16_4", "Group6:Q16_4")]), 
  SE = c(coef(summary(reg1))[, 2][c("Group4", "Q16_4", "Group4:Q16_4")], 
         coef(summary(reg2))[, 2][c("Group5", "Q16_4", "Group5:Q16_4")], 
         coef(summary(reg3))[, 2][c("Group6", "Q16_4", "Group6:Q16_4")]), 
  Lower = c(confint(reg1, level = 0.95)[c("Group4", "Q16_4", "Group4:Q16_4"),1], 
            confint(reg2, level = 0.95)[c("Group5", "Q16_4", "Group5:Q16_4"),1], 
            confint(reg3, level = 0.95)[c("Group6", "Q16_4", "Group6:Q16_4"),1]), 
  Upper = c(confint(reg1, level = 0.95)[c("Group4", "Q16_4", "Group4:Q16_4"),2], 
            confint(reg2, level = 0.95)[c("Group5", "Q16_4", "Group5:Q16_4"),2], 
            confint(reg3, level = 0.95)[c("Group6", "Q16_4", "Group6:Q16_4"),2])
)
reg1_2_3$TV <- factor(reg1_2_3$TV, levels = c("EX", "NHK", "NTV"))
reg1_2_3$Variable <- factor(
  reg1_2_3$Variable, 
  levels = c("Pro-government bias * \nNeutrality", "Neutrality", "Pro-government bias")
)
reg1_2_3_plot <- ggplot(reg1_2_3, 
                        aes(x = Variable, y = Estimate, 
                            ymin = Lower, ymax = Upper)) + 
  geom_hline(yintercept = 0, color = "gray", linetype = 2) + 
  geom_pointrange() + 
  geom_text(aes(label = paste0(round(Estimate, 2), "\n(", round(SE, 2), ")")), 
            size = 4, vjust = -0.2) + 
  facet_wrap(~TV, ncol = 3) + 
  labs(x = "", y = "Estimate", title = "(a) Coefficients") + 
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), 
        axis.title.x = element_blank()) + 
  coord_flip()
plot(reg1_2_3_plot)


# marginal effect
## Figure 2b
interplot1 <- interplot(reg1, var1 = "Group4", var2 = "Q16_4", point = TRUE) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  ylim(-1.7, 1.7) + 
  labs(x = expression(Agree  %<-% 
                        "NHK Should Be More Neutral than Others" 
                      %->%  Disagree), 
       y = "Effect on Distrust for the Report", 
       title = "(b) Marginal Effects: NHK")
plot(interplot1)
