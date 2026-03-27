# 05_Hypothesis2.R
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


# Hypothesis 2
## respondent's ideology
## NHK
reg8 <- lm(Q10 ~ Group4 * Q5_1 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_1, exp[exp$TV == "NHK",])
summary(reg8)

## EX
reg9 <- lm(Q10 ~ Group5 * Q5_1 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_3, exp[exp$TV == "EX",])
summary(reg9)

## NTV
reg10 <- lm(Q10 ~ Group6 * Q5_1 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_2, exp[exp$TV == "NTV",])
summary(reg10)

## Figure 4a
reg8_9_10 <- data.frame(
  TV = c(rep("NHK", 3), rep("EX", 3), rep("NTV", 3)), 
  Variable = rep(c("Pro-government bias", "Ideology", "Pro-government bias * \nIdeology"), times = 3), 
  Estimate = c(coef(reg8)[c("Group4", "Q5_1", "Group4:Q5_1")], 
               coef(reg9)[c("Group5", "Q5_1", "Group5:Q5_1")], 
               coef(reg10)[c("Group6", "Q5_1", "Group6:Q5_1")]), 
  SE = c(coef(summary(reg8))[, 2][c("Group4", "Q5_1", "Group4:Q5_1")], 
         coef(summary(reg9))[, 2][c("Group5", "Q5_1", "Group5:Q5_1")], 
         coef(summary(reg10))[, 2][c("Group6", "Q5_1", "Group6:Q5_1")]), 
  Lower = c(confint(reg8, level = 0.95)[c("Group4", "Q5_1", "Group4:Q5_1"),1], 
            confint(reg9, level = 0.95)[c("Group5", "Q5_1", "Group5:Q5_1"),1], 
            confint(reg10, level = 0.95)[c("Group6", "Q5_1", "Group6:Q5_1"),1]), 
  Upper = c(confint(reg8, level = 0.95)[c("Group4", "Q5_1", "Group4:Q5_1"),2], 
            confint(reg9, level = 0.95)[c("Group5", "Q5_1", "Group5:Q5_1"),2], 
            confint(reg10, level = 0.95)[c("Group6", "Q5_1", "Group6:Q5_1"),2])
)
reg8_9_10$TV <- factor(reg8_9_10$TV, levels = c("EX", "NHK", "NTV"))
reg8_9_10$Variable <- factor(
  reg8_9_10$Variable, 
  levels = c("Pro-government bias * \nIdeology", "Ideology", "Pro-government bias")
)
reg8_9_10_plot <- ggplot(reg8_9_10, 
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
plot(reg8_9_10_plot)


# marginal effect
## Figure 4b
interplot9 <- interplot(reg9, var1 = "Group5", var2 = "Q5_1", point = TRUE) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  ylim(-1.7, 1.7) + 
  labs(x = expression(Left  %<-% "Respondent's Ideology" %->%  Right), 
       y = "Effect on Distrust for the Report", 
       title = "(b) Marginal Effects: EX")
plot(interplot9)

## Figure 4c
interplot10 <- interplot(reg10, var1 = "Group6", var2 = "Q5_1", point = TRUE) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  ylim(-1.7, 1.7) + 
  labs(x = expression(Left  %<-% "Respondent's Ideology" %->%  Right), 
       y = "Effect on Distrust for the Report", 
       title = "(c) Marginal Effects: NTV")
plot(interplot10)
