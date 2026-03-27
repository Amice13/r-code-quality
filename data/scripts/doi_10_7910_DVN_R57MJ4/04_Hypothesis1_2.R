# 04_Hypothesis1_2.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2021-6-25 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("dplyr")
require("tidyr")
require("ggplot2")
require("ggpubr")
require("coefplot")
require("interplot")
require("simpleboot")
theme_set(theme_classic(base_size = 12))

# Hypothesis 1-2
# only for those who believe NHK should be neutral
## respondent's ideology
reg4 <- lm(Q10 ~ Group4 * Q5_1 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_1, 
           exp[exp$TV == "NHK" & exp$Q16_4 %in% c(1, 2),])
summary(reg4)
## Figure 3a
reg4_plot <- coefplot::coefplot(
  reg4, intercept = FALSE, coefficients = c("Group4", "Q5_1", "Group4:Q5_1"), 
  lwdOuter = 1, decreasing = TRUE, color = "black", 
  title = "(a) Respondents' Ideology", 
  ylab = "", xlab = "Distrust for the Report", 
  newNames = c(Group4 = "Pro-government bias", 
               Q5_1 = "Respondent's Ideology", 
               `Group4:Q5_1` = "Pro-government bias * \nRespondent's Ideology")
  ) + 
  geom_text(aes(label = paste0(
    rev(round(reg4$coefficients[c("Group4", "Q5_1", "Group4:Q5_1")], 2)), "\n(", 
    rev(round(coef(summary(reg4))[, 2][c("Group4", "Q5_1", "Group4:Q5_1")], 2)), ")")), 
            size = 4, vjust = -0.2) + 
  xlim(-0.5, 1.75) + theme_classic(base_size = 12) + 
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))
plot(reg4_plot)

## bootstrap
reg4_boot <- lm.boot(reg4, R = 2000)
reg4_boot_res <- data.frame(
  variables = names(reg4$coefficients), 
  coefficients = reg4$coefficients, 
  bootstrap_sd = summary(reg4_boot)$stdev.params
)
reg4_boot_res <- mutate(
  reg4_boot_res, 
  variables = recode(variables, 
                     `(Intercept)` = "Intercept", 
                     `Group4` = "Pro-government bias", 
                     `Q5_1` = "Respondent's Ideology", 
                     `Q1` = "Female", 
                     `Q2` = "Age", 
                     `College` = "College Education", 
                     `Q6_1` = "Political Interest", 
                     `Q17` = "Frequency of Viewing TV", 
                     `Q11_1` = "Credibility of NHK", 
                     `Group4:Q5_1` = "Pro-government bias * \nRespondent's Ideology")
)
reg4_boot_res$variables <- factor(
  reg4_boot_res$variables, 
  levels = c("Pro-government bias * \nRespondent's Ideology", "Credibility of NHK", 
             "Frequency of Viewing TV", "Political Interest", 
             "College Education", "Age", "Female", "Respondent's Ideology", 
             "Pro-government bias", "Intercept")
)
## Figure A.5a
reg4_boot_plot <- ggplot(reg4_boot_res[reg4_boot_res$variables != "Intercept",], 
                          aes(x = variables, y = coefficients, 
                              ymin = coefficients - 1.96*bootstrap_sd, 
                              ymax = coefficients + 1.96*bootstrap_sd)) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  geom_pointrange() + ylim(-0.5, 1.75) + 
  geom_text(aes(label = round(reg4$coefficients[-1], 2)), size = 4, vjust = -1.3) + 
  coord_flip() + 
  labs(title = "(a) Respondents' Ideology", 
       x = "", y = "Distrust for the Report") + 
  theme_classic(base_size = 12) + 
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))
plot(reg4_boot_plot)

## LDP supporter
reg5 <- lm(Q10 ~ Group4 * LDP + Q1 + Q2 + College + Q6_1 + Q17 + Q11_1, 
           exp[exp$TV == "NHK" & exp$Q16_4 %in% c(1, 2),])
summary(reg5)
## Figure 3b
reg5_plot <- coefplot::coefplot(
  reg5, intercept = FALSE, coefficients = c("Group4", "LDP", "Group4:LDP"), 
  lwdOuter = 1, decreasing = TRUE, color = "black", 
  title = "(b) Party Affiliation", 
  ylab = "", xlab = "Distrust for the Report", 
  newNames = c(Group4 = "Pro-government bias", 
               LDP = "LDP Supporter", 
               `Group4:LDP` = "Pro-government bias * \nLDP Supporter")
  ) + 
  geom_text(aes(label = paste0(
    rev(round(reg5$coefficients[c("Group4", "LDP", "Group4:LDP")], 2)), "\n(", 
    rev(round(coef(summary(reg5))[, 2][c("Group4", "LDP", "Group4:LDP")], 2)), ")")), 
    size = 4, vjust = -0.2) + 
  xlim(-0.75, 1.75) + theme_classic(base_size = 12) + 
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))
plot(reg5_plot)

## bootstrap
reg5_boot <- lm.boot(reg5, R = 2000)
reg5_boot_res <- data.frame(
  variables = names(reg5$coefficients), 
  coefficients = reg5$coefficients, 
  bootstrap_sd = summary(reg5_boot)$stdev.params
)
reg5_boot_res <- mutate(
  reg5_boot_res, 
  variables = recode(variables, 
                     `(Intercept)` = "Intercept", 
                     `Group4` = "Pro-government bias", 
                     `LDP` = "LDP Supporter", 
                     `Q1` = "Female", 
                     `Q2` = "Age", 
                     `College` = "College Education", 
                     `Q6_1` = "Political Interest", 
                     `Q17` = "Frequency of Viewing TV", 
                     `Q11_1` = "Credibility of NHK", 
                     `Group4:LDP` = "Pro-government bias * \nLDP Supporter")
)
reg5_boot_res$variables <- factor(
  reg5_boot_res$variables, 
  levels = c("Pro-government bias * \nLDP Supporter", "Credibility of NHK", 
             "Frequency of Viewing TV", "Political Interest", 
             "College Education", "Age", "Female", "LDP Supporter", 
             "Pro-government bias", "Intercept")
)
## Figure A.5b
reg5_boot_plot <- ggplot(reg5_boot_res[reg5_boot_res$variables != "Intercept",], 
                          aes(x = variables, y = coefficients, 
                              ymin = coefficients - 1.96*bootstrap_sd, 
                              ymax = coefficients + 1.96*bootstrap_sd)) + 
  geom_hline(yintercept = 0, colour = "gray", linetype = "dashed") + 
  geom_pointrange() + ylim(-0.75, 1.75) + 
  geom_text(aes(label = round(reg5$coefficients[-1], 2)), size = 4, vjust = -1.3) + 
  coord_flip() + 
  labs(title = "(b) Party affiliation", 
       x = "", y = "Distrust for the Report") + 
  theme_classic(base_size = 12) + 
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13))
plot(reg5_boot_plot)


# only for those who don't believe NHK should be neutral
## respondent's ideology
reg6 <- lm(Q10 ~ Group4 * Q5_1 + Q1 + Q2 + College + Q6_1 + Q17 + Q11_1, 
           exp[exp$TV == "NHK" & exp$Q16_4 %in% c(3, 4, 5),])
summary(reg6)

## LDP supporter
reg7 <- lm(Q10 ~ Group4 * LDP + Q1 + Q2 + College + Q6_1 + Q17 + Q11_1, 
           exp[exp$TV == "NHK" & exp$Q16_4 %in% c(3, 4, 5),])
summary(reg7)
