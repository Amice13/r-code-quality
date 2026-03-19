# S1_Evaluation_of_Cabinet.R
# Purpose: The Role of Public Broadcasting in Media Bias
# Created: 2019-9-6 Taka-aki Asano
# Last Modified: 2021-10-14

# package
require("ggpubr")
require("coefplot")
require("interplot")
theme_set(theme_classic(base_size = 12))


# t-test
gov_mean <- ggbarplot(exp, x = "Control", y = "Q13", facet.by = "TV", 
                      label = TRUE, lab.nb.digits = 2, lab.vjust = -2.0, 
                      add = "mean_ci", ylim = c(1, 5)) + 
  stat_compare_means(comparisons = list(c("Control", "Pro-government Bias")), 
                     label = "p.signif", method = "t.test", 
                     label.x = 1.5, label.y = 4.5, 
                     symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns"))) + 
  labs(x = "", y = "Mean") + 
  theme(axis.text.x = element_text(size = 10))
plot(gov_mean)


# backfire mechanism or trusting mechanism
### NHK
reg1 <- lm(Q13 ~ Group4 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, exp[exp$TV == "NHK",])
summary(reg1)
reg1_plot <- coefplot::coefplot(
  reg1, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "NHK", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", 
               Q2 = "Age", 
               College = "College Education", 
               Q6_1 = "Political Interest", 
               Q17 = "Frequency of Viewing TV", 
               Group4 = "Pro-government bias", 
               Q10 = "Distrust for the Report", 
               `Group4:Q10` = "Pro-government bias * \nDistrust for the Report")
  ) + 
  geom_text(aes(label = rev(round(reg1$coefficients[-1], 2))), size = 4, vjust = -1.3) + 
  xlim(-2, 1) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg1_plot)
### EX
reg2 <- lm(Q13 ~ Group5 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, exp[exp$TV == "EX",])
summary(reg2)
reg2_plot <- coefplot::coefplot(
  reg2, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "EX", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", 
               Q2 = "Age", 
               College = "College Education", 
               Q6_1 = "Political Interest", 
               Q17 = "Frequency of Viewing TV", 
               Group5 = "Pro-government bias", 
               Q10 = "Distrust for the Report", 
               `Group5:Q10` = "Pro-government bias * \nDistrust for the Report")) + 
  geom_text(aes(label = rev(round(reg2$coefficients[-1], 2))), size = 4, vjust = -1.3) + 
  xlim(-2, 1) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg2_plot)
### NTV
reg3 <- lm(Q13 ~ Group6 * Q10 + Q1 + Q2 + College + Q6_1 + Q17, exp[exp$TV == "NTV",])
summary(reg3)
reg3_plot <- coefplot::coefplot(
  reg3, intercept = FALSE, lwdOuter = 1, 
  decreasing = TRUE, color = "black", title = "NTV", 
  ylab = "", xlab = "Negative Views of Abe's Diplomacy", 
  newNames = c(Q1 = "Female", 
               Q2 = "Age", 
               College = "College Education", 
               Q6_1 = "Political Interest", 
               Q17 = "Frequency of Viewing TV", 
               Group6 = "Pro-government bias", 
               Q10 = "Distrust for the Report", 
               `Group6:Q10` = "Pro-government bias * \nDistrust for the Report")
  ) + 
  geom_text(aes(label = rev(round(reg3$coefficients[-1], 2))), size = 4, vjust = -1.3) + 
  xlim(-2, 1) + theme_classic(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5))
plot(reg3_plot)


# marginal effect
## Distrust for the Report
### NHK
interplot1 <- interplot(reg1, var1 = "Group4", var2 = "Q10", point = TRUE) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  ylim(-1.7, 1.7) + 
  labs(x = expression(Lower  %<-% "Distrust for the Report" %->%  Higher), 
       y = "Effect on Negative Views of Abe's Diplomacy", 
       title = "NHK") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(interplot1)
### EX
interplot2 <- interplot(reg2, var1 = "Group5", var2 = "Q10", point = TRUE) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  ylim(-1.7, 1.7) + 
  labs(x = expression(Lower  %<-% "Distrust for the Report" %->%  Higher), 
       y = "Effect on Negative Views of Abe's Diplomacy", 
       title = "EX") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(interplot2)
### NTV
interplot3 <- interplot(reg3, var1 = "Group6", var2 = "Q10", point = TRUE) + 
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") + 
  ylim(-1.7, 1.7) + 
  labs(x = expression(Lower  %<-% "Distrust for the Report" %->%  Higher), 
       y = "Effect on Negative Views of Abe's Diplomacy", 
       title = "NTV") + 
  theme(plot.title = element_text(hjust = 0.5))
plot(interplot3)
