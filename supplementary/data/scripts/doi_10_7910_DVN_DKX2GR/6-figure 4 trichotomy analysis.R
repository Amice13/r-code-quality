##### Script Purpose ------------------------------ 
# This script creates the plots for figure 4

##### Load Packages ------------------------------ 
library(tidyverse)

##### Import Data ------------------------------ 
df <- readRDS("wrangled_df.rds")



# ##### Two Category analysis ------------------------------ 
# 
# # Figure 4A - boxplot by category
# ggplot(df, aes(x = twocategory, y = moralwords_perc)) + 
#   geom_boxplot(lwd = 0.5, outlier.shape = NA) + 
#   geom_jitter(alpha = 0.5, width = 0.05) + 
#   scale_y_continuous(breaks = seq(0,30, by = 5)) +
#   labs(title = "Two category model of Morality Policy Paradigm",
#        subtitle = "Differences are significant (p < .01)",
#        x = "",
#        y = "\n% Moral Rhetoric\n") +
#   theme_bw() + 
#   theme(legend.position = "none", 
#         panel.grid.major.x = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) 
# 
# ggsave("output/figure 4_twocategories.png", width = 8, height = 8, dpi = 400)
# 
# # Statistical test
# library(car)
# leveneTest(moralwords_perc ~ twocategory, data = df)   # passes, p > 0.05
# 
# with(df, t.test(moralwords_perc ~ twocategory))  # yep, significantly different p < 0.001
# 
# # To double-check which issues are in which category?
# table(df$issue, df$twocategory)



##### Three Category Model ------------------------------ 
# Category labels
category_labels <- c(
  "non-moral" = "Non-moral",
  "mixed" = "Mixed",
  "moral" = "Moral"
)


# Figure 4 - boxplot by category, with better labels
# Note: to get caption right, needed to manually insert \n breaks.
#       if dimensions need to change, will need to fix the breaks.
ggplot(df, aes(x = threecategory, y = moralwords_perc)) + 
  geom_boxplot(lwd = 0.5, outlier.shape = NA) + 
  geom_jitter(alpha = 0.5, width = 0.05) + 
  scale_y_continuous(breaks = seq(0,30, by = 5)) +
  scale_x_discrete(labels = category_labels) + 
  labs(title = "Three category model of the Morality Policy Paradigm",
       subtitle = "ANOVA and posthoc tests reveal all differences are significant (p < .01)",
       x = "",
       y = "\n% of Moral Language in Voters Guide Statements\n",
       caption = "Note: Non-moral policies are minimum wage and property tax limits. Mixed policies are abortion funds, 
       official English language, recreational marijuana, right to work, and tribal gaming. Moral policies are 
       banning same-sex marriage, death penalty, medical marijuana, and physician-assisted suicide.") +
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0))

ggsave("output/figure 4_threecategories.png", width = 7, height = 7, dpi = 400)


# Statistical test
library(car)
leveneTest(moralwords_perc ~ threecategory, data = df)   
# actually does not pass, p is less tahn 0.05... variances are not homogenous
# Will need to deploy a Welch t-test instead of a conventional one

anova1 <- aov(moralwords_perc ~ threecategory, data = df)
summary(anova1)   # yes, sig
TukeyHSD(anova1)  # Yes, all three are sig, different from each other

# Welch's one-way test, if variances are not homogenous
anova_welch <- oneway.test(moralwords_perc ~ threecategory, data = df)
anova_welch
# looks okay, p < 0.001
# plot(anova1) # visually inspect residuals plots



