##### Script Purpose ------------------------------ 
# Analysis of Study 2: Referenda from all issues



##### Load Packages ------------------------------ 
library(tidyverse)



##### Import Data ------------------------------ 
df <- readRDS("wrangled_df.rds")



##### Study 2, Looking at Total Moral Rhetoric ------------------------------ 

##### Figure 3 ------------------------------ 
# create readable labels for plot below
issue_labels <- c("minimumwage" = "Minimum\nWage",
                  "propertytaxlimit" = "Property\nTax Limits",
                  "officialenglish" = "English Official\nLanguage",
                  "abortionfunds" = "Abortion\nFunds",
                  "recreationpot" = "Recreational\nMarijuana",
                  "tribalgaming" = "Tribal\nGaming",
                  "righttowork" = "Right\nTo Work",
                  "deathpenalty" = "Death\nPenalty",
                  "medicalpot" = "Medical\nMarijuana",
                  "banssmarriage" = "Same-sex\nMarriage",
                  "pasuicide" = "Phys-Assisted\nSuicide")

ggplot(df, aes(x = fct_reorder(issue, moralwords_perc, .fun = mean), y = moralwords_perc)) + 
  geom_boxplot(lwd = 0.5, outlier.shape = NA) + 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") + 
  geom_jitter(alpha = 0.5, width = 0.05) + 
  scale_y_continuous(breaks = seq(0,30, by = 5)) +
  scale_x_discrete(labels = issue_labels) +
  labs(title = "Percentage of Moral Language Across Public Policy Domains",
       x = "",
       y = "\n% of Moral Language in Voter Guide Statements\n",
       caption = "Note: Issues are arranged by mean value, represented by a dashed line in the boxplots. The solid line is the median.") +
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  theme(plot.caption = element_text(hjust = 0))

ggsave("output/figure 3.png", width = 9, height = 7, dpi = 400)

# Originally, you used reorder() in the ggplot, but it ordered by mean instead of 
#   median. This causes death penalty to "sort" lower than medical marijuana,
#   because DP has a higher MEDIAN, but medical marijuana has a higher MEAN
# Check with this: tapply(df$moralwords_perc, df$issue, summary)
# When I switched to fct_reorder(), default sorting is by median
# You can use .fun = mean to change the fct_reorder to mean

##### ANOVA test and Table 1 ------------------------------ 
library(car)
library(broom)
leveneTest(moralwords_perc ~ as.factor(issue), data = df)
# Levene's Test for Homogeneity of Variance (center = median)
#        Df F value Pr(>F)
# group  10  1.4043 0.1863
#       121      
# p > 0.05, so we are fine on homogenous variances between the groups

anova1 <- aov(df$moralwords_perc ~ df$issue)
summary(anova1)
# Df Sum Sq Mean Sq F value Pr(>F)    
# df$issue     10   2007   200.7   17.75 <2e-16 ***
#   Residuals   121   1368    11.3  
# Yes, significant ANOVA, look at multiplec comparisons

# Multiple comparisons
tukey1 <- tidy(TukeyHSD(anova1))
tukey_table <- tukey1 %>% 
  separate(comparison, c("first","second")) %>% 
  mutate(adj.p.value = round(adj.p.value, 3)) %>% 
  mutate(is_it_sig = ifelse(adj.p.value <= 0.05, "*", "")) %>% 
  mutate(is_it_trending = ifelse(adj.p.value > 0.05 & adj.p.value <= 0.10, "†", ""))

write.csv(tukey_table, "output/table1_tukey table.csv")
# This table is used to draft table 1 by hand

# plot TukeyHSDs
# create stat significance categories
tukey1 <- tukey1 %>% 
  mutate(sig = ifelse(adj.p.value <= 0.05, "p < 0.05", "not sig")) %>% 
  mutate(sig = fct_relevel(sig, "not sig", after = Inf))  # reorder factors

p <- ggplot(tukey1, aes(x = reorder(comparison, estimate), y = estimate, color = sig))

p + geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0) + 
  coord_flip() + 
scale_color_manual(values = c("black", "gray75")) +
labs(title = "Pair-wise comparisions from ANOVA analysis of All Issues",
     subtitle = "",
     x = "\nPairs of Issues",
     y = "\nANOVA Estimates\n") +
  theme_bw() + 
  theme(legend.position = "top", 
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/appendix_figure 1 Anova Pairwise.png", width = 10, height = 8, dpi = 400)



##### Generate Appendix Table 2 ------------------------------ 
# This calculates the total counts of moral rhetoric overall, 
#   and for each of the five moral foundations. 
# Remember that this data is aggregating the text from all the documents together. 
# Other data looks at the percentage within a document. 
# So, this will be slightly different from other results because the unit of analysis
#   is different (the whole text instead of a series of documents)

df_summary <- df %>% 
  group_by(issue) %>% 
  summarize(
    total_words = sum(n_tokens),
    care_n = sum(care_n),
    fairness_n = sum(fairness_n),
    loyalty_n = sum(loyalty_n),
    authority_n = sum(authority_n),
    sanctity_n = sum(sanctity_n)
  ) %>% 
  mutate(side = "all") %>% 
  select(issue, side, everything())

# Compute averages
df_summary <- df_summary %>% 
  mutate(total_moral_words = care_n + fairness_n + loyalty_n + authority_n + sanctity_n) %>% 
  mutate(percent_moral_words = (total_moral_words / total_words) * 100) %>% 
  mutate(
    care_perc = (care_n / total_words) * 100, 
    fairness_perc = (fairness_n / total_words) * 100, 
    loyalty_perc = (loyalty_n / total_words) * 100, 
    authority_perc = (authority_n / total_words) * 100,
    sanctity_perc = (sanctity_n / total_words) * 100
  ) %>% 
  select(issue, side, total_words, percent_moral_words, care_perc:sanctity_perc)

write_csv(df_summary, "output/appendix_table2.csv")
# this csv is manually formatted to become appendix table 2


