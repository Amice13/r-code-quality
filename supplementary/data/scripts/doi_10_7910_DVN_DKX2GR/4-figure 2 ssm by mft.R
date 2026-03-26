##### Script Purpose ------------------------------ 
# Creates figure 2


##### Load Packages ------------------------------ 
library(tidyverse)



##### Import Data ------------------------------ 
df <- readRDS("wrangled_df.rds")

# filter to ssmarriage only, the subject of study 1
ssm_df <- df %>% 
  filter(issue == "banssmarriage")

# Reformat the doc_id for labeling data on plots in study 1
ssm_df <- ssm_df %>% 
  mutate(doc_id = paste(referendum, side))

# summary stats of moral rhetoric by side (pro or con)
ssm_df %>%
  group_by(side) %>%
  summarize(min = min(moralwords_perc),
            mean = mean(moralwords_perc),
            median = median(moralwords_perc),
            max = max(moralwords_perc))
# Means seem very similar, not likely stat sig.

# Do we meet assumption of equal variance?
bartlett.test(moralwords_perc ~ side, data = df) 
# yes, would need to be 0.05 to fail to meet

with(ssm_df, t.test(moralwords_perc ~ side, var.equal = TRUE))
# Yep, not stat sig. p = 0.9928
#   So, Pro and Con sides use the same amount of moral rhetoric in their appeals

# Visualize the foundation differences
# Figure 2, pro/con differences in MFT moral rhetoric
df_fig2 <- ssm_df %>% 
  select(side, care_perc:sanctity_perc) %>% 
  gather(group, value, -side) %>% 
  mutate(group = recode(group,
                        care_perc = "Care", 
                        fairness_perc = "Fairness", 
                        loyalty_perc = "Loyalty", 
                        authority_perc = "Authority", 
                        sanctity_perc = "Sanctity")) %>% 
  mutate(group = fct_relevel(group, "Care", "Fairness", "Loyalty", "Authority", "Sanctity")) # factorize the group variable according to MFT conventions

ggplot(df_fig2, aes(x = side, y = value)) +
  geom_boxplot(lwd = .5, outlier.shape = NA) + 
  geom_jitter(alpha = 0.5, width = 0.05) + 
  facet_grid(cols = vars(group)) +
  scale_x_discrete(labels = c("Oppose", "Support")) +
  labs(title = "Moral Foundations Language in Voter Guides for Banning Same-sex Marriage",
       subtitle = "All Pro/Con differences statistically significant, except Care foundation (p = 0.2)\n",
       x = "\nVoter Guide Statements Opposing or Supporting Banning Same-Sex marriage\n",
       y = "\n% of Moral Language in Voter Guide Statements\n") +
  theme_bw() + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave("output/figure 2.png", width = 8, height = 7, dpi = 400)



##### T-Tests ------------------------------ 
# MF t-tests from figure 2
with(ssm_df, t.test(care_perc ~ side, var.equal = TRUE))       # ns, p = 0.200
with(ssm_df, t.test(fairness_perc ~ side, var.equal = TRUE))   # sig, p < 0.01
with(ssm_df, t.test(loyalty_perc ~ side, var.equal = TRUE))    # sig, p < 0.01
with(ssm_df, t.test(authority_perc ~ side, var.equal = TRUE))  # sig, p < 0.04
with(ssm_df, t.test(sanctity_perc ~ side, var.equal = TRUE))   # sig, p < 0.01
#  So, all pro/con means are significantly different except for Care
