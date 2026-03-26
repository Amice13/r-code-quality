##### Script Purpose ------------------------------ 
# Creates figure 1


##### Load Packages ------------------------------ 
library(tidyverse)



##### Import Data ------------------------------ 
df <- readRDS("wrangled_df.rds")

# filter to ssmarriage only, the subject of study 1
ssm_df <- df %>% 
  filter(issue == "banssmarriage")

# Need to wrangle the 32 observations into 16 by combining the pro/con
#   This is purely for figure 1, as the rest of the analysis of Study 1 requires 
#   pro/con to be separate
# Selecting referendum and word counts so I can calculate the percentages correctly
# You can't just aggregate pro/con together by moralwords_perc because you'd be
#   averaging two percentages which is wrong. 

df_fig1 <- ssm_df %>% 
  select(referendum, n_tokens, moralwords_n) %>% 
  group_by(referendum) %>% 
  summarize(n_tokens = sum(n_tokens),
            moralwords_n = sum(moralwords_n)) %>% 
  mutate(moralwords_perc = (moralwords_n / n_tokens) * 100)



##### Figure 1 visualization ------------------------------ 
# Referenda labels
referenda_labels <- c(
  "AK1998" = "Alaska 1998",
  "AZ2006" = "Arizona 2006",
  "AZ2008" = "Arizona 2008",
  "CA2000" = "California 2000",
  "CA2008" = "California 2008",
  "CO2006" = "Colorado 2006",
  "ID2006" = "Idaho 2006",
  "MT2004" = "Montana 2004",
  "NE2000" = "Nebraska 2000",
  "NV1998" = "Nevada 1998",
  "NV2002" = "Nevada 2002",
  "OH2004" = "Ohio 2004",
  "OR2004" = "Oregon 2004",
  "TX2005" = "Texas 2005",
  "UT2004" = "Utah 2004",
  "WA2012" = "Washington 2012"
)


# Summary of the quantity of total moral rhetoric in each ssmarriage statement
df_fig1 %>%
  summarize_at(vars(moralwords_perc), list(min = min, mean = mean, max = max))
# min = 10.5%, mean = 16.1%, max = 23.1% moral rhetoric

# Figure 1, amount of moral rhetoric across SSM statements
ggplot(df_fig1, aes(x = reorder(referendum, moralwords_perc), y = moralwords_perc)) + 
  geom_bar(stat = "identity") + 
  geom_hline(aes(yintercept = mean(moralwords_perc))) + 
  annotate("text", x = 1, y = 18, label = "Average") +
  scale_y_continuous(breaks = seq(0, 30, by = 5)) +
  scale_x_discrete(labels = referenda_labels) +
  coord_flip() +
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title = "Moral Language in Referenda Voter Guides",
       y = "\n% of Moral Language in Voter Guide Statements\n",
       x = "Referenda on Banning Same-Sex Marriage")
ggsave("output/figure 1.png", width = 6, height = 6, dpi = 400)
