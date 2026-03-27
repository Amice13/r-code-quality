##############################
#
# Replication file for the comparative analysis in:
#
# Reining in the Rascals: Challenger Parties' Path to Power
#
# For publication in the the Journal of Politics
#
# Frederik Hjorth, Jacob Nyrup, & Martin Vinæs Larsen
# 
##################

## Load packages ---

pacman::p_load(tidyverse,lubridate,readxl)

## Load data ---

df <- read_xlsx("df_comparative.xlsx")

####
# Figure 2 ---
####

decades <- df %>% ungroup() %>% select(election_id,decade) %>% distinct(election_id,.keep_all=TRUE)

perelection <- df %>% 
  group_by(election_id) %>% 
  tally() %>% 
  left_join(.,decades,by=c("election_id")) %>% 
  group_by(decade) %>% 
  summarize(mean_parties = mean(n)) %>%
  mutate()

# Figure 2A: Average number of challenger parties per election ---

ggplot(perelection,aes(x=decade,y=mean_parties, group=1)) +
  geom_point(stat='summary', fun=sum) +
  stat_summary(fun=sum, geom="line") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  xlab("") + 
  ylim(0,5) +
  ylab("Average number of challenger parties") +
  theme(legend.position = "none") +
  scale_color_manual(values=c("grey70"))

ggsave("output/fig2_a.pdf",width=6,height=6)

# Figure 2B: Likelihood of joining government ---

joincoalition <- df %>% group_by(ranbefore) %>% 
                          summarize(mean = mean(cabinet_party)) %>% 
                          filter(!is.na(ranbefore)) %>%
                          mutate(ranbefore = fct_relevel(ranbefore,"First term","Second term","Third term"))

dif1 <- joincoalition[2,2] - joincoalition[1,2]
dif2 <- joincoalition[3,2] - joincoalition[2,2]

ggplot(joincoalition,aes(x=ranbefore,y=mean)) +
  geom_col() +
  theme_bw() +
  theme(axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  labs(x="Number of terms in parliament",y="Probability of joining the goverment coalition (pct.)") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), lim=c(0,0.15)) +
  ggplot2::annotate("rect", xmin = 0.6, xmax = 2.4, ymin = 0.10, ymax =0.10, alpha=0.5,colour = "black", size=0.2) +
  ggplot2::annotate("rect", xmin = 0.6, xmax = 0.6, ymin = 0.10, ymax =0.095, alpha=0.5,colour = "black", size=0.2) +
  ggplot2::annotate("rect", xmin = 2.4, xmax = 2.4, ymin = 0.10, ymax =0.095, alpha=0.5,colour = "black", size=0.2) +
  ggplot2::annotate("text", x = 1.5, y = 0.11, label = paste0("Difference: ", round(dif1,3)*100,"%"),size = 4.5) +
  ggplot2::annotate("rect", xmin = 1.5, xmax = 3.4, ymin = 0.13, ymax =0.13, alpha=0.5,colour = "black", size=0.2) +
  ggplot2::annotate("rect", xmin = 1.5, xmax = 1.5, ymin = 0.13, ymax =0.125, alpha=0.5,colour = "black", size=0.2) +
  ggplot2::annotate("rect", xmin = 3.4, xmax = 3.4, ymin = 0.13, ymax =0.125, alpha=0.5,colour = "black", size=0.2) +
  ggplot2::annotate("text", x = 2.5, y = 0.14, label = paste0("Difference: ", round(dif2,3)*100,"%"),size = 4.5)

ggsave("output/fig2_b.pdf",width=6,height=6)

# Parties in first election

pfirstelection <- df %>% filter(cabinet_party == 1 & no_election == 1)

# Parties after first election

pafterfirstelection <- df %>% filter(cabinet_party == 1 & no_election > 1) %>% distinct(party_name,.keep_all = TRUE)

