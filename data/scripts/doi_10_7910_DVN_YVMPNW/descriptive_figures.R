################################################################################
#Replication Materials for
#Explanation Giving Promotes Democratic Satisfaction Regardless of Respondent Partisanship
#
#Syntax produces:
  #Figures 1, 2, and 3 and
  #Figures OA1, OA2, and OA3
################################################################################

#######################
#Packages & Data
#######################

library(correlation)
library(rio)
library(srvyr)
library(ggrepel)
library(tidyverse)
library(ggstance)

`%notin%` <- Negate(`%in%`)

###Micro Data
ess <- import("ess_final.dta") %>%
  filter(country %notin% c("Albania", "Russia", "Ukraine", "Kosovo", "Bulgaria")) %>%
  select(-dem_imp) %>%
  mutate(anweight = pspwght*pweight*10e3)
  
#######################
#Figure 1: Importance of Different Regime Elements
#######################

###Unweighted Means
unwe <- ess %>%
  select(country, ends_with("_imp"), pspwght, pweight, strata) %>%
  pivot_longer(cols=elections_imp:inequality_imp) %>%
  group_by(name) %>%
  summarize(unweighted = mean(value, na.rm=T))  

###Weighted
ess_long <- ess %>%
  select(country, ends_with("_imp"), pspwght, pweight, strata, anweight) %>%
  pivot_longer(cols=elections_imp:inequality_imp) %>%
  as_survey_design(strata="strata", weights=anweight)

wei <- ess_long %>%
  group_by(name) %>%
  summarize(weighted = survey_mean(value, na.rm=T))

###Comb
comb <- left_join(unwe, wei, by="name") %>%
  mutate(lower = weighted - (1.96*weighted_se), 
         upper = weighted + (1.96*weighted_se), 
         name = dplyr::recode(name, 
                              "discuss_imp" = "Voters Discuss Politics", 
                              "courts_imp" = "Courts Treat All Same", 
                              "explains_imp" = "Gov't Explains Decisions", 
                              "elections_imp" = "Free/Fair Elections", 
                              "stop_imp" = "Courts Stop Gov't", 
                              "mreliable_imp" = "Media Gives Reliable Info", 
                              "punished_imp" = "Parties Punished for Poor Perf.", 
                              "minority_imp" = "Minority Rights Protected", 
                              "ref_imp" = "Referendums", 
                              "inequality_imp" = "Gov't Reduces Income Diffs", 
                              "mcriticize_imp" = "Media Free to Criticize Gov't", 
                              "alternatives_imp" = "Clear Party Alternatives", 
                              "poverty_imp" = "Gov't Protects All From Poverty", 
                              "opposition_imp" = "Opposition Free to Criticize Gov't", 
                              "imm_imp" = "Immigrants Only Vote Once Citizens")) %>%
  arrange(-weighted)

###Explanations data
head(comb)

#solution for bolding single entry from here: https://stackoverflow.com/questions/20609716/changing-format-of-some-axis-labels-in-ggplot2-according-to-condition
#Note, this generates an warning message that may be relevant for those in the future: 
  #'Warning message:
  #' Vectorized input to `element_text()` is not officially supported.
  #' ℹ Results may be unexpected or may change in future versions of ggplot2. 

ggplot(comb, aes(x=weighted, y=reorder(name, weighted))) + 
  ggstance::geom_pointrangeh(aes(xmin=lower, xmax=upper)) + 
  theme_bw(14) + 
  labs(y=NULL, x = "Avg. on 0-10 scale (higher = more important)") + 
  theme(axis.text.y = element_text(face=c("plain", "plain", 
                                          "plain", "plain", 
                                          "plain", "plain", 
                                          "plain", "plain", 
                                          "plain", "plain", 
                                          "plain", "plain", 
                                          "plain", "bold", 
                                          "plain")))

ggsave("figure1.png", height=6, width=10, dpi=600)
ggsave("figure1.pdf", height=6, width=10, dpi=600)

###Remove unnecessary dfs
rm(ess_long, wei, unwe, comb)

#######################
#Figure 2: Distribution of Perceived Explanation Giving by Country
#######################

ggplot(ess, aes(x=explains_applies)) + 
  geom_bar() + 
  theme_bw() + 
  facet_wrap(~country) + 
  labs(x = "Government Explains Its Decisions to Voters") + 
  scale_x_continuous(breaks=seq(from=0, to=10, by=1))

ggsave("figure2.png", height=6, width=10, dpi=600)    
ggsave("figure2.pdf", height=6, width=10, dpi=600)    


#######################
#Figure OA1: Distribution of Gov't Satisfaction by Country
#######################

ggplot(ess, aes(x=gov_satis)) + 
  geom_bar() + 
  theme_bw() + 
  facet_wrap(~country) + 
  labs(x = "Government Satisfaction") + 
  scale_x_continuous(breaks=seq(from=0, to=10, by=1))

ggsave("figure_oa1.png", height=6, width=10, dpi=600)                       

#######################
#Figure OA2: Distribution of Democratic Satisfaction by Country
#######################

ggplot(ess, aes(x=dem_satis)) + 
  geom_bar() + 
  theme_bw() + 
  facet_wrap(~country) + 
  labs(x = "Democratic Satisfaction") + 
  scale_x_continuous(breaks=seq(from=0, to=10, by=1))

ggsave("figure_oa2.png", height=6, width=10, dpi=600)                

#######################
#Descriptive Data: (Weighted) Means for Dem & Gov't Satisfaction and Explains Applies
#######################
ess_long <- ess %>%
  select(gov_satis, dem_satis, explains_applies, strata, anweight) %>%
  pivot_longer(cols=gov_satis:explains_applies) %>%
  as_survey_design(strata="strata", weights=anweight)

wei <- ess_long %>%
  group_by(name) %>%
  summarize(weighted = survey_mean(value, na.rm=T)) %>%
  mutate(lower = weighted - (1.96*weighted_se), 
         upper = weighted + (1.96*weighted_se))

head(wei)
rm()

#######################
#Figure OA3: Distribution of Partisanship by Country
#######################

ess %>%
  select(country, co_partisan) %>%
  filter(!is.na(co_partisan)) %>%
  group_by(country) %>%
  mutate(country_n = n()) %>%
  ungroup() %>%
  group_by(country, country_n, co_partisan) %>%
  summarize(cat_n = n()) %>%
  ungroup() %>%
  mutate(cat_prop = cat_n/country_n,
         co_partisan = factor(co_partisan, 
                              levels=c(1,2,3), 
                              labels=c("Co-Partisan", 
                                       "Opposing P.", 
                                       "Non-Partisan"))) %>%
  ggplot(aes(x=co_partisan, y=cat_prop)) + 
  geom_col() + 
  facet_wrap(~ country) + 
  theme_bw(14) + 
  labs(title = "Partisanship Distribution across Countries", 
       y = "Proportion in Category", 
       x = NULL) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0))

ggsave("figure_oa3.png",  height=8, width=8, dpi=600)       

#######################
#Figure 3: Perceptions of Explanation Giving in Mass and Elite Samples
#######################

###Country Mean
sdes <- ess %>% 
  as_survey_design(strata="strata", weights=anweight)

wei_bycountry <- sdes %>%
  group_by(country) %>%
  summarize(explains = survey_mean(explains_applies, na.rm=T))

###Combine
combined <- ess %>%
  select(country, reason_std) %>%
  group_by(country) %>%
  slice(1) %>%
  left_join(wei_bycountry, ., by="country")

###Combine
combined %>%
  select(-country, -explains_se) %>%
  correlation()

###Plot
ggplot(combined, aes(x=reason_std, y=explains)) + 
  geom_point() + 
  geom_smooth(method=lm) + 
  theme_bw() + 
  labs(x = "Explanations (V-DEM)", 
       y = "Explanations (ESS)") + 
  annotate(geom="label", x=-1.85, y=6.5, label="r=0.47") + 
  geom_text_repel(aes(label=country))

ggsave("figure3.png", height=6, width=10, dpi=600)
ggsave("figure3.pdf", height=6, width=10, dpi=600)

#######################
#Remove data objects
#######################
rm(list = ls())
