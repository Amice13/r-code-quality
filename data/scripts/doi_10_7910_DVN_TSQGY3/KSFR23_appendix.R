###Q###Q###Q###Q###Q###Q###Q
###Q###Q  Appendix  ###Q###Q
###Q###Q###Q###Q###Q###Q###Q

## load packages ====
# R version 4.2.3 (2023-03-15)
library(here) # v1.0.1
library(tidyverse) # v2.0.0
library(emmeans) # v1.8.8
library(ggpubr) # v0.6.0
library(quanteda) # v3.3.1
library(quanteda.textstats) # v0.96.3

# Load data ====
load(here("data", "KSFR23_data_clean.Rdata"))
load(here("data", "KSFR23_manipulation_checks.Rdata"))

# multiple testing correction ====
adjust = "holm"

## include utility functions ====
source(here("KSFR23_util.R"))

# Knowledge of EU Politics and Europarties ====
# """" Table A3 ----
# Respondent Performance for each item and overall
#
round(100*c(mean(dtaDE$EUinfo1, na.rm = T),
            mean(dtaDE$EUinfo2, na.rm = T),
            mean(dtaDE$EUinfo3, na.rm = T),
            mean(dtaDE$EUinfo4, na.rm = T),
            mean(dtaDE$EUinfo5, na.rm = T),
            mean(dtaDE$EUinfo6, na.rm = T),
            mean(dtaDE$EUinfo7, na.rm = T)), 1)
round(100*c(mean(dtaIT$EUinfo1, na.rm = T),
            mean(dtaIT$EUinfo2, na.rm = T),
            mean(dtaIT$EUinfo3, na.rm = T),
            mean(dtaIT$EUinfo4, na.rm = T),
            mean(dtaIT$EUinfo5, na.rm = T),
            mean(dtaIT$EUinfo6, na.rm = T),
            mean(dtaIT$EUinfo7, na.rm = T)), 1)
c(mean(dta$EUinfo, na.rm = T), mean(dtaDE$EUinfo, na.rm = T), mean(dtaIT$EUinfo, na.rm = T))

# """" Figure A8 ----
# Respondent performance in identifying their preferred party's europarty
europarty = dta %>% group_by(Country, EupartyCorrect, EUinfo_bin) %>%
  summarize(count = n()) %>% 
  ungroup() %>%
  filter(!is.na(EUinfo_bin) & !is.na(EupartyCorrect)) %>%
  group_by(Country, EUinfo_bin) %>%
  mutate(share = count / sum(count))
europarty$EupartyCorrect = factor(europarty$EupartyCorrect, 
                                  levels = c('DK', 'false', 'true'),
                                  labels = c("Don't know", 'Wrong', 'Correct'))
europarty$Country = factor(europarty$Country, 
                           levels = c("DE", "IT"), 
                           labels = c("Germany", "Italy"))
europarty$EUinfo_bin = factor(europarty$EUinfo_bin, 
                              levels = c("High", "Low"), 
                              labels = c("High knowledge", "Low knowledge"))
europarty$wrapper = paste0(europarty$Country, ': ', europarty$EUinfo_bin, ' knowledge')
figa8 = ggplot(europarty, aes(y = share, x = EUinfo_bin, fill = EupartyCorrect)) + 
  geom_bar(position = 'dodge', stat = 'identity', width = 0.8) +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 0.8),
                     breaks = seq(0, 0.8,0.1),
                     labels = seq(0, 80, 10)) +
  facet_wrap(~Country, ncol = 2, scales = 'free_x', drop = T) +
  scale_fill_manual(values = c("#000000", "#E69F00", "#56B4E9")) +
  labs(y = 'Percentage of respondents') +
  theme_bw() + 
  theme(legend.position = 'bottom',
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        strip.text = element_text(size = theme_get()$text$size - 2 ,face = "bold"),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        legend.margin = margin(-6, 0, 0, 0, 'pt'))
figa8
if(!file.exists(here("figures/appendix", "FigA8_Europarty.pdf"))){
  ggsave(here("figures/appendix", "FigA8_Europarty.pdf"), figa8,
         width = 6, height = 3)
}


# Manipulation checks ====
# """" Table A4 ----
# Left-right placement
dta %>% group_by(Country, treat) %>% 
  summarize(LR_mean = mean(manip_LR, na.rm = T),
            LR_median = median(manip_LR, na.rm = T),
            LR_sd = sd(manip_LR, na.rm = T)) %>%
  mutate(treat = factor(treat, 
                        levels = c("Cont", "Left", "Auto", "Xeno"),
                        labels = c("Control", "Left", "Autocratic", "Xenophobic"))) %>%
  arrange(Country, treat)

# """" Figure A9 & A10 ----
# Policy attribution
manip_policyDE = dtaDE %>% pivot_longer(cols = starts_with('manip_policy'), 
                                        names_to = "policy") %>% 
  group_by(policy, treat, value) %>%
  summarize(count = n()) %>% 
  filter(!is.na(value)) %>%
  group_by(policy, treat) %>%
  mutate(n = sum(count), 
         pct = 100*count/n,
         treat = factor(treat, 
                        levels = c("Xeno", "Auto", "Left", "Cont"),
                        labels = c("Far Right", "Autocratic", "Far Left", "Control"))) %>% 
  arrange(policy, treat)

manip_policyDE$lab = 
  rep(c("Increase the duration of permanent resident status to 20 years in order to apply for citizenship.", 
        "Change regulations on airlines emissions.",
        "Increase the penalty for making false critical remarks and statements 
    by politicians.",
        "Make journalists responsible under criminal law for reporting telephone transcripts.",
        "Make it a legal requirement for doctors to report illegal immigrants to the police.", 
        "Increase compulsory education by one year.",
        "Increase the inheritance tax.",
        "Create a new tax on the wealthy."), each = 16)

figA9_DE = ggplot(manip_policyDE, aes(x = value, y = treat, fill = pct)) +
  geom_tile() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = 'white', high = 'black') +
  facet_wrap(~lab, ncol = 4,
             labeller = label_wrap_gen(width = 20)) + 
  theme_bw() +
  labs(fill = "% of respondents\nper treatment group") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = theme_get()$text$size - 2,
                                  vjust = 0), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(vjust = 1),
        legend.margin = margin(-6, 0, 0, 0, "pt")
  )
figA9_DE
if(!file.exists(here("figures/appendix", "FigA9_PolicyAttributionDE.pdf"))){
  ggsave(here("figures/appendix", "FigA9_PolicyAttributionDE.pdf"), figA9_DE,
         width = 6, height = 4.5)
}

manip_policyIT = dtaIT %>% pivot_longer(cols = starts_with('manip_policy'), 
                                        names_to = "policy") %>% 
  group_by(policy, treat, value) %>%
  summarize(count = n()) %>% 
  filter(!is.na(value)) %>%
  group_by(policy, treat) %>%
  mutate(n = sum(count), 
         pct = 100*count/n,
         treat = factor(treat, 
                        levels = c("Xeno", "Auto", "Left", "Cont"),
                        labels = c("Far Right", "Autocratic", "Far Left", "Control"))) %>% 
  arrange(policy, treat)

manip_policyIT$lab = 
  rep(c("Increase the duration of permanent resident status to 20 years in order to apply for citizenship.", 
        "Change regulations on airlines emissions.",
        "Increase the penalty for making false critical remarks and statements 
    by politicians.",
        "Make journalists responsible under criminal law for reporting telephone transcripts.",
        "Make it a legal requirement for doctors to report illegal immigrants to the police.", 
        "Increase compulsory education by one year.",
        "Increase the inheritance tax.",
        "Create a new tax on the wealthy."), each = 16)

figA10_IT = ggplot(manip_policyIT, aes(x = value, y = treat, fill = pct)) +
  geom_tile() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_gradient(low = 'white', high = 'black') +
  facet_wrap(~lab, ncol = 4,
             labeller = label_wrap_gen(width = 20)) + 
  theme_bw() +
  labs(fill = "% of respondents\nper treatment group") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = theme_get()$text$size - 2,
                                  vjust = 0), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(vjust = 1),
        legend.margin = margin(-6, 0, 0, 0, "pt"))
figA10_IT
if(!file.exists(here("figures/appendix", "FigA10_PolicyAttributionIT.pdf"))){
  ggsave(here("figures/appendix", "FigA10_PolicyAttributionIT.pdf"), figA10_IT,
         width = 6, height = 4.5)
}

# """" Figure A11 ----
# Descriptive Adjectives for new party
## GERMANY
# Add text field to data set
dtaDE$text = manip_descriptorsDE %>% 
  na_if(., "-99") %>% 
  gsub(" ", "-", .) %>% 
  gsub(",", " ", .)

# turn into data feature matrix
dtaDE_dfm = dfm(tokens(corpus(dtaDE)))

# count occurances for each treatment group
mDE = textstat_frequency(dtaDE_dfm, groups = docvars(dtaDE_dfm)$treat) %>%
  mutate(treat = factor(group, levels = c("Cont", "Left", "Auto", "Xeno"),
                        labels = c("Control", "Far Left", "Autocratic", "Far Right")))

# add group sizes to obtain percentages
mDE = left_join(mDE, dtaDE %>% group_by(treat) %>% 
                  summarize(total = n()), by = c("group" = "treat")) %>%
  mutate(pct = frequency/total,
         feature = gsub("-", " ", feature))

pDEadj = ggplot(mDE, aes(x = feature, y = pct, fill = treat)) + 
  geom_bar(position = 'dodge', stat = 'identity',
           width = 0.8) +
  scale_fill_manual(values = c("#000000","#E69F00","#56B4E9","#009E73")) +
  labs(y = "Percentage\nof respondents") + 
  scale_y_continuous(expand = expansion(c(0, 0.1), 0)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        legend.margin = margin(-6, 0, 0, 0, 'pt'))

## ITALY
# Add text field to data set
dtaIT$text = manip_descriptorsIT %>% 
  na_if(., "-99") %>% 
  gsub(" ", "-", .) %>% 
  gsub(",", " ", .)

# turn into data feature matrix
dtaIT_dfm = dfm(tokens(corpus(dtaIT)))

# count occurances for each treatment group
mIT = textstat_frequency(dtaIT_dfm, groups = docvars(dtaIT_dfm)$treat) %>%
  mutate(treat = factor(group, levels = c("Cont", "Left", "Auto", "Xeno"),
                        labels = c("Control", "Far Left", "Autocratic", "Far Right")))

# add group sizes to obtain percentages
mIT = left_join(mIT, dtaIT %>% group_by(treat) %>% 
                  summarize(total = n()), by = c("group" = "treat")) %>%
  mutate(pct = frequency/total,
         feature = gsub("-", " ", feature) %>%
           gsub("[[:blank:]]$", "", .))

pITadj = ggplot(mIT, aes(x = feature, y = pct, fill = treat)) + 
  geom_bar(position = 'dodge', stat = 'identity',
           width = 0.8) +
  scale_fill_manual(values = c("#000000","#E69F00","#56B4E9","#009E73")) +
  labs(y = "Percentage\nof respondents") + 
  scale_y_continuous(expand = expansion(c(0, 0.1), 0)) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom')

pAdj = ggarrange(pDEadj, pITadj, common.legend = T, ncol = 1,
                 legend = 'bottom', align = 'v') + 
  theme(plot.margin = margin(6,6,6,6, 'pt'))
pAdj
if(!file.exists(here("figures/appendix", "FigA11_Adjectives.pdf"))){
  ggsave(here("figures/appendix", "FigA11_Adjectives.pdf"), pAdj,
         width = 6, height = 4)
}

# """" Figure A12 ----
# Resemblance with real party
# Overview Resemblance with real major party
# Germany
dtaDE$resembleRight = 0
dtaDE$resembleRight[grepl('alternative|afd', tolower(manip_resemblesDE))] = 1
dtaDE$resembleLeft = 0
dtaDE$resembleLeft[grepl('link|pds', tolower(manip_resemblesDE))] = 1
dtaDE$resemble = ifelse(dtaDE$reminds == 0 | is.na(dtaDE$reminds), "None",
                        ifelse(dtaDE$resembleLeft == 1, "Linke/PDS", 
                               ifelse(dtaDE$resembleRight == 1, "AfD",
                                      "Other")))
# Italy
dtaIT$resembleRight = 0
dtaIT$resembleRight[grepl('fratelli|fdi|lega', tolower(manip_resemblesIT))] = 1
dtaIT$resembleLeft = 0
dtaIT$resembleLeft[grepl('m5|5s|stelle', tolower(manip_resemblesIT))] = 1
dtaIT$resemble = ifelse(dtaIT$reminds == 0 | is.na(dtaIT$reminds), "None",
                        ifelse(dtaIT$resembleLeft == 1, "M5S", 
                               ifelse(dtaIT$resembleRight == 1, "FdI/Lega",
                                      "Other")))

resemble_sum = bind_rows(dtaDE %>% mutate(country = "Germany"), 
                         dtaIT %>% mutate(country = "Italy")) %>% 
  group_by(country, treat, resemble) %>% 
  summarize(count = n()) %>% 
  group_by(country, treat) %>% 
  mutate(pct = 100*count/sum(count))

resemble_sum$resemble = factor(resemble_sum$resemble, 
                               levels = c("None", "Other", "AfD", "Linke/PDS",
                                          "FdI/Lega", "M5S"))
resemble_sum$treat = factor(resemble_sum$treat, 
                            levels = c("Cont", "Left", "Auto", "Xeno"),
                            labels = c("Control", "Far Left", "Autocratic", "Far Right"))

figA12 = ggplot(resemble_sum, aes(x = resemble, y = pct, 
                                  fill = treat)) + 
  geom_bar(stat = "identity",
           position = "dodge") + 
  labs(x = "Resemblance",
       y = "Percentage of respondents") + 
  scale_fill_manual(values = c("#000000","#E69F00","#56B4E9","#009E73")) +
  scale_y_continuous(expand = expansion(c(0, 0.1), 0)) + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")) + 
  facet_wrap(~country, scales = "free_x", ncol = 1)

figA12
if(!file.exists(here("figures/appendix", "FigA12_ResemblesOverview.pdf"))){
  ggsave(here("figures/appendix", "FigA12_ResemblesOverview.pdf"), figA12,
         width = 6, height = 4.5)
}


# Sample info ====
# """" Table A5 ----
dta %>% filter(Country == "DE") %>%
  group_by(PartySupport) %>%
  summarize(N = n()) %>%
  mutate(Pct = 100*round(N/sum(N), 3))

# """" Table A6 ----
dta %>% filter(Country == "IT") %>%
  group_by(PartySupport) %>%
  summarize(N = n()) %>%
  mutate(Pct = 100*round(N/sum(N), 3))

### 
# Robustness checks ====
###
# """" Figures A13-A16 ----
# High party attachment
# reload data & subset
load(here("data", "KSFR23_data_clean.Rdata"))
dta = dta %>% filter(PartyAttach > 4)
dtaDE = dta %>% filter(Country == 'DE')
dtaIT = dta %>% filter(Country == 'IT')

# run analysis on subset
source(here("KSFR23_analysis_robustness.R"))

# figures
fig2
if(!file.exists(here("figures/appendix", "FigA13_RobHighPartyAttach_Overview.pdf"))){
  ggsave(here("figures/appendix", "FigA13_RobHighPartyAttach_Overview.pdf"), fig2,
         width = 6, height = 3.5)
}
fig5
if(!file.exists(here("figures/appendix", "FigA14_RobHighPartyAttach_LevelEstimates_treat.pdf"))){
  ggsave(here("figures/appendix", "FigA14_RobHighPartyAttach_LevelEstimates_treat.pdf"), fig5,
         width = 6, height = 1.6)
}  
fig3
if(!file.exists(here("figures/appendix", "FigA15_RobHighPartyAttach_LevelEstimates_pooled.pdf"))){
  ggsave(here("figures/appendix", "FigA15_RobHighPartyAttach_LevelEstimates_pooled.pdf"), fig3,
         width = 6, height = 1.5)
}

fig7
if(!file.exists(here("figures/appendix", "FigA16_RobHighPartyAttach_PartyEstimates.pdf"))){
  ggsave(here("figures/appendix", "FigA16_RobHighPartyAttach_PartyEstimates.pdf"), fig7,
         width = 6, height = 4)
}


# """" Figures A17-A20 ----
# High likelihood of coalition
# reload data & subset
load(here("data", "KSFR23_data_clean.Rdata"))
dta = dta %>% filter(manip_collablikely != "VeryLow")
dtaDE = dta %>% filter(Country == 'DE')
dtaIT = dta %>% filter(Country == 'IT')

# run analysis on subset
source(here("KSFR23_analysis_robustness.R"))

# figures
fig2
if(!file.exists(here("figures/appendix", "FigA17_RobCoalLikely_Overview.pdf"))){
  ggsave(here("figures/appendix", "FigA17_RobCoalLikely_Overview.pdf"), fig2,
         width = 6, height = 3.5)
}
fig5
if(!file.exists(here("figures/appendix", "FigA18_RobCoalLikely_LevelEstimates_treat.pdf"))){
  ggsave(here("figures/appendix", "FigA18_RobCoalLikely_LevelEstimates_treat.pdf"), fig5,
         width = 6, height = 1.6)
}
fig3
if(!file.exists(here("figures/appendix", "FigA19_RobCoalLikely_LevelEstimates_pooled.pdf"))){
  ggsave(here("figures/appendix", "FigA19_RobCoalLikely_LevelEstimates_pooled.pdf"), fig3,
         width = 6, height = 1.5)
}
fig7
if(!file.exists(here("figures/appendix", "FigA20_RobCoalLikely_PartyEstimates.pdf"))){
  ggsave(here("figures/appendix", "FigA20_RobCoalLikely_PartyEstimates.pdf"), fig7,
         width = 6, height = 4)
}

# """" Figures A21-A23 ----
#Mainstream party supporters
# reload data & subset
load(here("data", "KSFR23_data_clean.Rdata"))
dta = dta %>% filter(!PartySupport %in% c('AfD', 'LIN', 'LeU', 'FdI'))
dtaDE = dta %>% filter(Country == 'DE')
dtaIT = dta %>% filter(Country == 'IT')

# run analysis on subset
source(here("KSFR23_analysis_robustness.R"))

# figures
fig2
if(!file.exists(here("figures/appendix", "FigA21_RobMainstream_Overview.pdf"))){
  ggsave(here("figures/appendix", "FigA21_RobMainstream_Overview.pdf"), fig2,
         width = 6, height = 3.5)
}
fig5
if(!file.exists(here("figures/appendix", "FigA22_RobMainstream_LevelEstimates_treat.pdf"))){
  ggsave(here("figures/appendix", "FigA22_RobMainstream_LevelEstimates_treat.pdf"), fig5,
         width = 6, height = 1.6)
}
fig3
if(!file.exists(here("figures/appendix", "FigA23_RobMainstream_LevelEstimates_pooled.pdf"))){
  ggsave(here("figures/appendix", "FigA23_RobMainstream_LevelEstimates_pooled.pdf"), fig3,
         width = 6, height = 1.5)
}

# """" Figures A24-A26 ----
# Treatment resembles real party
# (replicate analysis for two groups manually instead of using KSFR21_Analysis_Robustness.R in order to keep both groups in the same figures)

# reload data & subset to respondents who said treatment party reminded them of a real-life party
load(here("data", "KSFR23_data_clean.Rdata"))
dta = dta %>% filter(reminds == 1)
dtaDE = dta %>% filter(Country == 'DE')
dtaIT = dta %>% filter(Country == 'IT')

# run analysis on subset
  source(here("KSFR23_analysis_robustness.R"))

# save relevant plotting data to combine with other group below
  cont1_combined = cont1 %>% mutate(reminds = "1")
  cont2_combined = cont2 %>% mutate(reminds = "1")
  cont3_combined = cont3 %>% mutate(reminds = "1")
  cont4_combined = cont4 %>% mutate(reminds = "1")
  CIs_combined = CIs %>% mutate(reminds = "1")
  mod1.contr4_combined = mod1.contr4 %>% mutate(reminds = "1")
  mod7.contr_combined = mod7.contr %>% mutate(reminds = "1")

# reload data & subset to other group
  load(here("data", "KSFR23_data_clean.Rdata"))
  dta = dta %>% filter(reminds == 0)
  dtaDE = dta %>% filter(Country == 'DE')
  dtaIT = dta %>% filter(Country == 'IT')

# run analysis on other group
  source(here("KSFR23_analysis_robustness.R"))

# save relevant plotting data by adding to *_combined plotting data
  cont1_combined = bind_rows(cont1_combined,
                             cont1 %>% mutate(reminds = "0"))
  cont2_combined = bind_rows(cont2_combined,
                             cont2 %>% mutate(reminds = "0"))
  cont3_combined = bind_rows(cont3_combined,
                             cont3 %>% mutate(reminds = "0"))
  cont4_combined = bind_rows(cont4_combined,
                             cont4 %>% mutate(reminds = "0"))
  CIs_combined = bind_rows(CIs_combined, 
                           CIs %>% mutate(reminds = "0"))
  mod1.contr4_combined = bind_rows(mod1.contr4_combined, 
                                   mod1.contr4 %>% mutate(reminds = "0"))
  mod7.contr_combined = bind_rows(mod7.contr_combined, 
                                  mod7.contr %>% mutate(reminds = "0"))

# generate and save figures  
p1 = ggplot(cont1_combined, aes(y = treat, x = emmean, 
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = reminds)) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 4),
                     breaks = c("0", "1"), labels = c("Resembles real party", "Doesn't resemble real party")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  labs(x = 'Support for collaboration') + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())

# plot attachment change
p2 = ggplot(cont2_combined, aes(y = treat, x = emmean, 
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = reminds)) +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 4),
                     breaks = c("0", "1"), labels = c("Resembles real party", "Doesn't resemble real party")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  #scale_x_continuous(limits = c(0, 10)) + 
  labs(x = 'Change in attachment\nto preferred party') + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())
# plot turnout change
p3 = ggplot(cont3_combined, aes(y = treat, x = emmean,  
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = reminds)) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 4),
                     breaks = c("0", "1"), labels = c("Resembles real party", "Doesn't resemble real party")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  labs(x = 'Turnout propensity') + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())

# plot likelihood to vote for same party
p4 = ggplot(cont4_combined, aes(y = treat, x = emmean,
                       xmin = lower.CL, xmax = upper.CL,
                       color = Country, shape = reminds)) +
  geom_point(size = 1.5, 
             position = position_dodge(0.6)) + 
  geom_linerange(size = 0.7,
                 position = position_dodge(0.6)) +
  scale_color_manual(values = c("#E69F00", "#56B4E9"),
                     breaks = c("DE", "IT"), labels = c("Germany", "Italy")) +
  scale_shape_manual(values = c(19, 4),
                     breaks = c("0", "1"), labels = c("Resembles real party", "Doesn't resemble real party")) +
  scale_y_discrete(labels = c("Far Right", "Autocratic", "Far Left", "Control")) +
  labs(x = 'Propensity to vote for preferred\n party in next federal election') + 
  theme_bw() +
  theme(axis.title.y = element_blank(), 
        legend.position = 'none',
        legend.title = element_blank())

# plot
fig2 = ggarrange(p1, p2, p3, p4, align = 'hv', common.legend = T, legend = 'bottom')
fig2
if(!file.exists(here("figures/appendix", "FigA24_RobReminds_Overview.pdf"))){
  ggsave(here("figures/appendix", "FigA24_RobReminds_Overview.pdf"), fig2,
         width = 6, height = 5)
}

# Figure 3
  fig3 = ggplot(mod1.contr4_combined, 
                aes(x = estimate, y = treat,
                    xmin = lower.CL, xmax = upper.CL, 
                    shape = reminds)) + 
    geom_vline(xintercept = 0, linetype = 2) + 
    geom_point(size = 1.5,
               position = position_dodge(0.6)) + 
    geom_linerange(size = 0.7,
                   position = position_dodge(0.6)) +
    scale_shape_manual(values = c(19, 4),
                       breaks = c("0", "1"), labels = c("Resembles real party", "Doesn't resemble real party")) +
    facet_wrap(~Country, ncol = 2,
               scales = 'free_y', drop = T,
               strip.position = 'top') +
    theme_bw() + 
    labs(x = "Contrast estimate") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = theme_get()$text$size - 2,face = "bold"),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")
  fig3
  if(!file.exists(here("figures/appendix", "FigA25_RobReminds_LevelEstimates_treat.pdf"))){
    ggsave(here("figures/appendix", "FigA25_RobReminds_LevelEstimates_treat.pdf"), fig3,
           width = 6, height = 2.5)
  }

  # Figure 5
  fig5 = ggplot(CIs_combined, aes(x = estimate, xmin = lower.CL, xmax = upper.CL,
                                  y = DV, shape = reminds)) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_point(size = 1.6,
               position = position_dodge(0.6)) + 
    geom_linerange(size = 0.7,
                   position = position_dodge(0.6)) +
    #scale_x_continuous(limits = c(-1, 1), breaks = seq(-.5, .5, .1)) + 
    scale_shape_manual(values = c(19, 4),
                       breaks = c("0", "1"), labels = c("Resembles real party", "Doesn't resemble real party")) +
    labs(x = 'Pooled contrast estimate') +
    theme_bw() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom")
  fig5
  if(!file.exists(here("figures/appendix", "FigA26_RobReminds_LevelEstimates_pooled.pdf"))){
    ggsave(here("figures/appendix", "FigA26_RobReminds_LevelEstimates_pooled.pdf"), fig5,
           width = 6, height = 2.5)
  }
   
  
  
# The Role of EU Knowledge ====
# reload data & subset to other group
  load(here("data", "KSFR23_data_clean.Rdata"))
  dtaDE = dta %>% filter(Country == 'DE')
  dtaIT = dta %>% filter(Country == 'IT')
# """" Figure A27 ----
  # High vs. low EU knowledge respondents
  # fit models
  mod5DE = lm(SuppCollab ~ EUinfo_bin*treat, data = dta, subset = (Country == 'DE'))
  mod5DE.em = emmeans(mod5DE, specs = ~EUinfo_bin*treat, data = mod5DE$model)
  mod5IT = lm(SuppCollab ~ EUinfo_bin*treat, data = dta, subset = (Country == 'IT'))
  mod5IT.em = emmeans(mod5IT, specs = ~EUinfo_bin*treat, data = mod5IT$model)
  
  # obtain contrasts
  mod5DE.contr = contrast(mod5DE.em, method = contrasterInfo(mod5DE.em, 
                                                             c("Cont", "Left", "Auto", "Xeno"),
                                                             rep("Low", 4)),
                          adjust = adjust)
  mod5DE.contr.CI = confint(mod5DE.contr)
  
  mod5IT.contr = contrast(mod5IT.em, method = contrasterInfo(mod5IT.em, 
                                                             c("Cont", "Left", "Auto", "Xeno"),
                                                             rep("Low", 4)),
                          adjust = adjust)
  mod5IT.contr.CI = confint(mod5IT.contr)
  
  # combine
  mod5.contr = bind_rows(mod5DE.contr.CI %>% mutate(Country = 'Germany'), 
                         mod5IT.contr.CI %>% mutate(Country = 'Italy')) %>%
    mutate(treat = substring(contrast, 1, 4),
           PartySupport = "Combined")
  
  # relevel factors
  mod5.contr$treat = factor(mod5.contr$treat, 
                            levels = c("Xeno","Auto","Left", "Cont"),
                            labels = c("Far Right", "Autocratic", "Far Left", "Control"))
  
  # plot
  fig6 = ggplot(data = mod5.contr, aes(x = estimate, y = treat,
                                       xmin = lower.CL, xmax = upper.CL)) +
    geom_vline(xintercept = 0, linetype = 2) + 
    geom_point(size = 1.5) + 
    geom_linerange(size = 0.7) + 
    facet_wrap(~Country,
               scales = 'free_y', drop = T) +
    theme_bw() + 
    labs(x = "Support for collaboration for\nhigh versus low EU knowledge") + 
    theme(legend.position = 'bottom',
          strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          strip.text = element_text(size = theme_get()$text$size - 2 ,face = "bold"),
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          legend.margin = margin(-6, 0, 0, 0, 'pt'))
  fig6
  if(!file.exists(here("figures/appendix", "FigA27_EUinfoEstimates.pdf"))){
    ggsave(here("figures/appendix", "FigA27_EUinfoEstimates.pdf"), fig6,
           width = 6, height = 1.75)
  }
  
# """" Figure A28 ----
  # Difference in treatment effect at national vs. European level: 
  # Across treatments and for DE & IT and level of EU information
  mod1DE = lm(SuppCollab ~ treat*level*EUinfo_bin, dta, subset = (Country == 'DE'))
  mod1DE.em = emmeans(mod1DE, specs = ~treat*level*EUinfo_bin, data = mod1DE$model)
  mod1IT = lm(SuppCollab ~ treat*level*EUinfo_bin, dta, subset = (Country == 'IT'))
  mod1IT.em = emmeans(mod1IT, specs = ~treat*level*EUinfo_bin, data = mod1IT$model)
  
  # contrasts to consider
  intr = expand.grid(treat = c('Left', 'Auto', 'Xeno'),
                     level = c('ForEP'),
                     EUinfo_bin = c("High", "Low"),
                     stringsAsFactors = F)
  
  # compute contrasts and CIs
  mod1DE.contr = contrast(mod1DE.em, 
                          method = contrasterLevelInfo(mod1DE.em, 
                                                       intr$treat, 
                                                       intr$level,
                                                       intr$EUinfo_bin), 
                          adjust = adjust)
  mod1DE.contr.CI = confint(mod1DE.contr)
  mod1IT.contr = contrast(mod1IT.em, 
                          method = contrasterLevelInfo(mod1IT.em, 
                                                       intr$treat, 
                                                       intr$level,
                                                       intr$EUinfo_bin), 
                          adjust = adjust)
  mod1IT.contr.CI = confint(mod1IT.contr)
  
  # combine
  mod1.contr = bind_rows(mod1DE.contr.CI %>% mutate(Country = 'Germany'), 
                         mod1IT.contr.CI %>% mutate(Country = 'Italy')) %>%
    mutate(treat = substring(contrast, 1, 4),
           level = substring(contrast, 5, 9),
           EUinfo_bin = substring(contrast, 10, 15))
  
  # relevel factors
  mod1.contr$treat = factor(mod1.contr$treat, 
                            levels = c("Xeno","Auto","Left"),
                            labels = c("Far Right", "Autocratic", "Far Left"))
  
  # plot
  fig7 = ggplot(mod1.contr, 
                aes(x = estimate, y = treat, color = EUinfo_bin, shape = EUinfo_bin,
                    xmin = lower.CL, xmax = upper.CL)) + 
    geom_vline(xintercept = 0, linetype = 2) + 
    geom_point(size = 1.5, 
               position = position_dodge(0.5)) + 
    geom_linerange(size = 0.7,
                   position = position_dodge(0.5)) +
    scale_color_manual(values = c("#E69F00", "#56B4E9"),
                       name = 'EU knowledge') +
    scale_shape_manual(values = c(19, 15),
                       name = 'EU knowledge') +
    facet_wrap(~Country, ncol = 2,
               scales = 'free_y', drop = T,
               strip.position = 'top') +
    theme_bw() + 
    labs(x = "Contrast estimate") +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = theme_get()$text$size - 2,face = "bold"),
          legend.position = 'bottom',
          legend.margin = margin(-6, 0, 0, 0, 'pt'),
          axis.title.y = element_blank())
  fig7
  if(!file.exists(here("figures/appendix", "FigA28_LevelEUinfoEstimates_treat.pdf"))){
    ggsave(here("figures/appendix", "FigA28_LevelEUinfoEstimates_treat.pdf"), fig7,
           width = 6, height = 1.75)
  }
  
###
# Analysis for Other Outcome Measures
###
# reload data
load(here("data", "KSFR23_data_clean.Rdata"))

# """" Figure A29 ----
# Change in Party Attachment
DV = "PartyAttachChange"
source(here("KSFR23_analysis_otherDVs.R"))

fig3
if(!file.exists(here("figures/appendix", "FigA29_PartyAttach_LevelEstimates_treat.pdf"))){
  ggsave(here("figures/appendix", "FigA29_PartyAttach_LevelEstimates_treat.pdf"), fig3,
         width = 6, height = 1.6)
}

# """" Figure A30 ----
# Turnout
DV = "turnout"
source(here("KSFR23_analysis_otherDVs.R"))

fig3
if(!file.exists(here("figures/appendix", "FigA30_Turnout_LevelEstimates_treat.pdf"))){
  ggsave(here("figures/appendix", "FigA30_Turnout_LevelEstimates_treat.pdf"), fig3,
         width = 6, height = 1.6)
}

# """" Figure A31 ----
# Propensity to Vote for Preferred Party
DV = "sticky"
source(here("KSFR23_analysis_otherDVs.R"))

fig3
if(!file.exists(here("figures/appendix", "FigA31_Vote_LevelEstimates_treat.pdf"))){
  ggsave(here("figures/appendix", "FigA31_Vote_LevelEstimates_treat.pdf"), fig3,
         width = 6, height = 1.6)
}


# Post-Hoc Power Analysis ====
diff2s = seq(0, 2, l = 100) # difference in differences, i.e. contrast effect magnitude

## Completely unpooled
# actual sample size and sd in sample
pop = dta %>% filter(level != "NatEP") %>%
  group_by(Country, treat, level) %>%
  summarize(n = n(),
            sd = sd(SuppCollab, na.rm = T)) %>% # This is equivalent to Welch t-test SE computation
  # Using pooled SE = 2.613 yields similar results
  pivot_wider(., id_cols = c(Country, treat),
              values_from = c(n, sd),
              names_from = c(level)) %>%
  mutate(seMean_NatNat = sd_NatNat / sqrt(n_NatNat),
         seMean_ForEP = sd_ForEP / sqrt(n_ForEP),
         seDiff1 = sqrt(seMean_NatNat^2 + seMean_ForEP^2)) %>% # Difference 1 between levels
  pivot_wider(., id_cols = c(Country),
              values_from = c(seDiff1),
              names_from = c(treat)) %>%
  mutate(seDiff2_Left = sqrt(Cont^2 + Left^2), # Difference 2 between treatments
         seDiff2_Auto = sqrt(Cont^2 + Auto^2),
         seDiff2_Xeno = sqrt(Cont^2 + Xeno^2)) %>%
  pivot_longer(., 
               cols = starts_with("seDiff2"),
               values_to = 'seDiff2')

# compute critical value under H_0 (two-sided) @ alpha = 0.05
crit = qnorm(1 - 0.05/2, 0, pop$seDiff2)

# compute power
powUNPOOLED = sapply(1:nrow(pop), function(i) 1 - pnorm(crit[i], diff2s, pop$seDiff2[i]))

## Pooled treatments & country
dta$treat_bin = ifelse(dta$treat == 'Cont', 'Cont', 'Treat')
sePOOLED = dta %>% filter(level != "NatEP") %>%
  group_by(treat_bin, level) %>%
  summarize(n = n(),
            sd = sd(SuppCollab, na.rm = T)) %>%
  mutate(seMean = sd / sqrt(n)) %>%
  pivot_wider(., id_cols = treat_bin,
              values_from = c(seMean),
              names_from = level) %>%
  mutate(seDiff1 = sqrt(NatNat^2 + ForEP^2)) %>%
  ungroup() %>%
  summarize(sePOOLED = sqrt(sum(seDiff1^2))) %>%
  pull(sePOOLED)

# compute critical value under H_0 (two-sided) @ alpha = 0.05
crit = qnorm(0.975, 0, sePOOLED)

# compute power
powPOOLED = 1 - pnorm(crit, diff2s, sePOOLED)

# """" Figure A32 ----
p.dat = data.frame(x = rep(diff2s, 7),
                   y = c(as.vector(powUNPOOLED), powPOOLED),
                   grp = rep(letters[1:7], each = 100),
                   col = c(rep("Split", 600), rep("Pooled", 100)))
figa32 = ggplot(p.dat, aes(x = x, y = y, color = col, group = grp)) + 
  geom_hline(yintercept = 0.8, lty = 2, col = 'grey40') +
  geom_line() +
  scale_color_manual(values = c("#000000","#E69F00")) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  labs(y = "Power", x = "Effect Size") +
  theme(legend.title = element_blank())

figa32
if(!file.exists(here("figures/appendix", "FigA32_Power.pdf"))){
  ggsave(here("figures/appendix", "FigA32_Power.pdf"), figa32,
         width = 6, height = 3)
}