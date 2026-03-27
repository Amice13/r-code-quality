# Install required packages 
# install.packages("remotes")
# library(remotes)
# install_version("reshape2", version = "1.4.4");install_version("dplyr", version = "1.1.4")
# install_version("haven", version = "2.5.4");install_version("stargazer", version = "5.2.3")
# install_version("AER", version = "1.2-12");install_version("sandwich", version = "3.1-0")
# install_version("parallel", version = "4.3.3");install_version("randomizr", version = "1.0.0")
# install_version("ggplot2", version = "3.4.4");install_version("xtable", version = "1.8-4")
# install_version("lfe", version = "2.9-0");install_version("estimatr", version = "1.0.2")
# install_version("texreg", version = "1.39.3");install_version("car", version = "3.1-2")
# install_version("sjmisc", version = "2.8.9");install_version("MASS", version = "7.3-60.0.1")
# install_version("doSNOW", version = "1.0.20");install_version("doRNG", version = "1.8.6")
# install_version("stringr", version = "1.5.0");install_version("ggpubr", version = "0.6.0")


# Load required packages
library(reshape2)
library(dplyr)
library(haven)
library(stargazer)
library(AER)
library(sandwich)
library(parallel)
library(randomizr)
library(ggplot2)
library(xtable)
library(lfe)
library(estimatr)
library(texreg)
library(car)
library(sjmisc)
library(MASS)
library(doSNOW)
library(doRNG)
library(stringr)
library(ggpubr)
# List all objects in the environment
all_objects <- ls()

# Specify the objects to keep
objects_to_keep <- c("lib", 'wd')

# Identify objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)
rm(list = objects_to_remove)


##### Set working directory ########
setwd(wd)


# Create output folders
dir.create("output")
dir.create("output/tables")
dir.create("output/figures")
tab.out = "output/tables/"
fig.out = "output/figures/"



###### FIGURE 5: COMBINED COEFFICIENT PLOTS WITH RECALL SURVEY RESULTS #######
# Survey 1
newmemb_coef = read.csv("output/tables/newmembers_coefficients.csv",
                        stringsAsFactors = F) %>%
  mutate(group = 1) %>%
  mutate(period = 1) 
excluded_coef = read.csv("output/tables/excluded_coefficients.csv",
                         stringsAsFactors = F) %>%
  mutate(group = 2) %>%
  mutate(period = 1) 
excluded_fem_coef = read.csv("output/tables/excluded_fem_coefficients.csv",
                             stringsAsFactors = F) %>%
  mutate(group = 2) %>%
  mutate(period = 1) 
skills_coef = read.csv("output/tables/skills_coefficients.csv",
                       stringsAsFactors = F) %>%
  mutate(group = 3) %>%
  mutate(period = 1) 
skills_fem_coef = read.csv("output/tables/skills_fem_coefficients.csv",
                           stringsAsFactors = F) %>%
  mutate(group = 3)%>%
  mutate(period = 1) 

onboarding_coef = bind_rows(newmemb_coef, excluded_coef,excluded_fem_coef, 
                            skills_coef, skills_fem_coef) %>%
  filter(outcome=="missedcall"|outcome=="excluded_group"|outcome=="skill_index") %>%
  filter(term!="(Intercept)")


# Survey #2
recall_coef = read.csv("output/tables/recall_coefficients.csv", stringsAsFactors = F) %>%
  bind_rows(read.csv("output/tables/recall_fem_coefficients.csv", stringsAsFactors = F)) %>%
  mutate(period = 2) %>%
  mutate(group = case_when(
    outcome=="consented_21" ~ 1,
    outcome=="excluded_group_21" ~ 2, 
    outcome=="skill_index_21" ~ 3
  )) %>%
  mutate(outcome= ifelse(outcome=="consented_21", "missedcall", outcome)) %>%
  mutate(outcome = str_replace(outcome, pattern = "_21", "")) %>%
  filter(term!="(Intercept)")





coef.fig = onboarding_coef %>%
  bind_rows(recall_coef) %>%
  mutate(order = case_when(
    term == "t_poli_takeup" ~ 2,
    term == "t_cand_takeup" ~ 3,
    term == "t_care_takeup" ~ 1,
    term == "t_ideo_takeup" ~ 4,
    term == "t_fem_takeup" ~ 5),
    order = factor(order, labels = c("Career, LATE","Policy, LATE",
                                     "Candidacy, LATE","Ideology, LATE",
                                     "Female, LATE"))) %>%
  mutate(outcome_label = case_when(
    outcome == "missedcall" ~ 1,
    outcome == "excluded_group" ~ 2,
    outcome == "skill_index" ~ 3
  ), outcome_label = factor(outcome_label, labels = c("SIZE: # New Recruits", "TYPE: # Excluded Group",
                                                      "SKILL: # Skilled"))) %>%
  mutate(group_labels = factor(group, labels = c("Number of New Recruits",
                                                 "Diversity of Recruits",
                                                 "Skills of Recruits"))) %>%
  mutate(period = ifelse(period==1, "Onboarding", "Long-term Retention") %>% as.factor()) 



## Plot
ggplot(coef.fig)+
  geom_point(aes(y = order, x = estimate, color=period, shape = period), size = 7, 
             position = position_dodge(width = 0.8))+
  geom_errorbar(aes(y = order,
                    x = estimate, color = period,
                    xmin = estimate-(1.96*se_bootstr),
                    xmax = estimate+(1.96*se_bootstr)), width = 0, size=2,
                position = position_dodge(width = 0.8))+
  geom_hline(yintercept = 4.5, linetype = "dashed",color = "gray")+
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray")+
  facet_wrap(.~outcome_label, as.table=F)+
  labs(y = "Treatment Type", x = "Treatment Effect")+
  scale_color_manual(name = "Survey:", values = c(c("#56B4E9", "#009E73"))) +
  scale_shape_manual(name = "Survey:", values = c(17, 19))+
  theme_pubr()+
  guides(color = guide_legend(reverse=TRUE),
         shape = guide_legend(reverse=TRUE))+
  theme(strip.text.x = element_text(face = "bold", size =22), 
        axis.text = element_text(hjust = 1, size=20),
        axis.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(face = "bold", size = 22),
        legend.text=element_text(size=22),
        legend.title = element_text(size=22),
        legend.key.size =unit(3, "line"))
ggsave(filename = paste0(fig.out, "figure5_main_coefficient_plot.pdf"), 
       width = 15, height = 7)





###### INTERACTION PLOTS #######

###### FIGURE 6: New members #####
newmemb_coef = read.csv("output/tables/newmembers_interaction_coefficients.csv",
                        stringsAsFactors = F) %>%
  mutate(period = "onboarding") %>%
  dplyr::select(-X) 
newmemb_pvalues = newmemb_coef %>%
  filter(str_detect(term, "Joint"))
newmemb_fem_coef = read.csv("output/tables/newmembers_coefficients.csv",
                            stringsAsFactors = F) %>%
  mutate(period = "onboarding") %>%
  dplyr::select(-X) %>%
  filter(term=="t_fem_takeup"|term=="(Intercept)") %>%
  mutate(sample = "male")



# replace intercept estimates with the intercept estimates from baseline pamphlets
newmemb_coef_new = newmemb_coef %>%
  filter(!str_detect(term, "Joint")) %>%
  mutate(se_bootstr_new = ifelse(term=="(Intercept)" , 0, se_bootstr)) %>%
  mutate(sample = ifelse(is.na(sample), "messages", sample)) %>%
  mutate(estimate_new = case_when(
    term=="(Intercept)" ~ estimate,
    term!="(Intercept)" & sample=="messages" ~ 7.53366626+estimate
  ))
  
  

library(stringr)
coefs = newmemb_coef_new %>%
  mutate(intercepts = ifelse(term=="(Intercept)", estimate_new, NA)) %>%
  mutate(female_treat = ifelse(str_detect(term, "female_"), 1, 0)) %>%
  mutate(female_treat_fact = factor(female_treat, labels = c("Male Treatment", "Female Treatment"))) %>%
  mutate(messages = case_when(
    str_detect(term, "Intercept") & sample=="messages" ~ 10,
    str_detect(term, "_ideo_male") & sample=="messages" ~ 9,
    str_detect(term, "_cand_male") & sample=="messages" ~ 8,
    str_detect(term, "_poli_male") & sample=="messages" ~ 7,
    str_detect(term, "_care_male") & sample=="messages" ~ 6,
    str_detect(term, "_base_female") & sample=="messages" ~ 5,
    str_detect(term, "_ideo_female") & sample=="messages" ~ 4,
    str_detect(term, "_cand_female") & sample=="messages" ~ 3,
    str_detect(term, "_poli_female") & sample=="messages" ~ 2,
    str_detect(term, "_care_female") & sample=="messages" ~ 1,
  )) %>%
  mutate(messages_fact = factor(messages, labels = c("Career (F)", "Policy (F)", "Candidacy (F)",
                                                     "Ideology (F)", "Baseline (F)", "Career (M)", 
                                                     "Policy (M)", "Candidacy (M)", "Ideology (M)", 
                                                     "Baseline (M)"))) %>%
  mutate(p_value_text = case_when(
    term=="(Intercept)" ~ paste0("All F vs. All M:\ncoef=", round(newmemb_fem_coef$estimate[newmemb_fem_coef$term=="t_fem_takeup"], 3), ", p=", round(newmemb_fem_coef$p.value[newmemb_fem_coef$term=="t_fem_takeup"], 3)),
    term=="t_cand_male_takeup" ~ paste0("M Benefits vs. Baseline (M):\nJoint Orthogonality\nF-stat=",round(newmemb_pvalues$statistic[newmemb_pvalues$term=="Joint Sign. Male Pamph."], 3),", p=0.000"),
    term=="t_base_female_takeup" ~ paste0("Baseline (F) vs. Baseline (M):\ncoef=", round(newmemb_coef_new$estimate[newmemb_coef_new$term=="t_base_female_takeup"], 3), ", p=", round(newmemb_coef_new$p.value[newmemb_coef_new$term=="t_base_female_takeup"], 3)),
    term=="t_cand_female_takeup" ~ paste0("F Benefits vs. Baseline (F):\nJoint Orthogonality\nF-stat=",round(newmemb_pvalues$statistic[newmemb_pvalues$term=="Joint Sign. Female Pamp."], 3),", p=", round(newmemb_pvalues$p.value[newmemb_pvalues$term=="Joint Sign. Female Pamp."], 3))
  )) %>%
  mutate(p_value_pos = ifelse(!is.na(p_value_text), 20, NA))



library(ggpubr)
ggplot(coefs)+
  geom_hline(aes(yintercept = 7.53366626))+
  geom_point(aes(x = messages_fact, y = estimate_new, color = female_treat_fact), size = 5)+
  geom_errorbar(aes(x = messages_fact, ymax = estimate_new+(1.96*se_bootstr_new), 
                    ymin = estimate_new-(1.96*se_bootstr_new), color = female_treat_fact),
                width = 0, size = 1)+
  geom_text(aes(x = messages_fact, y = p_value_pos, label = p_value_text), color = "black", size = 4,
            position = position_nudge(y = 0), hjust = 0)+
  scale_color_manual(name = "Survey:", values = c("#56B4E9", "#009E73")) +
  coord_flip()+
  labs( y = "# New Recruits", x = "")+
  lims(y = c(-1, 38))+
  theme_pubclean()+
  theme(legend.position = "none", 
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        axis.text = element_text(hjust = 1, size=16),
        axis.title = element_text(face = "bold", size = 16),
        legend.text= element_text(size=16),
        legend.title = element_text(size=16),
        legend.key.size =unit(3, "line"))


ggsave(filename = paste0(fig.out,"figure6_interaction_plot_missedcalls.pdf"), 
       width = 7, height = 8, dpi = 1000)




####### FIGURE G.9: Excluded groups ######
excluded_coef = read.csv("output/tables/excluded_group_interaction_coefficients.csv", stringsAsFactors = F) %>%
  mutate(period = "onboarding") %>%
  dplyr::select(-X) 
excluded_pvalues = excluded_coef %>%
  filter(str_detect(term, "Joint"))
excluded_fem_coef = read.csv("output/tables/excluded_fem_coefficients.csv", stringsAsFactors = F) %>%
  filter(outcome=="excluded_group") %>%
  mutate(period = "onboarding") %>%
  dplyr::select(-X) %>%
  filter(term=="t_fem_takeup"|term=="(Intercept)") %>%
  mutate(sample = "male")



# replace intercept estimates with the intercept estimates from baseline pamphlets
excluded_coef_new = excluded_coef %>%
  filter(!str_detect(term, "Joint")) %>%
  mutate(se_bootstr_new = ifelse(term=="(Intercept)" , 0, se_bootstr)) %>%
  mutate(sample = ifelse(is.na(sample), "messages", sample)) %>%
  mutate(estimate_new = case_when(
    term=="(Intercept)" ~ estimate,
    term!="(Intercept)" & sample=="messages" ~ 5.75608798+estimate
  ))



library(stringr)
coefs = excluded_coef_new %>%
  mutate(intercepts = ifelse(term=="(Intercept)", estimate_new, NA)) %>%
  mutate(female_treat = ifelse(str_detect(term, "female_"), 1, 0)) %>%
  mutate(female_treat_fact = factor(female_treat, labels = c("Male Treatment", "Female Treatment"))) %>%
  mutate(messages = case_when(
    str_detect(term, "Intercept") & sample=="messages" ~ 10,
    str_detect(term, "_ideo_male") & sample=="messages" ~ 9,
    str_detect(term, "_cand_male") & sample=="messages" ~ 8,
    str_detect(term, "_poli_male") & sample=="messages" ~ 7,
    str_detect(term, "_care_male") & sample=="messages" ~ 6,
    str_detect(term, "_base_female") & sample=="messages" ~ 5,
    str_detect(term, "_ideo_female") & sample=="messages" ~ 4,
    str_detect(term, "_cand_female") & sample=="messages" ~ 3,
    str_detect(term, "_poli_female") & sample=="messages" ~ 2,
    str_detect(term, "_care_female") & sample=="messages" ~ 1,
  )) %>%
  mutate(messages_fact = factor(messages, labels = c("Career (F)", "Policy (F)", "Candidacy (F)",
                                                     "Ideology (F)", "Baseline (F)", "Career (M)", 
                                                     "Policy (M)", "Candidacy (M)", "Ideology (M)", 
                                                     "Baseline (M)"))) %>%
  mutate(p_value_text = case_when(
    term=="(Intercept)" ~ paste0("All F vs. All M:\ncoef=",round(excluded_fem_coef$estimate[excluded_fem_coef$term=="t_fem_takeup"], 3), ", p=", round(excluded_fem_coef$p.value[excluded_fem_coef$term=="t_fem_takeup"], 3)),
    term=="t_cand_male_takeup" ~ paste0("M Benefits vs. Baseline (M):\nJoint Orthogonality\nF-stat=",round(excluded_pvalues$statistic[excluded_pvalues$term=="Joint Sign. Male Pamph."], 3),", p=0.000"),
    term=="t_base_female_takeup" ~ paste0("Baseline (F) vs. Baseline (M):\ncoef=",round(excluded_coef_new$estimate[excluded_coef_new$term=="t_base_female_takeup"], 3), ", p=", round(excluded_coef_new$p.value[excluded_coef_new$term=="t_base_female_takeup"], 3)),
    term=="t_cand_female_takeup" ~ paste0("F Benefits vs. Baseline (F):\nJoint Orthogonality\nF-stat=",round(excluded_pvalues$statistic[excluded_pvalues$term=="Joint Sign. Female Pamp."], 3),", p=", round(excluded_pvalues$p.value[excluded_pvalues$term=="Joint Sign. Female Pamp."], 3))
  )) %>%
  mutate(p_value_pos = ifelse(!is.na(p_value_text), 20, NA))



library(ggpubr)
ggplot(coefs)+
  geom_hline(aes(yintercept = 5.75608798))+
  geom_point(aes(x = messages_fact, y = estimate_new, color = female_treat_fact), size = 5)+
  geom_errorbar(aes(x = messages_fact, ymax = estimate_new+(1.96*se_bootstr_new), 
                    ymin = estimate_new-(1.96*se_bootstr_new), color = female_treat_fact),
                width = 0, size = 1)+
  geom_text(aes(x = messages_fact, y = p_value_pos, label = p_value_text), color = "black", size = 4,
            position = position_nudge(y = 0), hjust = 0)+
  scale_color_manual(name = "Survey:", values = c("#56B4E9", "#009E73")) +
  coord_flip()+
  labs( y = "# Excluded Group", x = "")+
  lims(y = c(-1, 38))+
  theme_pubclean()+
  theme(legend.position = "none", 
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        axis.text = element_text(hjust = 1, size=16),
        axis.title = element_text(face = "bold", size = 16),
        legend.text= element_text(size=16),
        legend.title = element_text(size=16),
        legend.key.size =unit(3, "line"))


ggsave(filename = paste0(fig.out,"figureG9_interaction_plot_excluded.pdf"), 
       width = 7, height = 8, dpi = 1000)



####### FIGURE G.9: Skilled groups ######
skilled_coef = read.csv("output/tables/skill_index_interaction_coefficients.csv", stringsAsFactors = F) %>%
  mutate(period = "onboarding") %>%
  dplyr::select(-X) 

skilled_pvalues = skilled_coef %>%
  filter(str_detect(term, "Joint"))
skilled_fem_coef = read.csv("output/tables/skills_fem_coefficients.csv", stringsAsFactors = F) %>%
  filter(outcome=="skill_index") %>%
  mutate(period = "onboarding") %>%
  dplyr::select(-X) %>%
  filter(term=="t_fem_takeup"|term=="(Intercept)") %>%
  mutate(sample = "male")



# replace intercept estimates with the intercept estimates from baseline pamphlets
skilled_coef_new = skilled_coef %>%
  filter(!str_detect(term, "Joint")) %>%
  mutate(se_bootstr_new = ifelse(term=="(Intercept)" , 0, se_bootstr)) %>%
  mutate(sample = ifelse(is.na(sample), "messages", sample)) %>%
  mutate(estimate_new = case_when(
    term=="(Intercept)" ~ estimate,
    term!="(Intercept)" & sample=="messages" ~ 7.3860958+estimate
  ))



library(stringr)
coefs = skilled_coef_new %>%
  mutate(intercepts = ifelse(term=="(Intercept)", estimate_new, NA)) %>%
  mutate(female_treat = ifelse(str_detect(term, "female_"), 1, 0)) %>%
  mutate(female_treat_fact = factor(female_treat, labels = c("Male Treatment", "Female Treatment"))) %>%
  mutate(messages = case_when(
    str_detect(term, "Intercept") & sample=="messages" ~ 10,
    str_detect(term, "_ideo_male") & sample=="messages" ~ 9,
    str_detect(term, "_cand_male") & sample=="messages" ~ 8,
    str_detect(term, "_poli_male") & sample=="messages" ~ 7,
    str_detect(term, "_care_male") & sample=="messages" ~ 6,
    str_detect(term, "_base_female") & sample=="messages" ~ 5,
    str_detect(term, "_ideo_female") & sample=="messages" ~ 4,
    str_detect(term, "_cand_female") & sample=="messages" ~ 3,
    str_detect(term, "_poli_female") & sample=="messages" ~ 2,
    str_detect(term, "_care_female") & sample=="messages" ~ 1,
  )) %>%
  mutate(messages_fact = factor(messages, labels = c("Career (F)", "Policy (F)", "Candidacy (F)",
                                                     "Ideology (F)", "Baseline (F)", "Career (M)", 
                                                     "Policy (M)", "Candidacy (M)", "Ideology (M)", 
                                                     "Baseline (M)"))) %>%
  mutate(p_value_text = case_when(
    term=="(Intercept)" ~ paste0("All F vs. All M:\ncoef=",round(skilled_fem_coef$estimate[skilled_fem_coef$term=="t_fem_takeup"], 3),", p=", round(skilled_fem_coef$p.value[skilled_fem_coef$term=="t_fem_takeup"], 3)),
    term=="t_cand_male_takeup" ~ paste0("M Benefits vs. Baseline (M):\nJoint Orthogonality\nF-stat=",round(skilled_pvalues$statistic[skilled_pvalues$term=="Joint Sign. Male Pamph."], 3),", p=0.000"),
    term=="t_base_female_takeup" ~ paste0("Baseline (F) vs. Baseline (M):\ncoef=",round(skilled_coef_new$estimate[skilled_coef_new$term=="t_base_female_takeup"], 3),", p=", round(skilled_coef_new$p.value[skilled_coef_new$term=="t_base_female_takeup"], 3)),
    term=="t_cand_female_takeup" ~ paste0("F Benefits vs. Baseline (F):\nJoint Orthogonality\nF-stat=",round(skilled_pvalues$statistic[skilled_pvalues$term=="Joint Sign. Female Pamp."], 3),", p=", round(skilled_pvalues$p.value[skilled_pvalues$term=="Joint Sign. Female Pamp."], 3))
  )) %>%
  mutate(p_value_pos = ifelse(!is.na(p_value_text), 20, NA))



library(ggpubr)
ggplot(coefs)+
  geom_hline(aes(yintercept = 7.3860958))+
  geom_point(aes(x = messages_fact, y = estimate_new, color = female_treat_fact), size = 5)+
  geom_errorbar(aes(x = messages_fact, ymax = estimate_new+(1.96*se_bootstr_new), 
                    ymin = estimate_new-(1.96*se_bootstr_new), color = female_treat_fact),
                width = 0, size = 1)+
  geom_text(aes(x = messages_fact, y = p_value_pos, label = p_value_text), color = "black", size = 4,
            position = position_nudge(y = 0), hjust = 0)+
  scale_color_manual(name = "Survey:", values = c("#56B4E9", "#009E73")) +
  coord_flip()+
  labs( y = "# Skilled", x = "")+
  lims(y = c(-1, 38))+
  theme_pubclean()+
  theme(legend.position = "none", 
        panel.border = element_blank(),
        strip.text = element_text(size = 12),
        panel.grid.minor = element_blank(),
        axis.text = element_text(hjust = 1, size=16),
        axis.title = element_text(face = "bold", size = 16),
        legend.text= element_text(size=16),
        legend.title = element_text(size=16),
        legend.key.size =unit(3, "line"))


ggsave(filename = paste0(fig.out,"figureG9_interaction_plot_skilled.pdf"), 
       width = 7, height = 8, dpi = 1000)



