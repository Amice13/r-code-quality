
## ---------------------------
## 01_analysis_mediation_moderation.R
## ---------------------------

## ---------------------------
## Import packages
## ---------------------------


rm(list = ls())
library(here)
library(dplyr)
library(stargazer)
library(ggplot2)
library(Hmisc)
library(scales)
library(xtable)
library(kableExtra)
library(mediation)

source(here("utils.R"))

## ---------------------------
## Define paths and other parameters
## ---------------------------
TABLE_OUTPUT_DIR = "tables/"
FIG_OUTPUT_DIR = "figures/"
plot_labels = c("Residential", "Online or none")

color_map = c("Residential" = "darkgreen",
              "Online or none" = "wheat4") 


## ---------------------------
## Read data
## ---------------------------
exp_df_wprobs = read.csv(here("analytic_df.csv")) %>%
  mutate(invite_forplot_char = ifelse(randomized_invite == "invite", "Residential",
                                      "Online or none"),
         invite_forplot = factor(invite_forplot_char, 
                                 levels = plot_labels, 
                                 ordered = TRUE))


tx_group = exp_df_wprobs %>%
          filter(randomized_invite == "invite" &
                totalunits_t1t2_all > 0 & !is.na(totalunits_t1t2_all) &
                outcome_weighted_firstyear_gpa > 0) %>%
              mutate(treatment = ifelse(randomized_invite == 
                                          "invite",
                                        1, 0))
control_group = exp_df_wprobs %>%
          filter(randomized_invite == "no_invite" &
                totalunits_t1t2_all > 0 & !is.na(totalunits_t1t2_all) & 
                outcome_weighted_firstyear_gpa > 0) %>%
              mutate(treatment = ifelse(randomized_invite == 
                                          "invite",
                                        1, 0))
analytic_samp = rbind.data.frame(tx_group,
                                 control_group) 


## ---------------------------
## Descriptive relationships
## ---------------------------

ggplot(analytic_samp, aes(x = highest_testscore, y = outcome_weighted_firstyear_gpa_includeSB,
                          group = invite_forplot, color = invite_forplot)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  theme_new(base_size = 24) +
  theme(legend.position = c(0.2, 0.2)) +
  scale_color_manual(values = color_map) +
  guides(size = FALSE) +
  ylab("GPA (includes SB program)") +
  xlab("Highest test score\n(SAT scale)") +
  labs(color = "SB invite") +
  ylim(0, 4)

ggsave(here(sprintf("%s%s", FIG_OUTPUT_DIR, "testscore_moderation.pdf")),
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)

## par college across all outcomes
outcomes_all <- c("outcome_weighted_firstyear_gpa_includeSB",
                  "outcome_academic_leave",
                      "outcome_perc_non100level_t1t2",
                      "outcome_totalunits_t1t2_includesb",
                      "totalunits_t1t2_all",
                     "outcome_perc_units_graded_SB")
                   

parcollege_compare = analytic_samp %>%
        group_by(invite_forplot, moderator_parcollege) %>%
        summarise_at(.vars = outcomes_all,
                    funs(weighted.mean(., w=ra_weight))) %>%
        reshape2::melt(, id.var = c("invite_forplot", "moderator_parcollege"))

## pipeline
pipeline_compare = analytic_samp %>%
        group_by(invite_forplot, moderator_pipeline) %>%
        summarise_at(.vars = outcomes_all,
                    funs(weighted.mean(., w=ra_weight))) %>%
        reshape2::melt(, id.var = c("invite_forplot", "moderator_pipeline"))

## combine
outcomes_compare_pipeline_parcollege = rbind.data.frame(parcollege_compare %>% rename(moderator = moderator_parcollege),
                                                   pipeline_compare %>% rename(moderator = moderator_pipeline)) %>%
        mutate(moderator_ordered = 
        factor(moderator, 
               levels = c("Neither parent college",
                          "Some parent college",
                          "Not pipeline participant",
                          "Pipeline participant"),
               labels = c("Neither par. college",
                      "Some par. college",
                      "No pipeline",
                      "Pipeline"),
               ordered = TRUE),
        outcomes_clean = 
      factor(case_when(grepl("weighted_firstyear", variable) ~ "GPA (with SB)",
                grepl("academic_leave", variable) ~ "Withdrawal", 
                grepl("perc_non100", variable) ~ "% > 100 level",
                grepl("totalunits.*includesb", variable) ~ "Total units (with SB)",
                grepl("totalunits", variable) ~ "Total units (no SB)",
                TRUE ~ "% courses graded"),
             ordered = TRUE)) 


## ---------------------------
## Regressions
## ---------------------------


outcome_labels = c("First-year GPA (includes SB)", 
                   "Withdrawal", 
                  "Perc. courses > 100-level",  "Total units (no SB)", "Total units (with SB)",
                  "Perc. courses graded")


## Test score heterogeneity
all_het_test = lapply(outcomes_all,
                      run_regs_preferred,
                      data = analytic_samp,
                      moderator = "highest_testscore",
                      model = "het")
names(all_het_test) <- outcomes_all

stargazer(all_het_test, type = "latex",
          column.labels  = outcome_labels,
         covariate.labels = c("Invited", "Test score", 
                              "Invited x Test score",
                              "Constant"),
          omit = c("block_id"), report = "vcsp*",
          column.sep.width = "2pt", 
          omit.stat = c("f", "ser"),
         star.cutoffs = c(0.05, 0.01, 0.001),
          dep.var.labels.include = FALSE,
          label = "tab:het_tx_testscore",
          title = "Heterogeneous treatment effects: pre-treatment standardized test scores (recoded to SAT and taking highest score). The table omits the block-specific fixed effects",
          out = paste0(here(TABLE_OUTPUT_DIR), 
                       "het_tx_testscore.tex"))

all_het_parcollege = lapply(outcomes_all,
                      run_regs_preferred,
                      data = analytic_samp,
                      moderator = "moderator_parcollege",
                      model = "het")
names(all_het_parcollege) <- outcomes_all

stargazer(all_het_parcollege, type = "latex",
          column.labels  = outcome_labels,
         covariate.labels = c("Invited", "Some college", 
                              "Invited x Some college",
                              "Constant"),
          omit = c("block_id"), report = "vcsp*",
          column.sep.width = "2pt", 
          omit.stat = c("f", "ser"),
          dep.var.labels.include = FALSE,
          label = "tab:het_tx_parcollege",
         star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Heterogeneous treatment effects: pre-treatment parent educational attainment (some college = one parent attended some college). The table omits the block-specific fixed effects",
          out = paste0(here(TABLE_OUTPUT_DIR), 
                       "het_tx_parcollege.tex"))

all_het_pipeline = lapply(outcomes_all,
                      run_regs_preferred,
                      data = analytic_samp,
                      moderator = "moderator_pipeline",
                      model = "het")
names(all_het_pipeline) <- outcomes_all

stargazer(all_het_pipeline, type = "latex",
          column.labels  = outcome_labels,
         covariate.labels = c("Invited", "Yes pipeline", 
                              "Invited x Yes pipeline",
                              "Constant"),
          omit = c("block_id"), report = "vcsp*",
          column.sep.width = "2pt", 
          omit.stat = c("f", "ser"),
          dep.var.labels.include = FALSE,
          label = "tab:het_tx_pipeline",
         star.cutoffs = c(0.05, 0.01, 0.001),
          title = "Heterogeneous treatment effects: pre-treatment pipeline program participation. The table omits the block-specific fixed effects",
          out = paste0(here(TABLE_OUTPUT_DIR), 
                       "het_tx_pipeline.tex"))


### Mediation 
## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop_med = analytic_samp %>%
      group_by(invite_forplot, mediator_total_helpvisits_t1t2) %>%
      summarise(num  = n()) %>%
      left_join(analytic_samp %>%
      group_by(invite_forplot) %>%
      summarise(denom  = n())) %>%
      mutate(prop = num/denom) %>%
      arrange(mediator_total_helpvisits_t1t2, invite_forplot)


## plot results
ggplot(prop_med,
      aes(x = mediator_total_helpvisits_t1t2, y = prop, group = invite_forplot,
                        fill = invite_forplot)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(breaks = pretty_breaks(n = 10), limits = c(0, 1)) +
  theme_new(base_size = 18) +
  scale_fill_manual(values = color_map) +
  ylab("Proportion of group") +
  theme(legend.position = c(0.8, 0.9),
        legend.background = element_blank()) +
  labs(fill = "SB invite") +
  xlab("Number of visits to academic help center")

ggsave(here(sprintf("%s%s", FIG_OUTPUT_DIR, "helpcenter_descriptive.pdf")),
       plot = last_plot(),
       device = "pdf",
       width = 12,
       height = 8)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## add binary coding
analytic_samp = analytic_samp %>%
        mutate(mediator_anyhelp = mediator_total_helpvisits_t1t2 > 0) 


visits_differences <- nonpar_summary_function(tx_group = analytic_samp %>% filter(treatment == 1),
                        control_group = analytic_samp %>% filter(treatment == 0),
                        outcome_string = "mediator_total_helpvisits_t1t2") %>%
                    mutate(control_lower = control_value - 1.96*control_sem,
                          control_upper = control_value + 1.96*control_sem,
                          tx_lower = tx_value - 1.96*tx_sem,
                          tx_upper = tx_value + 1.96*tx_sem)



med.fit = lm(mediator_total_helpvisits_t1t2 ~ treatment + block_id, data= analytic_samp)
summary(med.fit)

med.fit.binary = lm(mediator_anyhelp ~ treatment + block_id, data= analytic_samp)
summary(med.fit.binary)


out.fit = lm(outcome_perc_non100level_t1t2 ~ mediator_total_helpvisits_t1t2 + treatment + block_id,
             data = analytic_samp)
med.out = mediate(med.fit, out.fit, treat = "treatment", 
                  mediator = "mediator_total_helpvisits_t1t2", sims = 100, weight = ra_weight)

summary(med.out)

