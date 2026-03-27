# This R script conducts the analysis reported in the main manuscript.
# To guarantee version control, it uses groundhog (verion 3.2.0). 
library("groundhog") # Version control
pkgs <- c("Hmisc", # Weighted mean and variance
          "tidyverse", # Data management
          "ggplot2", # Visualization
          "estimatr", # Robust SE
          "furniture", # row-means for additive scales
          "pollster", # weighted cross-tables
          "naniar", # Missing data tables
          "patchwork", # Combine ggplot figures
          "ggrepel", # Nicely placed labels in figures
          "spatstat", # A dependency of the sourced function
          "DescTools", # Weighted quartiles
          "ggpubr", # Combine plots
          "gtools") # Add stars indicating p-value
groundhog.library(pkgs, "2024-10-15", include.suggests = TRUE)
source("1b-fun.R") # Self-written functions
load("CleanedData.RData") # Read the cleaned data

sink("logfile.txt") # Start the "logfile"
# Missing values
################
print("Tables of missing values by variable")
print("Table 1: 50% of the sample who had outcomes surveyed right away")
Dat_wave1 %>%
  filter(wave == 1 & !is.na(problem_a_f_1_resp)) %>%
  dplyr::select(
    recogni, reason, blind_appl_w1, eqltrt, donate, # outcomes
    Alder, Region, koen, educ, hh_size, empl) %>% # Controls
  miss_var_summary(digits = 0)

print("Table 2: 50% of the sample who had outcomes surveyed one to three weeks later")
Dat_wave2 %>%
  filter(wave == 2) %>%
  dplyr::select(
    recogni, blind_appl_w1, blind_appl_w2, eqltrt, # outcomes
    Alder, Region, koen, educ, hh_size, empl) %>% # Controls
  miss_var_summary(digits = 0)

# Misperceptions about the extent of ethno-racial discrimination
################################################################
print("Cross-table of types of misperceivers by field experiment")
Dat %>% mutate(
  bias = case_when(
    guess1_bias <= 0 ~ "Accurtate or Inflated",
    (tr_study == "Hiring" & guess1 > 22 & guess1 < 34) |
      (tr_study == "Housing" & guess1 > 29 & guess1 < 37) |
      (tr_study == "Politicians" & guess1 > 55 & guess1 < 71) |
      (tr_study == "Primary schools" & guess1 > 15 & guess1 < 25) ~ "Underperceived",
    (tr_study == "Hiring" & guess1 == 34) |
      (tr_study == "Housing" & guess1 == 37) |
      (tr_study == "Politicians" & guess1 == 71) |
      (tr_study == "Primary schools" & guess1 == 25) ~ "Fair",
    (tr_study == "Hiring" & guess1 > 34) |
      (tr_study == "Housing" & guess1 > 37) |
      (tr_study == "Politicians" & guess1 > 71) |
      (tr_study == "Primary schools" & guess1 > 25) ~ "Disadvantaged",
    TRUE ~ as.character(NA))) %>%
  crosstab(df = ., x = bias, y = tr_study, weight = wgt, pct_type = "column")
sink() # Stop the "logfile"

# Figure 3
##########
## Panel A: Work
plot1 <- ggplot(data = Dat %>% filter(tr_study == "Hiring"), 
                aes(x = guess1, fill = tr_study)) +
  geom_histogram(aes(weights = ((wgt / sum(wgt)) * 100)), alpha = 3/4) + 
  scale_fill_manual(values = "#901A1E") +
  labs(y = "", title = "(A) Hiring\n",
       x = "Expected number of positive replies to\n100 Muslim-named ficticious applications\n") +
  geom_segment(x = 22, y = 0, xend = 22, yend = 17.5, linetype = "longdash") +
  geom_text(x = 52, y = 24, label = "Audit result for\nMuslim names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 39, y = 24, xend = 22, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  geom_segment(x = 34, y = 0, xend = 34, yend = 17.5) +
  geom_text(x = 69, y = 17.5, label = "Audit result for\nDanish names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 56, y = 17.5, xend = 35, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 5, 10, 15, 20, 25),
                     labels = c("0", "5", "10", "15", "20", "25%")) +
  scale_x_continuous(breaks = c(0, 10, 22, 34, 40, 50, 60, 70, 80, 90, 100)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

## Panel B: Housing
plot2 <- ggplot(data = Dat %>% filter(tr_study == "Housing"), 
                aes(x = guess1, fill = tr_study)) +
  geom_histogram(aes(weights = ((wgt / sum(wgt)) * 100)), alpha = 3/4) + 
  geom_segment(x = 29, y = 0, xend = 29, yend = 17.5, linetype = "longdash") +
  geom_text(x = 59, y = 24, label = "Audit result for\nMuslim names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 46, y = 24, xend = 29, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  geom_segment(x = 37, y = 0, xend = 37, yend = 17.5) +
  geom_text(x = 73, y = 17.5, label = "Audit result for\nDanish names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 60, y = 17.5, xend = 37, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  scale_fill_manual(values = "#122947") +
  labs(y = "", title = "(B) Housing\n",
       x = "Expected number of positive replies to\n100 Muslim-named ficticious applications\n") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25),
                     labels = c("0", "5", "10", "15", "20", "                     25%")) +
  scale_x_continuous(breaks = c(0, 10, 20, 29, 37, 50, 60, 70, 80, 90, 100)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

## Panel C: Politics
plot3 <- ggplot(data = Dat %>% filter(tr_study == "Politicians"), 
                aes(x = guess1, fill = tr_study)) +
  geom_histogram(aes(weights = ((wgt / sum(wgt)) * 100)), alpha = 3/4) + 
  geom_segment(x = 55, y = 0, xend = 55, yend = 17.5, linetype = "longdash") +
  geom_text(x = 85, y = 24, label = "Audit result for\nMuslim names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 72, y = 24, xend = 55, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  geom_segment(x = 71, y = 0, xend = 71, yend = 17.5) +
  geom_text(x = 95, y = 17.5, label = "Audit result for\nDanish names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 82, y = 17.5, xend = 71, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  scale_fill_manual(values = "#39641c") +
  labs(y = "", title = "(C) Politicians\n",
       x = "Expected number of positive replies to\n100 Muslim-named ficticious applications\n") +
  scale_y_continuous(limits = c(0, 25), breaks = c(0, 5, 10, 15, 20, 25),
                     labels = c("0", "5", "10", "15", "20", "25%")) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 55, 60, 71, 80, 90, 100)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

## Panel D: Schools
plot4 <- ggplot(data = Dat %>% filter(tr_study == "Primary schools"), 
                aes(x = guess1, fill = tr_study)) +
  geom_histogram(aes(weights = ((wgt / sum(wgt)) * 100)), alpha = 3/4) + 
  geom_segment(x = 15, y = 0, xend = 15, yend = 17.5, linetype = "longdash") +
  geom_text(x = 45, y = 24, label = "Audit result for\nMuslim names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 32, y = 24, xend = 15, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  geom_segment(x = 25, y = 0, xend = 25, yend = 17.5) +
  geom_text(x = 60, y = 17.5, label = "Audit result for\nDanish names", 
            size = 3.3, lineheight = 0.9) +
  geom_curve(x = 47, y = 17.5, xend = 25, yend = 18, arrow = arrow(length = unit(0.1, "inches"), type = "closed"), size = 0.3) +
  scale_fill_manual(values = "#0a5963") +
  labs(y = "", title = "(D) Primary schools\n",
       x = "Expected number of positive replies to\n100 Muslim-named ficticious applications\n") +
  scale_y_continuous(breaks = c(0, 5, 10, 15, 20, 25),
                     labels = c("0", "5", "10", "15", "20", "                     25%")) +
  scale_x_continuous(breaks = c(0, 10, 15, 25, 30, 40, 50, 60, 70, 80, 90, 100)) +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

## Panel E: CDF
plot5 <- ggplot(data = Dat, aes(x = guess1_bias, color = tr_study, weight = wgt)) +
  geom_vline(xintercept = 0) +
  stat_ecdf(size = 1.1, alpha = 3/4) + 
  labs(color = "",
       # lty = "Correspondence \n experiment",
       y = "Cumulative share", x = "", title = "(E) Bias in expected return rates\n") + 
  scale_color_manual(values = c("#901A1E", "#122947", "#39641c", "#0a5963")) +
  scale_x_continuous(breaks = c(-60, -40, -20, 0, 20, 40, 60, 80),
                     labels = c("-60", "-40\nOver-perception of\nethnic discrimination",
                                "-20", "0\nAccurate \n perception", "20", "40\nUnder-perception of\nethnic discrimination", 
                                "60", "80")) + 
  theme_classic() +
  theme(legend.position = c(0.85, 0.25),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

## Panel F: OLS Regression
plot6 <- lm_robust(guess1_bias ~ tr_study + mod_incineq + mod_immibad + mod_homo + 
                     media + Alder + koen + educ + empl, 
                   data = Dat, weights = wgt) %>%
  tidy() %>% mutate(
    term = case_when(
      term == "mod_incineq" ~ "Reduce income\ninequality",
      term == "mod_immibad" ~ "Immigration makes\nDK worse place",
      term == "mod_homo" ~ "Same rights for\ngay couples",
      term == "media" ~ "News media\nconsumption",
      term == "Alder" ~ "Age",
      term == "koenMand" ~ "Gender\nMen",
      term == "educMedium" ~ "Education\nMedium",
      term == "educHigh" ~ "High",
      term == "emplPart time" ~ "Employment\nPart time",
      term == "emplNot employed" ~ "Not employed") %>%
      fct_relevel("High", "Education\nMedium", "Not employed",
                  "Employment\nPart time",
                  "Gender\nMen", "Age",
                  "News media\nconsumption",
                  "Same rights for\ngay couples", "Reduce income\ninequality"),
    min90 = estimate - qt(0.95, df = df) * std.error,
    max90 = estimate + qt(0.95, df = df) * std.error) %>%
  dplyr::filter(term != "(Intercept)" & term != "tr_studyHousing" &
                  term != "tr_studyPolitics" & term != "tr_studySchool") %>%
  ggplot(data = ., 
         aes(y = estimate, x = term, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "#901A1E") +
  geom_linerange(size = 1.3) +
  geom_linerange(aes(ymin = min90, ymax = max90), size = 1.3, color = "#808080") +
  geom_point(size = 2.5) +
  coord_flip() +
  scale_y_continuous(breaks = c(-5, -2.5, 0, 2.5), limits = c(-6.5, 2.6),
                     labels = c("-7.5", "-5", "-2.5\nBias of the\nreference group", "0")) + 
  labs(y = "", x = "", title = "(F) Regression of bias in expected return rate\n") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2),
        axis.text = element_text(colour = "black"))

## Putting them all together
plot <- (plot1 | plot2) / (plot3 | plot4) /  (plot5 | plot6) +
  plot_layout(heights = c(1, 1, 1))

## Save for the article
ggsave(plot, file = "Fig3_Misperceptions.png", width = 10.5, height = 13, dpi = "retina")

# Figure 4: Learning
####################
Dat_beliefs <- Dat %>% # Identify sub-sample
  filter(tr_crction == "No correction" |
           tr_crction == "Correction") %>%
  mutate(tr_crction = fct_drop(tr_crction))

# Panel A
plot_a <- ggplot(aes(y = guess2_bias, x = guess1_bias, 
                     lty = tr_crction, shape = tr_crction,
                     color = tr_crction, fill = tr_crction), 
                 data = Dat_beliefs) +
  geom_point(aes(size = wgt), alpha = 1/2) +
  scale_shape_manual(values = c(1, 2)) +
  scale_color_manual(values = c("#901A1E", "#122947")) +
  scale_fill_manual(values = c("#901A1E", "#122947")) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(weight = wgt), method = "lm") +
  scale_y_continuous(limits = c(-56, 86)) + 
  theme_classic() +
  labs(title = "(A) Linear fit",
       y = "Subsequent misperception about another\nfield experiment at the end of the survey", 
       x = "Initial misperception at the outset of the survey") +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

# Panel B
plot_b <- ggplot(aes(y = guess2_bias, x = guess1_bias, 
                     lty = tr_crction, shape = tr_crction,
                     color = tr_crction, fill = tr_crction), 
                 data = Dat_beliefs) +
  scale_shape_manual(values = c(1, 2)) +
  scale_color_manual(values = c("#901A1E", "#122947")) +
  scale_fill_manual(values = c("#901A1E", "#122947")) +
  geom_abline(intercept = 0, slope = 1) +
  geom_hline(yintercept = 0) +
  geom_smooth(aes(weight = wgt), method = "lm", se = FALSE, linewidth = 0.5) +
  geom_smooth(aes(weight = wgt), method = "gam", se = FALSE) +
  scale_y_continuous(limits = c(-56, 86)) + 
  labs(title = "(B) Linear versus non-linear fit",
       y = "", 
       shape = "", lty = "",
       color = "", 
       x = "Initial misperception at the outset of the survey") +
  guides(size = "none", fill = "none") +
  theme_classic() +
  theme(legend.position = c(0.83, 0.12),
        plot.title = element_text(hjust = 0.5),
        axis.text = element_text(colour = "black"))

## Putting them all together
patch <- (plot_a | plot_b) +
  plot_layout(axes = "collect", widths = c(1, 1))

## Save for the article
ggsave(patch, file = "Fig4_Learning.png", width = 10, height = 5.5, dpi = "retina")

# Regressions for Figures 5 to 12
#################################
# Run post-stratification weighted OLS for whole sample and for underperceivers
recogni_a <- mrln_estimtr(y = "z1_recogni", pred = "tr_crction",
                            add_cntr =" + z_guess1_bias + tr_study",
                            daten = Dat_wave1)
recogni_b <- mrln_estimtr(y = "z1_recogni", pred = "tr_crction",
                            add_cntr =" + z_guess1_bias + tr_study",
                            daten = Dat_w1_uperc)
recogni_c <- mrln_estimtr(y = "z0_recogni", pred = "tr_crction",
                            daten = Dat_wave1)
reason_a <- mrln_estimtr(y = "z1_reason", pred = "tr_crction",
                           add_cntr =" + z_guess1_bias + tr_study",
                           daten = Dat_wave1)
reason_b <- mrln_estimtr(y = "z1_reason", pred = "tr_crction",
                           add_cntr =" + z_guess1_bias + tr_study",
                           daten = Dat_w1_uperc)
reason_c <- mrln_estimtr(y = "z0_reason", pred = "tr_crction",
                           daten = Dat_wave1)
blind_appl_a <- mrln_estimtr(y = "z1_blind_appl_w1", pred = "tr_crction",
                               add_cntr =" + z_guess1_bias + tr_study",
                               daten = Dat_wave1)
blind_appl_b <- mrln_estimtr(y = "z1_blind_appl_w1", pred = "tr_crction",
                               add_cntr =" + z_guess1_bias + tr_study",
                               daten = Dat_w1_uperc)
blind_appl_c <- mrln_estimtr(y = "z0_blind_appl_w1", pred = "tr_crction",
                               daten = Dat_wave1)
eqltrt_a <- mrln_estimtr(y = "z1_eqltrt", pred = "tr_crction",
                           add_cntr =" + z_guess1_bias + tr_study",
                           daten = Dat_wave1)
eqltrt_b <- mrln_estimtr(y = "z1_eqltrt", pred = "tr_crction",
                           add_cntr =" + z_guess1_bias + tr_study",
                           daten = Dat_w1_uperc)
eqltrt_c <- mrln_estimtr(y = "z0_eqltrt", pred = "tr_crction",
                           daten = Dat_wave1)
donate_a <- mrln_estimtr(y = "z1_donate", pred = "tr_crction",
                           add_cntr =" + z_guess1_bias + tr_study",
                           daten = Dat_wave1)
donate_b <- mrln_estimtr(y = "z1_donate", pred = "tr_crction",
                           add_cntr =" + z_guess1_bias + tr_study",
                           daten = Dat_w1_uperc)
donate_c <- mrln_estimtr(y = "z0_donate", pred = "tr_crction",
                           daten = Dat_wave1)

sink("logfile.txt", append = TRUE) # Start the "logfile" again.

print("Treatment effects on donations to Immigrant Women's Center")
mrln_estimtr(y = "donate", pred = "tr_crction",
                         daten = Dat_wave1) %>% summary()
sink() # Stop the "logfile"

## Delayed outcomes
recogni_d <- mrln_estimtr(y = "z0_recogni", pred = "tr_crction",
                            daten = Dat_wave2)
blind_appl_d <- mrln_estimtr(y = "z0_blind_appl_w2", pred = "tr_crction",
                               daten = Dat_wave2)
eqltrt_d <- mrln_estimtr(y = "z0_eqltrt", pred = "tr_crction",
                           daten = Dat_wave2)

# Figures 5 to 9: Effect of audit result
########################################
# 5.1 Left side: Likert scale plots
Likert <- Dat_wave1 %>% # Generate a long dataset with weighted percentages 
  # per variable and answer category. Only use the control group for the Likert plots
  filter(tr_crction == "No correction") %>%
  # Keep only outcomes for the plots
  dplyr::select(Id, wgt, prob_ethn1, prob_ethn2, prob_ethn3,
                conseq1, conseq2, reason1, reason2, reason3,
                blind_appl_w1, eqltrt1, eqltrt2, eqltrt3, eqltrt4) %>%
  pivot_longer(!c(Id, wgt), # Reshape to long 
               names_to = "Variable", values_to = "Answer") %>%
  drop_na() %>% # Drop missing values
  group_by(Variable, Answer) %>%
  dplyr::summarize( # Get weighted number of cases per category
    n = sum(wgt, na.rm = TRUE))

Likert <- Likert %>% 
  group_by(Variable) %>%
  dplyr::summarize( # Get overall number of weighted cases
    all = sum(n)) %>%
  left_join(Likert, ., by = "Variable") %>%
  mutate(
    perc = (n / all), # Get weighted percentages
    Variable = case_when( # Rename
      Variable == "prob_ethn1" ~ "Is a serious problem",
      Variable == "prob_ethn2" ~ "Is widespread",
      Variable == "prob_ethn3" ~ "Is unjust",
      Variable == "conseq1" ~ "Discrimination of\nnon-Western minorities...\nHas bad consequences",
      Variable == "conseq2" ~ "Is one of the main\ncauses of ethnic inequalities",
      Variable == "reason1" ~ "Wrong stereotypes",
      Variable == "reason2" ~ "Clash between Non-Western\nand Danish norms and values",
      Variable == "reason3" ~ "Bad experiences with\nnon-Western minorities",
      Variable == "blind_appl_w1" ~ "Mandatory\nname-blind applications",
      Variable == "eqltrt1" ~ "Finance more research\non discrimination",
      Variable == "eqltrt2" ~ "More awareness raising\namong the public",
      Variable == "eqltrt3" ~ "Anti-discrimination office\noffering legal support",
      Variable == "eqltrt4" ~ "Targets for the\nrepresentation of minorities"),
    Answer = case_when( # Rename answer categories
      Answer == 1 ~ "Strongly disagree",
      Answer == 2 ~ "Disagree",
      Answer == 3 ~ "Neutral",
      Answer == 4 ~ "Agree",
      Answer == 5 ~ "Strongly agree") %>%
      fct_relevel("Strongly agree", "Agree", "Neutral", 
                  "Disagree", "Strongly disagree"))

## Figure 5, Panel A
plot_1 <- Likert %>%
  filter(Variable == "Is unjust" |
           Variable == "Is widespread" |
           Variable == "Is a serious problem" |
           Variable == "Discrimination of\nnon-Western minorities...\nHas bad consequences" |
           Variable == "Is one of the main\ncauses of ethnic inequalities") %>%
  mrln_Likert() +
  labs(title = "(A) Outcome distributions") +
  scale_x_continuous("", limits = c(-0.6, 0.75), 
                     labels = c("-50%", "-25%", "0%", "25%", "50%", "75%"), 
                     breaks=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
  theme(legend.position = "bottom")

## Figure 6, Panel A
plot_3 <- Likert %>%
  filter(Variable == "Wrong stereotypes" |
           Variable == "Clash between Non-Western\nand Danish norms and values" |
           Variable == "Bad experiences with\nnon-Western minorities") %>%
  mrln_Likert() +
  labs(title = "(A) Outcome distributions") +
  scale_x_continuous("", limits = c(-0.3, 0.9), 
                     labels = c("-25%", "0%", "25%", "50%", "75%"), 
                     breaks=c(-0.25, 0, 0.25, 0.5, 0.75)) +
  theme(legend.position = "bottom")

## Figure 7, Panel A
plot_5 <- Likert %>%
  filter(Variable == "Mandatory\nname-blind applications") %>%
  mrln_Likert() +
  labs(title = "(A) Outcome distribution") +
  scale_x_continuous("", limits = c(-0.65, 0.4), 
                     labels = c("-50%", "-25", "0%", "25%"), 
                     breaks=c(-0.5, -0.25, 0, 0.25)) +
  theme(legend.position = "bottom")

## Figure 8, Panel A
plot_7 <- Likert %>%
  filter(Variable == "Finance more research\non discrimination" |
           Variable == "More awareness raising\namong the public" |
           Variable == "Anti-discrimination office\noffering legal support" |
           Variable == "Targets for the\nrepresentation of minorities") %>%
  mrln_Likert() +
  labs(title = "(A) Outcome distributions") +
    scale_x_continuous("", limits = c(-0.5, 0.75), 
                     labels = c("-50%", "-25", "0%", "25%", "50%", "75%"), 
                     breaks=c(-0.5, -0.25, 0, 0.25, 0.5, 0.75)) +
  theme(legend.position = "bottom")

## Figure 9, Panel A
plot_9 <- ggplot(data = Dat_wave1 %>% filter(tr_crction == "No correction"),
                 aes(x = donate)) +
  geom_histogram(aes(weights = ((wgt / sum(wgt)) * 100)), 
                 alpha = 3/4, fill = "#901A1E") + 
  scale_y_continuous(breaks = c(0, 10, 20, 30), labels = c("0", "10", "20", "30%")) +
  labs(y = "", x = "Danish Kroner", title = "(A) Outcome distribution") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = -40))

# 5.2 Right side: Effect plots
## Figure 5, Panel B
plot_2 <- bind_rows(
  recogni_a %>% tidy(),
  recogni_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection") %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection" ~ ""),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects(NoAxis = TRUE, fxscale = FALSE) +
  labs(y = "", x = "", title = "(B) Treatment effect of audit study result") +
  theme(legend.position = "bottom")

## Figure 6, Panel B
plot_4 <- bind_rows(
  reason_a %>% tidy(),
  reason_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection") %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection" ~ ""),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects(NoAxis = TRUE, fxscale = FALSE) +
  labs(y = "", x = "", title = "(B) Treatment effect of audit study result") +
  theme(legend.position = "bottom")

## Figure 7, Panel B
plot_6 <- bind_rows(
  blind_appl_a %>% tidy(),
  blind_appl_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection") %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection" ~ ""),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects(NoAxis = TRUE, fxscale = FALSE) +
  labs(y = "", x = "", title = "(B) Treatment effect of audit study result") +
  theme(legend.position = "bottom")

## Figure 8, Panel B
plot_8 <- bind_rows(
  eqltrt_a %>% tidy(),
  eqltrt_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection") %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection" ~ ""),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects(NoAxis = TRUE, fxscale = FALSE) +
  labs(y = "", x = "", title = "(B) Treatment effect of audit study result") +
  theme(legend.position = "bottom")

## Figure 9, Panel B
plot_10 <- bind_rows(
  donate_a %>% tidy(),
  donate_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection") %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection" ~ ""),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects(NoAxis = TRUE, fxscale = FALSE) +
  labs(y = "", x = "", title = "(B) Treatment effect of audit study result") +
  theme(legend.position = c(0.85, 0.2))

## Putting them all together
patch_1 <- (plot_1 | plot_2) 
ggsave(patch_1, file = "Fig5_Correction.png", 
       width = 12, height = 5, dpi = "retina")

patch_2 <- (plot_3 | plot_4) 
ggsave(patch_2, file = "Fig6_Correction.png", 
       width = 12, height = 3.5, dpi = "retina")

patch_3 <- (plot_5 | plot_6) 
ggsave(patch_3, file = "Fig7_Correction.png", 
       width = 12, height = 2.5, dpi = "retina")

patch_4 <- (plot_7 | plot_8) 
ggsave(patch_4, file = "Fig8_Correction.png", 
       width = 12, height = 4.25, dpi = "retina")

patch_5 <- (plot_9 | plot_10)
ggsave(patch_5, file = "Fig9_Correction.png", 
       width = 12, height = 3.5, dpi = "retina")

# Figure 10: Framing effect
###########################
## Panel A
plot_1 <- bind_rows(
  recogni_a %>% tidy(),
  recogni_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection + Researcher" ~ "Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Audit result\n+ Potentially\naffected",
                  "Audit result\n+ Lawyer",
                  "Audit result\n+ Researcher"),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects() +
  labs(y = "", x = "", 
       title = "(A) Recognition of discrimination") +
  theme(legend.position = "none")

## Panel B
plot_2 <- bind_rows(
  reason_a %>% tidy(),
  reason_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection + Researcher" ~ "Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Audit result\n+ Potentially\naffected",
                  "Audit result\n+ Lawyer",
                  "Audit result\n+ Researcher"),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects() +
  labs(x = "", y = "", 
       title = "(B) Reasons for discrimination") +
  theme(legend.position = "none")

## Panel C
plot_3 <- bind_rows(
  blind_appl_a %>% tidy(),
  blind_appl_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection + Researcher" ~ "Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Audit result\n+ Potentially\naffected",
                  "Audit result\n+ Lawyer",
                  "Audit result\n+ Researcher"),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects() +
  labs(x = "", y = "", 
       title = "(C) Support for name-blind applications") +
  theme(legend.position = "none")

## Panel D
plot_4 <- bind_rows(
  eqltrt_a %>% tidy(),
  eqltrt_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection + Researcher" ~ "Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Audit result\n+ Potentially\naffected",
                  "Audit result\n+ Lawyer",
                  "Audit result\n+ Researcher"),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects() +
  labs(x = "", y = "", 
       title = "(D) Support for equal-treatment policies") +
  theme(legend.position = "none")

## Panel E
plot_5 <- bind_rows(
  donate_a %>% tidy(),
  donate_b %>% tidy(),
  .id = "Sample") %>%
  filter(term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionCorrection + Researcher" ~ "Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Audit result\n+ Potentially\naffected",
                  "Audit result\n+ Lawyer",
                  "Audit result\n+ Researcher"),
    Who = case_when(
      Sample < 2 ~ "Whole sample",
      TRUE ~ "Under-perceivers")) %>%
  mrln_effects() +
  labs(x = "", y = "", 
       title = "(E) Donation to Immigrant Women's Center") +
  theme(legend.position = c(0.88, 0.12))

## Putting them all together=
plot <- (plot_1 + plot_2 + plot_3 + plot_4 + plot_5) +
  plot_layout(ncol = 3)

## Save for the article
ggsave(plot, file = "Fig10_Framing.png", width = 13, height = 9, dpi = "retina")

# Figure 11: Priming effect
###########################
## Panel A
plot_1 <- recogni_c %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection" | 
           term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection" ~ "Elicitation\n+ Audit result",
      term == "tr_crctionCorrection + Researcher" ~ "Elicitation\n+ Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation\n+ Audit result\n+ Researcher",
                  "Elicitation\n+ Audit result",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", 
       title = "(A) Recognition of discrimination")

## Panel B
plot_2 <- reason_c %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection" | 
           term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection" ~ "Elicitation\n+ Audit result",
      term == "tr_crctionCorrection + Researcher" ~ "Elicitation\n+ Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation\n+ Audit result\n+ Researcher",
                  "Elicitation\n+ Audit result",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", title = "(B) Reasons for discrimination")

## Panel C
plot_3 <- blind_appl_c %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection" | 
           term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection" ~ "Elicitation\n+ Audit result",
      term == "tr_crctionCorrection + Researcher" ~ "Elicitation\n+ Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation\n+ Audit result\n+ Researcher",
                  "Elicitation\n+ Audit result",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", 
       title = "(C) Support for name-blind applications")

## Panel D
plot_4 <- eqltrt_c %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection" | 
           term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection" ~ "Elicitation\n+ Audit result",
      term == "tr_crctionCorrection + Researcher" ~ "Elicitation\n+ Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation\n+ Audit result\n+ Researcher",
                  "Elicitation\n+ Audit result",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "",
       title = "(D) Support for equal-treatment policies")

## Panel E
plot_5 <- donate_c %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection" | 
           term == "tr_crctionCorrection + Researcher" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection" ~ "Elicitation\n+ Audit result",
      term == "tr_crctionCorrection + Researcher" ~ "Elicitation\n+ Audit result\n+ Researcher",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation\n+ Audit result\n+ Researcher",
                  "Elicitation\n+ Audit result",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", 
       title = "(E) Donation to Immigrant Women's Center")

## Putting them all together=
plot <- (plot_1 + plot_2 + plot_3 + plot_4 + plot_5) +
  plot_layout(ncol = 3)

## Save for the article
ggsave(plot, file = "Fig11_Priming.png", width = 13, height = 9, dpi = "retina")

# Figure 12: Lasting effects
############################
## Panel A
plot_1 <- recogni_d %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", 
       title = "(A) Recognition of discrimination")

## Panel B
plot_2 <- blind_appl_d %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", title = "(B) Support for name-blind applications") +
  theme(legend.position = "none")

## Panel C
plot_3 <- eqltrt_d %>% tidy() %>%
  filter(term == "tr_crctionNo correction" | 
           term == "tr_crctionCorrection + Lawyer" |
           term == "tr_crctionCorrection + Affected" ) %>%
  mutate(
    term = case_when(
      term == "tr_crctionNo correction" ~ "Elicitation",
      term == "tr_crctionCorrection + Lawyer" ~ "Elicitation\n+ Audit result\n+ Lawyer",
      term == "tr_crctionCorrection + Affected" ~ "Elicitation\n+ Audit result\n+ Potentially\naffected") %>%
      fct_relevel("Elicitation\n+ Audit result\n+ Potentially\naffected",
                  "Elicitation\n+ Audit result\n+ Lawyer",
                  "Elicitation")) %>%
  mrln_effects(wer = FALSE) +
  labs(y = "", x = "", title = "(C) Support for equal-treatment policies") +
  theme(legend.position = "none")

## Putting them all together
patch <- (plot_1 | plot_2 | plot_3)

## Save for the article
ggsave(patch, file = "Fig12_Lasting.png", width = 13, height = 4, dpi = "retina")