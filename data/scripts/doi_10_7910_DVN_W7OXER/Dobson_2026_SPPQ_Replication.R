###############################################################################
#                                                                             #
#         SELECTIVE RECIPROCITY IN BIPARTISAN COLLABORATION:                  #
#         HOW MAJORITY SECURITY SHAPES LEGISLATIVE SUCCESS                    #
#                       Mackenzie R. Dobson                                   #                             
#                     University of Virginia                                  #
#                                                                             #
#      Replication Script for State Politics & Policy Quarterly               #
#                              2026                                           #
###############################################################################
# PREPARE R SESSION # 

# Load required packages after installing (if not already installed)


# Clear memory
rm(list=ls())

# Load packages 
library(tidyverse)
library(ggplot2)
library(foreign)
library(patchwork)
library(dplyr)
library(stringr)
library(haven)
library(purrr)
library(lfe)
library(stargazer)
library(lme4)
library(MASS)
library(broom)      
library(forcats)
library(grid)
library(ggtext)
library(scales)
library(ggrepel)
library(knitr)
library(kableExtra)

################################################################################
# Load data
################################################################################
# Set working directory (pathway to replication data)
setwd("")

# Load data 
df <- read.csv("Dobson_Bipartisanship_Scores_SPPQ_2026.csv")

################################################################################
# Prep data
################################################################################
# For robustness checks in Appendix: 

# Alternative specification of security: electoral duration and coalition size # 
df <- df %>%
  mutate(robust_secure_both = if_else(secure_num == 1 & consecutive_terms_in_control >= 2, 1, 0))

# Alternative specification of security: 60% of seats = secure 
df <- df %>%
  mutate(
    level_security_60 = case_when(
      partisan_seatshare_diff >= 0.2 ~ "Secure",
      partisan_seatshare_diff <  0.2 ~ "Insecure",
      TRUE ~ NA_character_
    ),
  )

# Filter the data to include only rows where in_majority == 1
majority_filtered_df <- subset(df, in_majority == 1)

# Filter the data to include only rows where in_majority == 0
minority_filtered_df <- subset(df, in_majority == 0)

# Create minority party incumbency advantage 
# Total count of minority party legislators 
minority_filtered_df <- minority_filtered_df %>%
  group_by(state, chamber, term) %>%
  mutate(total_minority_party_legislators = n_distinct(sles_id)) %>%
  ungroup()  # Ungroup after adding the variable

# Total count of minority party legislators who have a seniority >=2 
minority_filtered_df <- minority_filtered_df %>%
  group_by(state, chamber, term) %>%
  mutate(senior_minority_party_legislators = n_distinct(sles_id[seniority >= 2])) %>%
  ungroup()  # Ungroup after adding the variable

# Minority party incumbency rate 
minority_filtered_df <- minority_filtered_df %>%
  mutate(minority_party_incumbency_rate = senior_minority_party_legislators / total_minority_party_legislators) 

# Create subsets of the main data frame, disaggregated by party status and majority security 

minority_secure_chamber_df <- minority_filtered_df %>%
  filter(level_security == "secure")

majority_secure_chamber_df <- majority_filtered_df %>%
  filter(level_security == "secure")

minority_insecure_chamber_df <- minority_filtered_df %>%
  filter(level_security == "insecure")

majority_insecure_chamber_df <- majority_filtered_df %>%
  filter(level_security == "insecure")

# Create subsets of minority party legislators with "complete" bipartisan information,
# meaning legislators whose first term was after legiscan data became available 
# guards against learning effects that may be associated with seniority across all observations

freshman.legislators <- minority_filtered_df %>%
  filter(first_elected >= 2006 & seniority == 1) %>%
  filter(first_elected >= 2008 | state %in% c("ok", "pa"))

freshman.legislators <- freshman.legislators %>%
  dplyr::select(sles_id, state)

minority_complete_observations <- minority_filtered_df %>%
  semi_join(freshman.legislators, by = c("state", "sles_id"))

minority_complete_observations <- minority_complete_observations %>%
  filter(first_elected >= 2008 | state %in% c("ok", "pa"))

minority_complete_observations <- minority_complete_observations %>%
  filter(first_elected != 2004)

summary(minority_complete_observations$first_elected)

# Retain only unique observations for each combination of sles_id, state, chamber, term 
minority_complete_observations <- minority_complete_observations %>%
  distinct(sles_id, state, term, chamber, .keep_all = TRUE)

minority_secure_chamber_df_complete_obs <- minority_complete_observations %>%
  filter(level_security == "secure")

minority_insecure_chamber_df_complete_obs <- minority_complete_observations %>%
  filter(level_security == "insecure")

################################################################################
# Estimate in-text models
################################################################################

## Figure 1  ##

security_threshold <- 1/3  # gap = 0.333 -> majority share ≈ 66.7%

# Order states by chamber-averaged gap so the y-axis tells a story
state_order <- df %>%
  group_by(state) %>%
  summarize(avg_gap = mean(partisan_seatshare_diff, na.rm = TRUE), .groups = "drop") %>%
  arrange(avg_gap) %>% pull(state) %>% toupper()

df_state_avg <- df %>%
  group_by(state, chamber) %>%
  summarize(avg_seatshare_diff = mean(partisan_seatshare_diff, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(
    state  = factor(toupper(state), levels = state_order),
    chamber = factor(chamber, levels = c("house","senate")),
    level_security = ifelse(avg_seatshare_diff >= security_threshold, "Secure", "Insecure"),
    level_security = factor(level_security, levels = c("Insecure","Secure"))
  )

pd <- position_dodge(width = 0.35)   # Separate House/Senate on the y axis a bit

# Plot
ggplot() +
  # Shaded regions
  annotate("rect", xmin = 0, xmax = security_threshold, ymin = -Inf, ymax = Inf,
           fill = "#1b9e77", alpha = 0.06) +
  annotate("rect", xmin = security_threshold, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = "#377eb8", alpha = 0.06) +
  
  # Rug marks for distribution of all chambers
  geom_rug(
    data = df,
    aes(x = partisan_seatshare_diff),
    sides = "b", alpha = 0.4, color = "grey70", length = unit(0.03, "npc")
  ) +
  
  # Threshold + label
  geom_vline(xintercept = security_threshold, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  annotate("text", x = security_threshold, y = Inf, vjust = 1.4, hjust = -0.05,
           label = "≈ 2/3 majority", size = 3.2, color = "grey25", angle = 90) +
  
  # Chamber points
  geom_point(data = df_state_avg,
             aes(x = avg_seatshare_diff, y = state,
                 shape = chamber, color = level_security),
             size = 3.2, alpha = 0.95) +
  
  # Tags
  annotate("text", x = 0.17, y = -Inf, vjust = -60, hjust = 1,
           label = "Insecure Majority", size = 3.2, color = "#1b9e77",
           fontface = "bold") +
  annotate("text", x = 0.83, y = -Inf, vjust = -60, hjust = 3.15,
           label = "Secure Majority", size = 3.2, color = "#377eb8",
           fontface = "bold") +
  
  # x scales with secondary axis 
  scale_x_continuous(
    limits = c(0, 1),
    labels = scales::number_format(accuracy = 0.01),
    name   = "**Partisan Seat Share Difference**<br>|majority seats − minority seats|",
    sec.axis = sec_axis(
      ~ (.+1)/2,
      labels = scales::percent_format(accuracy = 1),
      name   = "**Implied Majority Party Seat Share**"
    )
  ) +
  scale_color_manual(values = c("Insecure" = "#1b9e77", "Secure" = "#377eb8"),
                     name = "Majority Security") +
  scale_shape_manual(values = c("house" = 16, "senate" = 17),
                     labels = c("House", "Senate"),
                     name = "Chamber") +
  labs(y = "**State**") +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8.5),
    legend.position = "bottom",
    legend.background = element_rect(color = "black", fill = "white"),
    legend.key.width = unit(0.7, "cm"),
    
    axis.title.x = element_markdown(),
    axis.title.x.top = element_markdown(),
    axis.title.y = element_markdown()
  )



## Table 2 ##
# DV: bipartisan cosponsorship offered
# IV: party seat share difference 

# Partisan seat share difference 
test1.minority <- felm(
  formula = proportion_direct_bipartisan_bills ~ partisan_seatshare_diff  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader  +
    power_comm + ideo_med_distance + party + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber | 0 | sles_id, data = minority_filtered_df
)

summary(test1.minority)


# Partisan seat share difference + conditioning on expected bipartisan cosponsorships offered
test1.minority.ev <- felm(
  formula = proportion_direct_bipartisan_bills ~ partisan_seatshare_diff + expected_bipartisan_cosponsorships_offered + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + party + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber | 0 | sles_id, data = minority_filtered_df
)

summary(test1.minority.ev)

# Partisan seat share difference 
test1.majority <- felm(
  formula = proportion_direct_bipartisan_bills ~ partisan_seatshare_diff + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_majleader  + Leader_speakerpres +
    power_comm + ideo_med_distance + party + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber | 0 | sles_id, data = majority_filtered_df
)

summary(test1.majority)


# Partisan seat share difference + conditioning on expected bipartisan cosponsorships offered 
test1.majority.ev <- felm(
  formula = proportion_direct_bipartisan_bills ~ partisan_seatshare_diff + expected_bipartisan_cosponsorships_offered + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_majleader + Leader_speakerpres +
    power_comm + ideo_med_distance + party + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber | 0 | sles_id, data = majority_filtered_df
)

summary(test1.majority.ev)


test1.table <- list(test1.minority, test1.minority.ev,
                    test1.majority, test1.majority.ev)

# This entire table is also reported in Table A.2 in the Supplementary Materials
stargazer(test1.table, type = "latex", digits = 3, out = "Figures and Tables/H1_test.tex")

# Note on aesthetics for Table 2/Table A.2: 

# For the in-text table, only primary independent variables are included (partisan_seatshare_diff and expected_bipartisan_cosponsorships_offered).
# The models are still estimated with full controls. This version of the table output can be found in Table A.2. In both the in-text and supplemental 
# appendix, I i) rename the model outputs from "(1) & (2) & (3) & (4)" to "Minority 2.1", "Minority 2.2", "Majority 2.3", and "Majority 2.4"; ii) add 
# check marks indicating fixed effect specification (e.g., chamber); iii) report only the following model diagnostics: Observations and Adjusted R-squared; and 
# iv) clean up variable names as follows: 
# partisan\_seatshare\_diff  --> Partisan Seat Share Difference 
# expected\_bipartisan\_cosponsorships\_offered --> Expected Bipartisan Cosponsorships Offered
# SM\_median\_diff --> Polarization 
# gov\_same\_party --> In Governor's Party
# seniority --> Seniority
# comm\_chair --> Committee Chair
# Leader\_minleader --> Minority Party Leadership
# Leader\_majleader --> Majority Party Leadership
# Leader\_speakerpres --> Speaker/President 
# power\_comm --> Power Committee
# ideo\_med\_distance --> Distance from Median
# party --> Republican
# female --> Female
# race\_asian --> Asian
# race\_black --> Black
# race\_hispanic --> Latino
# vote\_share --> Vote Share

## Table 3 ##
# DV: bipartisan cosponsorship offered
# IV: seniority 

# Complete observations sub-sample 

test2.secure.complete.obs <- felm(
  formula = proportion_direct_bipartisan_bills ~ seniority + SM_median_diff + gov_same_party +
    comm_chair + Leader_minleader + power_comm + ideo_med_distance +
    vote_share + minority_party_incumbency_rate| sles_id + chamber | 0 | sles_id, data = minority_secure_chamber_df_complete_obs
)

summary(test2.secure.complete.obs)

test2.insecure.complete.obs <- felm(
  formula = proportion_direct_bipartisan_bills ~ seniority  +SM_median_diff + gov_same_party +
    comm_chair + Leader_minleader + power_comm + ideo_med_distance +
    vote_share + minority_party_incumbency_rate| sles_id + chamber | 0 | sles_id, data = minority_insecure_chamber_df_complete_obs
)

summary(test2.insecure.complete.obs)

test2.table <- list(test2.insecure.complete.obs, test2.secure.complete.obs)

# This entire table is also reported in Table A.3 in the Supplementary Materials
stargazer(test2.table, type = "latex", digits = 3, out = "Figures and Tables/H2_test.tex")

# Note on aesthetics for Table 3/Table A.3: 

# For the in-text table, only the primary independent variable, seniority, is included.
# The models are still estimated with full controls. This version of the table output can be found in Table A.3. In both the in-text and supplemental 
# appendix, I i) rename the model outputs from "(1) & (2)" to "Insecure Majority 3.1" and "Secure Majority 3.2";  ii) add 
# check marks indicating fixed effect specification (e.g., legislator); iii) report only the following model diagnostics: Observations and Adjusted R-squared; and 
# iv) clean up variable names as follows: 
# seniority --> Seniority
# SM\_median\_diff --> Polarization 
# gov\_same\_party --> In Governor's Party
# comm\_chair --> Committee Chair
# Leader\_minleader --> Minority Party Leadership
# power\_comm --> Power Committee
# vote\_share --> Vote Share
# minority\_party\_incumbency\_rate --> Minority Party Incumbency Rate

## Table 4 ##
# DV: bipartisan cosponsorship attracted
# IV: bipartisan cosponsorship + lagged bipartisan cosponsorship offered

minority.secure.T <- felm(
  formula = average_prop_bill_bipartisan~ proportion_direct_bipartisan_bills + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(minority.secure.T)

minority.secure.T.minus.1 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  +SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(minority.secure.T.minus.1)

minority.secure.T.minus.2 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered_T2 + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(minority.secure.T.minus.2)

minority.insecure.T <- felm(
  formula = average_prop_bill_bipartisan~ proportion_direct_bipartisan_bills + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(minority.insecure.T)

minority.insecure.T.minus.1 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(minority.insecure.T.minus.1)

minority.insecure.T.minus.2 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered_T2 + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(minority.insecure.T.minus.2)


test3.table <- list(minority.insecure.T, minority.insecure.T.minus.1, minority.insecure.T.minus.2,
                    minority.secure.T, minority.secure.T.minus.1, minority.secure.T.minus.2)

# This entire table is also reported in Table A.4 in the Supplementary Materials
stargazer(test3.table, type = "latex", digits = 3, out = "Figures and Tables/H3_test.tex")

# Note on aesthetics for Table 4/Table A.4: 

# For the in-text table, only primary independent variables are included ( proportion\_direct\_bipartisan\_bills, lagged\_offered, lagged\_offered\_T2).
# The models are still estimated with full controls. This version of the table output can be found in Table A.4. In both the in-text and supplemental 
# appendix, I i) rename the model outputs from "(1) & (2) & (3) & (4) & (5) & (6)" to "Insecure Majority 4.1", "Insecure Majority 4.2", "Insecure Majority 4.3", 
# "Secure Majority 4.4", "Secure Majority 4.5", and "Secure Majority 4.6"; ii) add check marks indicating fixed effect specification (e.g., chamber); 
# iii) report only the following model diagnostics: Observations and Adjusted R-squared; and 
# iv) clean up variable names as follows: 

# proportion\_direct\_bipartisan\_bills --> Proportion Bipartisan Cosponsorships Offered 
# lagged\_offered --> Proportion Bipartisan Cosponsorships Offered t-1
# lagged\_offered\_T2 --> Proportion Bipartisan Cosponsorships Offered t-2
# lagged\_attracted --> Proportion Bipartisan Cosponsors Attracted t-1
# avg\_expected\_bipartisan\_cosponsors\_attracted --> Expected Bipartisan Cosponsors Attracted
# SM\_median\_diff --> Polarization 
# gov\_same\_party --> In Governor's Party
# seniority --> Seniority
# comm\_chair --> Committee Chair
# Leader\_minleader --> Minority Party Leadership
# power\_comm --> Power Committee
# ideo\_med\_distance --> Distance from Median
# female --> Female
# race\_asian --> Asian
# race\_black --> Black
# race\_hispanic --> Latino
# vote\_share --> Vote Share

## Figure 2 ##
# Do minority party legislators "get" more for their reciprocal capital on bills when
# majorities are less secure? 


# Substantive (insecure + minority)

s.bills.insecure.fig2 <- felm(
  formula = s_bills  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.bills.insecure.fig2)

s.aic.insecure.fig2 <- felm(
  formula = s_aic  ~ lagged_offered  + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share|state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.aic.insecure.fig2)

s.abc.insecure.fig2 <- felm(
  formula = s_abc  ~lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.abc.insecure.fig2)

s.pass.insecure.fig2 <- felm(
  formula = s_pass  ~ lagged_offered +expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.pass.insecure.fig2)


substantive.insecure.minority <- list(s.bills.insecure.fig2, s.aic.insecure.fig2, s.abc.insecure.fig2, s.pass.insecure.fig2)


# This entire table isreported in Table A.5 in the Supplementary Materials
stargazer(substantive.insecure.minority, type = "latex", digits = 3, out = "Figures and Tables/H4_test_insecure.tex")


# Substantive (secure + minority)

s.bills.secure.fig2 <- felm(
  formula = s_bills ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share| state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.bills.secure.fig2)

s.aic.secure.fig2 <- felm(
  formula = s_aic  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.aic.secure.fig2)

s.abc.secure.fig2 <- felm(
  formula = s_abc  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.abc.secure.fig2)

s.pass.secure.fig2 <- felm(
  formula = s_pass ~ lagged_offered  + expected_bipartisan_cosponsorships_offered   + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.pass.secure.fig2)


substantive.secure.minority <- list(s.bills.secure.fig2, s.aic.secure.fig2, s.abc.secure.fig2, s.pass.secure.fig2)

# This entire table is reported in Table A.6 in the Supplementary Materials
stargazer(substantive.secure.minority, type = "latex", digits = 3, out = "Figures and Tables/H4_test_secure.tex")


## Figure 2 ## 

# Name and store models
mods_insecure <- list(
  BILLS = s.bills.insecure.fig2,
  AIC   = s.aic.insecure.fig2,
  ABC   = s.abc.insecure.fig2,
  PASS  = s.pass.insecure.fig2
)
mods_secure <- list(
  BILLS = s.bills.secure.fig2,
  AIC   = s.aic.secure.fig2,
  ABC   = s.abc.secure.fig2,
  PASS  = s.pass.secure.fig2
)

tidy_list <- function(mod_list, sec_lab){
  map2_dfr(mod_list, names(mod_list), function(m, dvname){
    tidy(m, conf.int = TRUE) %>% mutate(DV = dvname, Security = sec_lab)
  })
}

coefs <- bind_rows(
  tidy_list(mods_insecure, "Insecure Majority Chamber"),
  tidy_list(mods_secure,  "Secure Majority Chamber")
)

# Keep only lagged_offered and set factor orders
lag_only <- coefs %>%
  filter(term == "lagged_offered") %>%
  mutate(
    DV = factor(DV, levels = c("BILLS","AIC","ABC","PASS")),
    Security = factor(Security, levels = c("Insecure Majority Chamber","Secure Majority Chamber"))
  )

# Faceted coefficient plot
p <- ggplot(lag_only,
            aes(x = DV, y = estimate,
                ymin = conf.low, ymax = conf.high)) +
  # red 0-line
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.8, color = "firebrick") +
  
  # Insecure 
  geom_pointrange(
    aes(alpha = (p.value < 0.05), shape = (p.value < 0.05)),
    data = subset(lag_only, Security == "Insecure Majority Chamber"),
    color = "#1b9e77",
    linewidth = 1.15,    
    size = 1
  ) +
  
  # Secure 
  geom_pointrange(
    aes(alpha = (p.value < 0.05), shape = (p.value < 0.05)),
    data = subset(lag_only, Security == "Secure Majority Chamber"),
    color = "#377eb8",
    linewidth = 1.15,
    size = 1
  ) +
  
  # fade/shape map: TRUE = significant; FALSE = not significant
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  
  # Add coefficient labels for estimates
  geom_text(
    data = subset(lag_only),   
    aes(label = ifelse(round(estimate, 0) == 1 & estimate < 1,
                       "< 1 bill",
                       ifelse(round(estimate, 0) == 1,
                              paste0(round(estimate, 0), " bill"),
                              paste0(round(estimate, 0), " bills")))),
    color = "black",
    hjust = -0.3,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  
  facet_wrap(~ Security, ncol = 2) +
  labs(
    x = NULL,
    y = "Count of Minority Party Sponsored Bills"
  ) +  
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),  # bold x tick labels
    axis.text.y = element_text(size = 11, face = "bold"),  # bold y tick labels
    axis.title.x = element_text(face = "bold"),            # bold x title
    axis.title.y = element_text(face = "bold"),            # bold y title
    legend.position = "none",
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold")
  )

print(p)


################################################################################
# Supplementary Materials
################################################################################

## Figure A.1 ## 
# Misclassifying Majority Security: Duration vs. Coalition Size 

# Summarize chamber-level data
chamber_misclass <- df %>%
  group_by(state, chamber) %>%
  summarize(
    avg_seatshare = mean(partisan_seatshare_diff, na.rm = TRUE),
    max_consec = max(consecutive_terms_in_control, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    label = toupper(state),
    chamber_shape = if_else(chamber %in% c("house"), "House", "Senate"),
    potential_misclass = avg_seatshare < 0.33 & max_consec >= 2
  )

# Plot
ggplot(chamber_misclass, aes(x = max_consec, y = avg_seatshare)) +
  geom_point(aes(color = potential_misclass, shape = chamber_shape), size = 3) +
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "gray50") +
  geom_text_repel(
    data = chamber_misclass %>% filter(potential_misclass == TRUE),
    aes(label = label),
    size = 3,
    max.overlaps = Inf,
    box.padding = 0.3,
    point.padding = 0.2,
    force = 1.5,
    segment.color = "gray60",
    segment.size = 0.3
  ) +
  scale_color_manual(
    values = c("TRUE" = "firebrick", "FALSE" = "gray70"),
    labels = c("FALSE" = "False", "TRUE" = "True")
  ) +
  scale_shape_manual(values = c("House" = 16, "Senate" = 17)) +
  labs(
    x = "Consecutive Terms in Power",
    y = "Partisan Seat Share Difference",
    color = "Misclassified?",
    shape = "Chamber"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),  # bold x tick labels
    axis.text.y = element_text(size = 11, face = "bold"),  # bold y tick labels
    axis.title.x = element_text(face = "bold"),            # bold x title
    axis.title.y = element_text(face = "bold"),            # bold y title
    legend.position = "bottom",
    legend.background = element_rect(color = "black", fill = "white"), # box
    legend.title = element_text(face = "bold"))                     


# Create the summary data frame
seat_var <- df %>%
  group_by(state, chamber) %>%
  summarize(
    mean_share = mean(partisan_seatshare_diff, na.rm = TRUE),
    sd_share = sd(partisan_seatshare_diff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(seat_var, n = Inf)

# Create bins based on corrected threshold for secure majorities
seat_var_binned <- seat_var %>%
  mutate(
    bin = case_when(
      mean_share >= 0.60 ~ "> 0.60",
      mean_share >= 0.33 ~ "0.33–0.60",
      TRUE ~ "< 0.33"
    ),
    low_variance = sd_share < 0.05,
    mid_variance = sd_share < 0.10
  ) %>%
  group_by(bin) %>%
  summarize(
    chambers = n(),
    pct_low_var = mean(low_variance) * 100,
    pct_mid_var = mean(mid_variance) * 100,
    .groups = "drop"
  )

print(seat_var_binned, n=Inf)

## Table A.1 ##

# Create the summary data frame
seat_var <- df %>%
  group_by(state, chamber) %>%
  summarize(
    mean_share = mean(partisan_seatshare_diff, na.rm = TRUE),
    sd_share = sd(partisan_seatshare_diff, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(seat_var, n = Inf)

# Create bins based on three-level thresholds for secure majorities
seat_var_binned <- seat_var %>%
  mutate(
    bin = case_when(
      mean_share >= 0.60 ~ "> 0.60",
      mean_share >= 0.33 ~ "0.33–0.60",
      TRUE ~ "< 0.33"
    ),
    low_variance = sd_share < 0.05,
    mid_variance = sd_share < 0.10
  ) %>%
  group_by(bin) %>%
  summarize(
    chambers = n(),
    pct_low_var = mean(low_variance) * 100,
    pct_mid_var = mean(mid_variance) * 100,
    .groups = "drop"
  )

print(seat_var_binned, n=Inf)

## Figure A.2 ##

obs_vs_exp_panel <- function(
    sims,
    observed,
    expected = NULL,
    title_main = NULL,
    subtitle_top = NULL,
    subtitle_bottom = NULL,
    x_lab = "Proportion Bipartisan Cosponsorships Offered",
    bins = NULL,
    obs_x = NULL,        # x in data units
    obs_y_frac = 0.90,   # y as fraction of max bar height
    exp_x = NULL,
    exp_y_frac = 0.80,
    obs_hjust = 0,       # 0 = left-justified, 1 = right-justified
    exp_hjust = 0
) {
  if (is.null(expected)) expected <- mean(sims, na.rm = TRUE)
  
  if (is.null(bins)) {
    bw <- 2 * IQR(sims, na.rm = TRUE) / (length(na.omit(sims))^(1/3))
    if (!is.finite(bw) || bw <= 0) bw <- 0.01
    bins <- max(20, min(60, ceiling((max(sims, na.rm = TRUE) - min(sims, na.rm = TRUE)) / bw)))
  }
  
  # Precompute histogram height to convert y_frac -> counts
  h <- hist(sims, breaks = bins, plot = FALSE)
  y_max <- max(h$counts)
  
  # Defaults for label x-positions if not provided
  if (is.null(obs_x)) obs_x <- min(sims, na.rm = TRUE) + 0.02
  if (is.null(exp_x)) exp_x <- min(sims, na.rm = TRUE) + 0.02
  
  # Convert y fractions to data units
  obs_y <- y_max * obs_y_frac
  exp_y <- y_max * exp_y_frac
  
  df <- tibble(sim = sims)
  obs_lab <- paste0("Observed = ", sprintf("%.2f", observed))
  exp_lab <- paste0("Expected = ", sprintf("%.2f", expected))
  
  ggplot(df, aes(x = sim)) +
    geom_histogram(bins = bins, fill = "#377eb8", color = "black") +
    geom_vline(xintercept = expected, color = "black", linewidth = 0.9) +
    geom_vline(xintercept = observed, color = "firebrick", linewidth = 0.9, linetype = "dashed") +
    # label positions are fully configurable now
    annotate("text", x = obs_x, y = obs_y, label = obs_lab,
             color = "firebrick", size = 4, hjust = obs_hjust) +
    annotate("text", x = exp_x, y = exp_y, label = exp_lab,
             color = "black", size = 4, hjust = exp_hjust) +
    labs(
      title = title_main,
      subtitle = paste0(
        if (!is.null(subtitle_top)) subtitle_top else "",
        if (!is.null(subtitle_bottom)) paste0("\n", subtitle_bottom) else ""
      ),
      x = x_lab,
      y = "Frequency Across 1,000 Simulations"
    ) +
    theme_bw(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    ) +
    coord_cartesian(xlim = range(pretty(range(c(sims, observed, expected)))))
}


# Examples created to visualize simulation exercise 
# The values of observed and simulated bipartisanship are found in the data frame (observed_bipartisan_cosponsorships_offered, expected_bipartisan_cosponsorships_offered)

# Thielen:
thielen_vals <- df %>%
  filter(standardized_name == "cynthia thielen", term == "2017_2018") %>%
  dplyr::select(proportion_direct_bipartisan_bills, expected_bipartisan_cosponsorships_offered)


p_obs  <- thielen_vals$proportion_direct_bipartisan_bills
p_exp  <- thielen_vals$expected_bipartisan_cosponsorships_offered

set.seed(123)
sims_thielen <- rbeta(1000, shape1 = 80, shape2 = 8)

h <- hist(sims_thielen, breaks = seq(0, 1, by = 0.01), plot = FALSE)
y_text <- max(h$counts) * 0.9

ggplot(data.frame(sims = sims_thielen), aes(x = sims)) +
  geom_histogram(binwidth = 0.01, color = "grey20", fill = "#377eb8") +
  geom_vline(xintercept = p_exp, color = "black", linewidth = 1) +
  geom_vline(xintercept = p_obs, color = "firebrick", linetype = "dashed", linewidth = 1) +
  annotate("text", x = p_exp, y = y_text, label = sprintf("Expected = %.2f", p_exp),
           color = "black", fontface = "bold", hjust = -0.5) +
  annotate("text", x = p_obs, y = y_text, label = sprintf("Observed = %.2f", p_obs),
           color = "firebrick", fontface = "bold", hjust = 1.4) +
  scale_x_continuous(breaks = seq(0.75, 1.00, 0.05)) +
  coord_cartesian(xlim = c(0.75, 1.00), expand = TRUE) +  
  labs(
    x = "Proportion Bipartisan Cosponsorships Offered",
    y = "Frequency Across 1,000 Simulations"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13),
    axis.text    = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )


# Rothfuss
rothfuss_vals <- df %>%
  filter(standardized_name == "chris rothfuss", term == "2013_2014") %>%
  dplyr::select(proportion_direct_bipartisan_bills,
                expected_bipartisan_cosponsorships_offered)

p_obs <- as.numeric(rothfuss_vals$proportion_direct_bipartisan_bills)
p_exp <- as.numeric(rothfuss_vals$expected_bipartisan_cosponsorships_offered)

set.seed(123)
sims_rothfuss <- rbeta(1000, shape1 = 40, shape2 = 15)  

h <- hist(sims_rothfuss, breaks = seq(0, 1, by = 0.01), plot = FALSE)
y_text <- max(h$counts) * 0.9

ggplot(data.frame(sims = sims_rothfuss), aes(x = sims)) +
  geom_histogram(binwidth = 0.01, color = "grey20", fill = "#377eb8") +
  geom_vline(xintercept = p_exp, color = "black", linewidth = 1) +
  geom_vline(xintercept = p_obs, color = "firebrick", linetype = "dashed", linewidth = 1) +
  annotate("text", x = p_exp, y = y_text,
           label = sprintf("Expected = %.2f", p_exp),
           color = "black", fontface = "bold", hjust = -0.2, size = 4) +
  annotate("text", x = p_obs, y = y_text,
           label = sprintf("Observed = %.2f", p_obs),
           color = "firebrick", fontface = "bold", hjust = -0.1, size = 4) +
  scale_x_continuous(breaks = seq(0.4, 1, 0.1)) +
  coord_cartesian(xlim = c(0.4, 1), expand = TRUE) +
  labs(
    x = "Proportion Bipartisan Cosponsorships Offered",
    y = "Frequency Across 1,000 Simulations"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13),
    axis.text    = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )



# Hurt 
hurt_vals <- df %>%
  filter(standardized_name == "robert hurt", term == "2010_2011") %>%
  dplyr::select(proportion_direct_bipartisan_bills, expected_bipartisan_cosponsorships_offered)

p_obs  <- hurt_vals$proportion_direct_bipartisan_bills
p_exp  <- hurt_vals$expected_bipartisan_cosponsorships_offered

set.seed(123)
sims_hurt <- rbeta(1000, shape1 = 25, shape2 = 25) 

h <- hist(sims_hurt, breaks = seq(0, 1, by = 0.01), plot = FALSE)
y_text <- max(h$counts) * 0.9

ggplot(data.frame(sims = sims_hurt), aes(x = sims)) +
  geom_histogram(binwidth = 0.01, color = "grey20", fill = "#377eb8") +
  geom_vline(xintercept = p_exp, color = "black", linewidth = 1) +
  geom_vline(xintercept = p_obs, color = "firebrick", linetype = "dashed", linewidth = 1) +
  annotate("text", x = p_exp, y = y_text,
           label = sprintf("Expected = %.2f", p_exp),
           color = "black", fontface = "bold", hjust = -0.3, size = 4) +
  annotate("text", x = p_obs, y = y_text,
           label = sprintf("Observed = %.2f", p_obs),
           color = "firebrick", fontface = "bold", hjust = -0.1, size = 4) +
  scale_x_continuous(breaks = seq(0.2, 0.7, 0.1)) +
  coord_cartesian(xlim = c(0.2, 0.7), expand = TRUE) +
  labs(
    x = "Proportion Bipartisan Cosponsorships Offered",
    y = "Frequency Across 1,000 Simulations"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold", size = 13),
    axis.title.y = element_text(face = "bold", size = 13),
    axis.text    = element_text(size = 11, color = "black"),
    panel.grid.minor = element_blank()
  )


## Figure A.3 ## 

# Define thresholds
security_threshold <- 0.333
duration_threshold <- 2

# Create chamber-level averages and robust security classification
df_state_avg_robust <- df %>%
  group_by(state, chamber) %>%
  summarize(
    avg_seatshare_diff = mean(partisan_seatshare_diff, na.rm = TRUE),
    avg_duration = mean(consecutive_terms_in_control, na.rm = TRUE),
    robust_security = ifelse(
      avg_seatshare_diff >= security_threshold & avg_duration >= duration_threshold,
      "Secure", "Insecure"
    ),
    .groups = 'drop'
  ) %>%
  mutate(
    state = toupper(state),
    robust_security = factor(robust_security, levels = c("Insecure", "Secure"))
  )

# Plot with point size mapped to average duration
ggplot() +
  annotate("rect", xmin = 0, xmax = security_threshold, ymin = -Inf, ymax = Inf, 
           fill = "#1b9e77", alpha = 0.07) +
  annotate("rect", xmin = security_threshold, xmax = 1, ymin = -Inf, ymax = Inf, 
           fill = "#377eb8", alpha = 0.07) +
  # Rug marks for distribution of all chambers
  geom_rug(
    data = df,
    aes(x = partisan_seatshare_diff),
    sides = "b", alpha = 0.4, color = "grey70", length = unit(0.03, "npc")
  ) +
  geom_vline(xintercept = security_threshold, linetype = "dashed", color = "gray30") +
  geom_point(
    data = df_state_avg_robust,
    aes(x = avg_seatshare_diff,
        y = fct_reorder(state, avg_seatshare_diff),
        shape = chamber,
        color = robust_security,
        size = avg_duration),
    alpha = 0.9
  ) +
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0.01)) +
  scale_color_manual(
    name = "Majority Security:",
    values = c("Insecure" = "#1b9e77", "Secure" = "#377eb8")
  ) +
  scale_shape_manual(
    name = "Chamber:",
    values = c("house" = 16, "senate" = 17),
    labels = c("House", "Senate")
  ) +
  scale_size_continuous(
    name = "Avg. Consecutive Terms",
    range = c(1, 4)
  ) +
  labs(
    x = "Partisan Seat Share Difference",
    y = "State",
    shape = "Chamber",
    color = "Robust Security"
  ) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),            # bold x title
    axis.title.y = element_text(face = "bold"),            # bold y title
    legend.position = "bottom",
    legend.box = "vertical",
    legend.background = element_rect(color = "black", fill = "white"), # box
    legend.title = element_text(face = "bold"))   


# Figure A.4 # 
# Why not veto override thresholds? 

veto.points <- read.csv("Birkhead_Harden_Windett_Vetoes.csv")


# Count legislatures by threshold
counts <- veto.points %>%
  mutate(threshold = as.numeric(filibuster_gop_gov)) %>%
  count(threshold, name = "n") %>%
  mutate(
    label = ifelse(threshold == 0.50, "50% +1 seat",
                   percent(threshold, accuracy = 1)),
    label = factor(label, levels = c("50% +1 seat", "57%", "60%", "67%", "69%"))
  ) %>%
  arrange(label)

# Plot
bar_color <- "#377eb8"
ymax <- max(counts$n)

ggplot(counts, aes(x = label, y = n)) +
  geom_col(fill = bar_color, width = 0.85) +
  geom_text(aes(label = n), vjust = -0.35, size = 5, fontface = "bold") +
  labs(
    x = "Threshold",
    y = "Count of State Legislatures"
  ) +
  scale_y_continuous(limits = c(0, ymax * 1.12), breaks = pretty(c(0, ymax))) +
  theme_bw(base_size = 12) +
  theme(
    plot.title   = element_text(face = "bold", size = 18, hjust = 0),
    plot.subtitle= element_text(size = 11, hjust = 0, margin = margin(b = 8)),
    axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 6)),
    axis.title.y = element_text(face = "bold", size = 13, margin = margin(r = 6)),
    axis.text.x  = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank()
  )

## Tables A.7 - A.10: Robustness Check of In-Text Models ##
# These robustness checks impose stricter criteria for chambers to be labeled as "secure."
# Chambers must satisfy 2 conditions: i) consecutive majority electoral control of 2+ terms and ii) control 2/3 or more of the seats 

# Create subsets of the main data frame, disaggregated by party status and robust security 
minority_robust_secure_df <- minority_filtered_df %>%
  filter(robust_secure_both == 1)

minority_robust_insecure_df <- minority_filtered_df %>%
  filter(robust_secure_both == 0)

majority_robust_secure_df <- majority_filtered_df %>%
  filter(robust_secure_both == 1)

majority_robust_insecure_df <- majority_filtered_df %>%
  filter(robust_secure_both == 0)

minority_robust_secure_df_complete_obs <- minority_complete_observations %>%
  filter(robust_secure_both == 1)

minority_robust_insecure_df_complete_obs <- minority_complete_observations %>%
  filter(robust_secure_both == 0)


# Table A.7 
# Within legislator test (H2) # 

# DV: bipartisan cosponsorship offered
# IV: seniority 

# Complete observations sub-sample 

test2.secure.complete.obs.robust <- felm(
  formula = proportion_direct_bipartisan_bills ~ seniority + SM_median_diff + gov_same_party +
    comm_chair + Leader_minleader + power_comm + ideo_med_distance +
    vote_share + minority_party_incumbency_rate| sles_id + chamber | 0 | sles_id, data = minority_robust_secure_df_complete_obs
)

summary(test2.secure.complete.obs.robust)

test2.insecure.complete.obs.robust <- felm(
  formula = proportion_direct_bipartisan_bills ~ seniority + SM_median_diff + gov_same_party +
    comm_chair + Leader_minleader + power_comm + ideo_med_distance +
    vote_share + minority_party_incumbency_rate| sles_id + chamber | 0 | sles_id, data = minority_robust_insecure_df_complete_obs
)

summary(test2.insecure.complete.obs.robust)

test2.robust.table <- list(test2.insecure.complete.obs.robust, test2.secure.complete.obs.robust)

# This entire table is reported in Table A.7 in the Supplementary Materials
stargazer(test2.robust.table, type = "latex", digits = 3, out = "Figures and Tables/H2_test_robustness.tex")

# Table A.8
# Reciprocity test (H3) # 

# DV: bipartisan cosponsorship attracted
# IV: bipartisan cosponsorship + lagged bipartisan cosponsorship offered

minority.secure.T.robust <- felm(
  formula = average_prop_bill_bipartisan~ proportion_direct_bipartisan_bills + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_secure_df
)

summary(minority.secure.T.robust)

minority.secure.T.minus.1.robust <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_secure_df
)

summary(minority.secure.T.minus.1.robust)

minority.secure.T.minus.2.robust <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered_T2 + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_secure_df
)

summary(minority.secure.T.minus.2.robust)


minority.insecure.T.robust <- felm(
  formula = average_prop_bill_bipartisan~ proportion_direct_bipartisan_bills + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_insecure_df
)

summary(minority.insecure.T.robust)

minority.insecure.T.minus.1.robust <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_insecure_df
)

summary(minority.insecure.T.minus.1.robust)

minority.insecure.T.minus.2.robust <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered_T2 + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_insecure_df
)

summary(minority.insecure.T.minus.2.robust)

test3.robust.table <- list(minority.insecure.T.robust, minority.insecure.T.minus.1.robust,
                           minority.insecure.T.minus.2.robust, minority.secure.T.robust,
                           minority.secure.T.minus.1.robust, minority.secure.T.minus.2.robust)

# This entire table is reported in Table A.8 in the Supplementary Materials
stargazer(test3.robust.table, type = "latex", digits = 3, out = "Figures and Tables/H3_test_robustness.tex")



# Table A.9 
# Policy returns on cosponsorship offered efforts (H4) 

# DV: stage of lawmaking process 
# IV: proportion bipartisan cosponsorships offered t-1

# Substantive (insecure + minority)

s.bills.insecure.robust <- felm(
  formula = s_bills  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_robust_insecure_df
)

summary(s.bills.insecure.robust)

s.aic.insecure.robust <- felm(
  formula = s_aic  ~ lagged_offered  + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share|state + term + chamber  | 0 | sles_id, data = minority_robust_insecure_df
)

summary(s.aic.insecure.robust)

s.abc.insecure.robust <- felm(
  formula = s_abc  ~lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_insecure_df
)

summary(s.abc.insecure.robust)

s.pass.insecure.robust <- felm(
  formula = s_pass  ~ lagged_offered +expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_insecure_df
)

summary(s.pass.insecure.robust)


test4.insecure.robust.table <- list(s.bills.insecure.robust, s.aic.insecure.robust,
                                    s.abc.insecure.robust, s.pass.insecure.robust)

# This entire table is reported in Table A.9 in the Supplementary Materials
stargazer(test4.insecure.robust.table, type = "latex", digits = 3, out = "Figures and Tables/H4_test_insecure_robustness.tex")


# Table A.10

# Substantive (secure + minority)

s.bills.secure.robust <- felm(
  formula = s_bills ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share| state + term + chamber  | 0 | sles_id, data = minority_robust_secure_df
)

summary(s.bills.secure.robust)

s.aic.secure.robust <- felm(
  formula = s_aic  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_robust_secure_df
)

summary(s.aic.secure.robust)

s.abc.secure.robust <- felm(
  formula = s_abc  ~ lagged_offered + expected_bipartisan_cosponsorships_offered + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_robust_secure_df
)

summary(s.abc.secure.robust)

s.pass.secure.robust <- felm(
  formula = s_pass ~ lagged_offered  + expected_bipartisan_cosponsorships_offered   + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_robust_secure_df
)

summary(s.pass.secure.robust)

test4.secure.robust.table <- list(s.bills.secure.robust, s.aic.secure.robust,
                                  s.abc.secure.robust, s.pass.secure.robust)

# This entire table is reported in Table A.10 in the Supplementary Materials
stargazer(test4.secure.robust.table, type = "latex", digits = 3, out = "Figures and Tables/H4_test_secure_robustness.tex")


##################################################################
## Tables A.11 - A.14 : Robustness Check of In-Text Models ##
# These robustness checks impose an alternative criterion for chambers to be labeled as "secure."
# Majority parties must control 60% (rather than 67%) of seats in a chamber to be labeled as "secure." 

# Table A.11

# Set security cutpoint to 60% of seats 
df <- df %>%
  mutate(
    level_security_60 = case_when(
      partisan_seatshare_diff >= 0.2 ~ "secure",
      partisan_seatshare_diff <  0.2 ~ "insecure",
      TRUE ~ NA_character_
    ),
  )

df <- df %>%
  mutate(secure_num_60 = if_else(level_security_60 == "secure", 1, 0))

# Filter the data to include only rows where in_majority == 1
majority_filtered_df <- subset(df, in_majority == 1)

# Filter the data to include only rows where in_majority == 0
minority_filtered_df <- subset(df, in_majority == 0)

# Create minority party incumbency advantage 

# Total count of minority party legislators 
minority_filtered_df <- minority_filtered_df %>%
  group_by(state, chamber, term) %>%
  mutate(total_minority_party_legislators = n_distinct(sles_id)) %>%
  ungroup()  # Ungroup after adding the variable

# Total count of minority party legislators who have a seniority >=2 
minority_filtered_df <- minority_filtered_df %>%
  group_by(state, chamber, term) %>%
  mutate(senior_minority_party_legislators = n_distinct(sles_id[seniority >= 2])) %>%
  ungroup()  # Ungroup after adding the variable

# Minority party incumbency rate 
minority_filtered_df <- minority_filtered_df %>%
  mutate(minority_party_incumbency_rate = senior_minority_party_legislators / total_minority_party_legislators) 



# Create subsets of the main data frame, disaggregated by party status and majority security 

minority_secure_chamber_df <- minority_filtered_df %>%
  filter(level_security_60 == "secure")

majority_secure_chamber_df <- majority_filtered_df %>%
  filter(level_security_60 == "secure")

minority_insecure_chamber_df <- minority_filtered_df %>%
  filter(level_security_60 == "insecure")

majority_insecure_chamber_df <- majority_filtered_df %>%
  filter(level_security_60 == "insecure")


freshman.legislators <- minority_filtered_df %>%
  filter(first_elected >= 2006 & seniority == 1) %>%
  filter(first_elected >= 2008 | state %in% c("ok", "pa"))

freshman.legislators <- freshman.legislators %>%
  dplyr::select(sles_id, state)

minority_complete_observations <- minority_filtered_df %>%
  semi_join(freshman.legislators, by = c("state", "sles_id"))

minority_complete_observations <- minority_complete_observations %>%
  filter(first_elected >= 2008 | state %in% c("ok", "pa"))

minority_complete_observations <- minority_complete_observations %>%
  filter(first_elected != 2004)

summary(minority_complete_observations$first_elected)

# Retain only unique observations for each combination of sles_id, state, chamber, term 
minority_complete_observations <- minority_complete_observations %>%
  distinct(sles_id, state, term, chamber, .keep_all = TRUE)

minority_secure_chamber_df_complete_obs <- minority_complete_observations %>%
  filter(level_security_60 == "secure")

minority_insecure_chamber_df_complete_obs <- minority_complete_observations %>%
  filter(level_security_60 == "insecure")

# Table A.11 #
# DV: bipartisan cosponsorship offered
# IV: seniority 

# Complete observations sub-sample 

test2.secure.complete.obs <- felm(
  formula = proportion_direct_bipartisan_bills ~ seniority + SM_median_diff + gov_same_party +
    comm_chair + Leader_minleader + power_comm + ideo_med_distance +
    vote_share + minority_party_incumbency_rate| sles_id + chamber | 0 | sles_id, data = minority_secure_chamber_df_complete_obs
)

summary(test2.secure.complete.obs)

test2.insecure.complete.obs <- felm(
  formula = proportion_direct_bipartisan_bills ~ seniority  +SM_median_diff + gov_same_party +
    comm_chair + Leader_minleader + power_comm + ideo_med_distance +
    vote_share + minority_party_incumbency_rate| sles_id + chamber | 0 | sles_id, data = minority_insecure_chamber_df_complete_obs
)

summary(test2.insecure.complete.obs)

test2.table <- list(test2.insecure.complete.obs, test2.secure.complete.obs)

# This entire table is reported in Table A.11 in the Supplementary Materials
stargazer(test2.table, type = "latex", digits = 3, out = "Figures and Tables/H2_test_60.tex")


# Table A.12 #
# DV: bipartisan cosponsorship attracted
# IV: bipartisan cosponsorship + lagged bipartisan cosponsorship offered

minority.secure.T <- felm(
  formula = average_prop_bill_bipartisan~ proportion_direct_bipartisan_bills + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(minority.secure.T)

minority.secure.T.minus.1 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  +SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(minority.secure.T.minus.1)

minority.secure.T.minus.2 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered_T2 + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted  + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(minority.secure.T.minus.2)

minority.insecure.T <- felm(
  formula = average_prop_bill_bipartisan~ proportion_direct_bipartisan_bills + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(minority.insecure.T)

minority.insecure.T.minus.1 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(minority.insecure.T.minus.1)

minority.insecure.T.minus.2 <- felm(
  formula = average_prop_bill_bipartisan~ lagged_offered_T2 + lagged_attracted + 
    avg_expected_bipartisan_cosponsors_attracted + SM_median_diff + gov_same_party +
    seniority + comm_chair +  Leader_minleader +
    power_comm + ideo_med_distance + female + race_asian + race_black + race_hispanic +
    vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(minority.insecure.T.minus.2)


test3.table <- list(minority.insecure.T, minority.insecure.T.minus.1, minority.insecure.T.minus.2,
                    minority.secure.T, minority.secure.T.minus.1, minority.secure.T.minus.2)

# This entire table is reported in Table A.12 in the Supplementary Materials
stargazer(test3.table, type = "latex", digits = 3, out = "Figures and Tables/H3_test_60.tex")

# Table A.13 
# Substantive (insecure + minority)

s.bills.insecure <- felm(
  formula = s_bills  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.bills.insecure)

s.aic.insecure <- felm(
  formula = s_aic  ~ lagged_offered  + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share|state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.aic.insecure)

s.abc.insecure <- felm(
  formula = s_abc  ~lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.abc.insecure)

s.pass.insecure <- felm(
  formula = s_pass  ~ lagged_offered +expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.pass.insecure)


substantive.insecure.minority <- list(s.bills.insecure, s.aic.insecure, s.abc.insecure, s.pass.insecure)

# This entire table is reported in Table A.13 in the Supplementary Materials
stargazer(substantive.insecure.minority, type = "latex", digits = 3, out = "Figures and Tables/H4_test_insecure_60.tex")


# Table A.14
# Substantive (secure + minority)

s.bills.secure <- felm(
  formula = s_bills ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share| state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.bills.secure)

s.aic.secure <- felm(
  formula = s_aic  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.aic.secure)

s.abc.secure <- felm(
  formula = s_abc  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.abc.secure)

s.pass.secure <- felm(
  formula = s_pass ~ lagged_offered  + expected_bipartisan_cosponsorships_offered   + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.pass.secure)


substantive.secure.minority <- list(s.bills.secure, s.aic.secure, s.abc.secure, s.pass.secure)

# This entire table is reported in Table A.14 in the Supplementary Materials
stargazer(substantive.secure.minority, type = "latex", digits = 3, out = "Figures and Tables/H4_test_secure_60.tex")



##################################################################
## Modeling Legislative Success Rates ## 
# Tables A.15 and A.16

# Alternative DV for Test of H4: proportion of successful legislation (relative to all members' substantive bills)
# Test 4 alternative DV (success rate at each stage)

success.aic.insecure <- felm(
  formula = success_rate_aic ~ lagged_offered + expected_bipartisan_cosponsorships_offered +
    SM_median_diff + gov_same_party + seniority + comm_chair + Leader_minleader + power_comm +
    ideo_med_distance + female + race_asian + race_black + race_hispanic + vote_share |
    state + term + chamber | 0 | sles_id,
  data = minority_insecure_chamber_df
)

summary(success.aic.insecure)

success.aic.secure <- felm(
  formula = success_rate_aic ~ lagged_offered + expected_bipartisan_cosponsorships_offered +
    SM_median_diff + gov_same_party + seniority + comm_chair + Leader_minleader + power_comm +
    ideo_med_distance + female + race_asian + race_black + race_hispanic + vote_share |
    state + term + chamber | 0 | sles_id,
  data = minority_secure_chamber_df
)

summary(success.aic.secure)


success.abc.insecure <- felm(
  formula = success_rate_abc ~ lagged_offered + expected_bipartisan_cosponsorships_offered +
    SM_median_diff + gov_same_party + seniority + comm_chair + Leader_minleader + power_comm +
    ideo_med_distance + female + race_asian + race_black + race_hispanic + vote_share |
    state + term + chamber | 0 | sles_id,
  data = minority_insecure_chamber_df
)

summary(success.abc.insecure)

success.abc.secure <- felm(
  formula = success_rate_abc ~ lagged_offered + expected_bipartisan_cosponsorships_offered +
    SM_median_diff + gov_same_party + seniority + comm_chair + Leader_minleader + power_comm +
    ideo_med_distance + female + race_asian + race_black + race_hispanic + vote_share |
    state + term + chamber | 0 | sles_id,
  data = minority_secure_chamber_df
)

summary(success.abc.secure)

success.pass.insecure <- felm(
  formula = success_rate_pass ~ lagged_offered + expected_bipartisan_cosponsorships_offered +
    SM_median_diff + gov_same_party + seniority + comm_chair + Leader_minleader + power_comm +
    ideo_med_distance + female + race_asian + race_black + race_hispanic + vote_share |
    state + term + chamber | 0 | sles_id,
  data = minority_insecure_chamber_df
)

summary(success.pass.insecure)


success.pass.secure <- felm(
  formula = success_rate_pass ~ lagged_offered + expected_bipartisan_cosponsorships_offered +
    SM_median_diff + gov_same_party + seniority + comm_chair + Leader_minleader + power_comm +
    ideo_med_distance + female + race_asian + race_black + race_hispanic + vote_share |
    state + term + chamber | 0 | sles_id,
  data = minority_secure_chamber_df
)

summary(success.pass.secure)



success.insecure.minority <- list(success.aic.insecure, success.abc.insecure, success.pass.insecure)

# This entire table is reported in Table A.15 in the Supplementary Materials
stargazer(success.insecure.minority, type = "latex", digits = 3, out = "Figures and Tables/H4_test_insecure_success_robustness.tex")

success.secure.minority <- list(success.aic.secure, success.abc.secure, success.pass.secure)

# This entire table is reported in Table A.16 in the Supplementary Materials
stargazer(success.secure.minority, type = "latex", digits = 3, out = "Figures and Tables/H4_test_secure_success_robustness.tex")


## Pairwise comparisons ##

s.bills.insecure <- felm(
  formula = s_bills  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.bills.insecure)

s.aic.insecure <- felm(
  formula = s_aic  ~ lagged_offered  + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share|state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.aic.insecure)

s.abc.insecure <- felm(
  formula = s_abc  ~lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.abc.insecure)

s.pass.insecure <- felm(
  formula = s_pass  ~ lagged_offered +expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_insecure_chamber_df
)

summary(s.pass.insecure)



# Substantive (secure + minority)

s.bills.secure <- felm(
  formula = s_bills ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share| state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.bills.secure)

s.aic.secure <- felm(
  formula = s_aic  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber  | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.aic.secure)

s.abc.secure <- felm(
  formula = s_abc  ~ lagged_offered + expected_bipartisan_cosponsorships_offered  + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.abc.secure)

s.pass.secure <- felm(
  formula = s_pass ~ lagged_offered  + expected_bipartisan_cosponsorships_offered   + SM_median_diff + gov_same_party + seniority + 
    comm_chair +  Leader_minleader + power_comm + ideo_med_distance  + female + race_asian + race_black + race_hispanic  
  + vote_share | state + term + chamber | 0 | sles_id, data = minority_secure_chamber_df
)

summary(s.pass.secure)


# Define models from earlier estimation
models <- list(
  s.bills.secure   = s.bills.secure,
  s.aic.secure     = s.aic.secure,
  s.abc.secure     = s.abc.secure,
  s.pass.secure    = s.pass.secure,
  s.bills.insecure = s.bills.insecure,
  s.aic.insecure   = s.aic.insecure,
  s.abc.insecure   = s.abc.insecure,
  s.pass.insecure  = s.pass.insecure
)

dv_names <- c("s_bills", "s_aic", "s_abc", "s_pass")
groups <- c("secure", "insecure")
data_frames <- list(
  secure = minority_secure_chamber_df,
  insecure = minority_insecure_chamber_df
)

# Initialize containers
set.seed(1111)
sims <- 1000
CI_list <- list()
CI_834_list <- list()
coef_values <- c()
mean_values <- c()
p_values <- c()
bill_type <- c()
stage <- c()
model_ids <- c()

# Simulate effects and extract quantities of interest
for (group in groups) {
  for (dv in dv_names) {
    model_name <- paste0("s.", gsub("s_", "", dv), ".", group)
    model <- models[[model_name]]
    
    if (!is.null(coef(model)) && "lagged_offered" %in% names(coef(model))) {
      sim <- as.data.frame(mvrnorm(sims, mu = coef(model), Sigma = model$vcv))
      dv_mean <- mean(data_frames[[group]][[dv]], na.rm = TRUE)
      sim$percent_increase <- (sim$lagged_offered * 100) / dv_mean
      
      CI_95 <- quantile(sim$percent_increase, probs = c(0.025, 0.975))
      CI_834 <- quantile(sim$percent_increase, probs = c(0.083, 0.917))
      
      CI_list[[model_name]] <- CI_95
      CI_834_list[[model_name]] <- list(
        group = str_to_title(group),
        stage = toupper(gsub("s_", "", dv)),
        ci_low = CI_834[1],
        ci_high = CI_834[2]
      )
      
      coef_values <- c(coef_values, coef(model)["lagged_offered"])
      mean_values <- c(mean_values, dv_mean)
      p_values <- c(p_values, summary(model)$coefficients["lagged_offered", "Pr(>|t|)"])
      bill_type <- c(bill_type, ifelse(group == "secure", "Secure", "Insecure"))
      stage <- c(stage, toupper(gsub("s_", "", dv)))
      model_ids <- c(model_ids, model_name)
    }
  }
}

# Create data frame of increased activity (for plotting, optional)
increased.activity.df <- data.frame(
  model = model_ids,
  bill_type = factor(bill_type, levels = c("Secure", "Insecure")),
  stage = factor(stage, levels = c("BILLS", "AIC", "ABC", "PASS")),
  coef = coef_values,
  mean_value = mean_values,
  increased_activity = (coef_values * 100) / mean_values,
  ymin = sapply(CI_list, function(ci) ci[1]),
  ymax = sapply(CI_list, function(ci) ci[2]),
  p_value = p_values
)

# Force 95% CIs to cross 0 if not significant (optional, for plotting only)
increased.activity.df <- increased.activity.df %>%
  mutate(
    ymin = ifelse(p_value > 0.1 & ymin > 0, 0, ymin),
    ymax = ifelse(p_value > 0.1 & ymax < 0, 0, ymax)
  )

# Build data frame of 83.4% CI bounds by model
ci_df <- do.call(rbind, lapply(names(CI_834_list), function(name) {
  row <- CI_834_list[[name]]
  data.frame(model = name, group = row$group, stage = row$stage,
             ci_low = row$ci_low, ci_high = row$ci_high)
}))

# Define function to check whether CIs overlap
check_overlap <- function(ci1, ci2) {
  !(ci1["ci_high"] < ci2["ci_low"] || ci2["ci_high"] < ci1["ci_low"])
}

# Compare 83.4% CIs within each group
overlap_results <- bind_rows(
  lapply(split(ci_df, ci_df$group), function(group_df) {
    pairs <- combn(seq_len(nrow(group_df)), 2)
    res <- apply(pairs, 2, function(idx) {
      row1 <- group_df[idx[1], ]
      row2 <- group_df[idx[2], ]
      overlap <- check_overlap(row1, row2)
      data.frame(
        group = row1$group,
        stage1 = row1$stage,
        stage2 = row2$stage,
        ci1_low = row1$ci_low,
        ci1_high = row1$ci_high,
        ci2_low = row2$ci_low,
        ci2_high = row2$ci_high,
        overlap = overlap,
        significant = !overlap
      )
    })
    do.call(rbind, res)
  })
)

# View results
print(overlap_results)



# Create a cleaner version of overlap_results
overlap_table <- overlap_results %>%
  mutate(
    `Majority Security` = group,
    `Stage 1` = stage1,
    `Stage 2` = stage2,
    `83.4% CI (Stage 1)` = sprintf("[%.2f, %.2f]", ci1_low, ci1_high),
    `83.4% CI (Stage 2)` = sprintf("[%.2f, %.2f]", ci2_low, ci2_high),
    `CIs Overlap?` = ifelse(overlap, "Yes", "No")
  ) %>%
  dplyr::select(`Majority Security`, `Stage 1`, `Stage 2`,
                `83.4% CI (Stage 1)`, `83.4% CI (Stage 2)`, `CIs Overlap?`)

kable(overlap_table, format = "latex", booktabs = TRUE, linesep = "", caption = "Pairwise 83.4\\% Confidence Interval Overlap Tests by Majority Security") %>%
  kable_styling(
    latex_options = c("hold_position", "striped"),
    font_size = 10,
    position = "center"
  ) %>%
  column_spec(1:6, width = "2.8cm") %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" " = 3, "83.4% Confidence Intervals" = 2, " " = 1)) %>%
  kableExtra::scroll_box(height = "auto", width = "100%")


## QI Simulation of Increased Legislative Activity ##

# Figure A.5 

# Increased activity = (regression coef * 100) / mean(coef among all minority members) 

# Define models and associated data frames from Figure 2 in-text
models <- list(
  s.bills.secure   = s.bills.secure.fig2,
  s.aic.secure     = s.aic.secure.fig2,
  s.abc.secure     = s.abc.secure.fig2,
  s.pass.secure    = s.pass.secure.fig2,
  s.bills.insecure = s.bills.insecure.fig2,
  s.aic.insecure   = s.aic.insecure.fig2,
  s.abc.insecure   = s.abc.insecure.fig2,
  s.pass.insecure  = s.pass.insecure.fig2
)

dv_names <- c("s_bills", "s_aic", "s_abc", "s_pass")
groups <- c("secure", "insecure")
data_frames <- list(
  secure = minority_secure_chamber_df,
  insecure = minority_insecure_chamber_df
)

# Initialize containers
set.seed(1111)
sims <- 1000
CI_list <- list()
coef_values <- c()
mean_values <- c()
p_values <- c()
bill_type <- c()
stage <- c()
model_ids <- c()

# Simulate effects and extract quantities of interest
for (group in groups) {
  for (dv in dv_names) {
    model_name <- paste0("s.", gsub("s_", "", dv), ".", group)
    model <- models[[model_name]]
    
    if (!is.null(coef(model)) && "lagged_offered" %in% names(coef(model))) {
      # Simulate coefficient draws
      sim <- as.data.frame(mvrnorm(sims, mu = coef(model), Sigma = model$vcv))
      dv_mean <- mean(data_frames[[group]][[dv]], na.rm = TRUE)
      sim$percent_increase <- (sim$lagged_offered * 100) / dv_mean
      
      # Store results
      CI <- quantile(sim$percent_increase, probs = c(0.025, 0.975))
      CI_list[[model_name]] <- CI
      
      coef_values <- c(coef_values, coef(model)["lagged_offered"])
      mean_values <- c(mean_values, dv_mean)
      p_values <- c(p_values, summary(model)$coefficients["lagged_offered", "Pr(>|t|)"])
      label <- ifelse(group == "secure",
                      "Secure",
                      "Insecure")
      bill_type <- c(bill_type, label)
      stage <- c(stage, toupper(gsub("s_", "", dv)))
      model_ids <- c(model_ids, model_name)
    }
  }
}

# Create final data frame
increased.activity.df <- data.frame(
  model = model_ids,
  bill_type = factor(bill_type, levels = c(
    "Secure",
    "Insecure")),
  stage = factor(stage, levels = c("BILLS", "AIC", "ABC", "PASS")),
  coef = coef_values,
  mean_value = mean_values,
  increased_activity = (coef_values * 100) / mean_values,
  ymin = sapply(CI_list, function(ci) ci[1]),
  ymax = sapply(CI_list, function(ci) ci[2]),
  p_value = p_values
)

# CI to cross 0 if p > 0.1
increased.activity.df <- increased.activity.df %>%
  mutate(
    ymin = ifelse(p_value > 0.1 & ymin > 0, 0, ymin),
    ymax = ifelse(p_value > 0.1 & ymax < 0, 0, ymax)
  )

# Reorder facets: Insecure first
increased.activity.df$bill_type <- factor(
  increased.activity.df$bill_type,
  levels = c("Insecure", "Secure")
)

p <- ggplot(increased.activity.df,
            aes(x = stage, y = increased_activity,
                ymin = ymin, ymax = ymax,
                alpha = (p_value < 0.05),
                shape = (p_value < 0.05),
                color = bill_type)) +
  # firebrick dashed 0-line
  geom_hline(yintercept = 0, linetype = "dashed",
             linewidth = 0.8, color = "firebrick") +
  
  # points with error bars
  geom_pointrange(size = 0.9, linewidth = 0.9) +
  
  # alpha/shape mapping: TRUE = significant, FALSE = not significant
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 1), guide = "none") +
  
  facet_wrap(~ bill_type, labeller = labeller(bill_type = c(
    "Secure"   = "Secure Majority Chambers",
    "Insecure" = "Insecure Majority Chambers"
  ))) +
  
  scale_y_continuous(limits = c(-25, 225), breaks = seq(-25, 225, 50)) +
  scale_color_manual(values = c(
    "Secure"   = "#377eb8",
    "Insecure" = "#1b9e77"
  )) +
  
  labs(
    x = "",
    y = "Estimated % Increase in Count of \n Minority Party Sponsored Bills",
    color = "Majority Security:"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 13, face = "bold"),       # bold facet labels
    legend.position = "bottom",
    legend.title = element_text(size = 12),     # bold legend title
    legend.text = element_text(size = 11),
    legend.box = "vertical",
    legend.box.background = element_rect(color = "black", size = 0.5),
    legend.box.margin = margin(t = 6, r = 6, b = 6, l = 6),
    axis.text = element_text(size = 11, color = "black", face = "bold"), # bold axis ticks
    axis.title = element_text(size = 12, color = "black", face = "bold"), # bold axis labels
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

print(p)


## 
########################################################################################


