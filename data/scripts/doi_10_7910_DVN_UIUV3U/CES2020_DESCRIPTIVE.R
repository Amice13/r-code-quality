# *****************************************************************
# OVERVIEW ####
# *****************************************************************
# CES20_DESCRIPTIVE.R
# Descriptive Statistics Analyses from the CES 2020 Data
# Jennifer Lin and Kristin Lunz Trujillo
# Created On: 2021 08 11

# *** The following script generates descriptive statistics for
#   the paper and includes code on components that are in the main text
#   and Appendix. ***

# ** Before running this file, ensure that you have ran
#   - Functions.R
#   - Load_RUCA.R
#   - CES2020_CLEAN.R
#  or components of this code will result in an error. See 
#  00_GENERAL.R for more details **

# *****************************************************************
# POLITICAL BEHAVIOR DESCRIPTIVE STATISTICS ####
# *****************************************************************

# *** The following code prepares the data for plotting and 
#   produces the respective figures. Each starts with a chunk of
#   code for preparing the plots, followed by code for the graph
#   made using ggplot. The sections are in this order and are 
#   indicated as such for the first figure only. ***

# For each set, sub-figure A analyzes the data by self-identified
#   place of residence and sub figure B analyzes data based on
#   RUCA score.

# General Political Participation

## Figure 1a ####

# Calculate survey means for each political behavior and prepare
#   for plotting
CES_participation <- CCES_20_survey %>% 
  group_by(residence) %>% 
  summarise_at(vars(starts_with("p_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  filter(!is.na(residence)) %>% 
  mutate(
    variable = case_when(
      variable == "p_VOTED" ~ "Voted in the 2020 Election",
      variable == "p_meeting" ~ "Attend local political meetings",
      variable == "p_sign" ~ "Put up a political sign",
      variable == "p_work_campaign" ~ "Work for a candidate or campaign",
      variable == "p_protest" ~ "Attend a political protest, \n march or demonstration",
      variable == "p_contact" ~ "Contact a public official",
      variable == "p_donate_money" ~ "Donate money to a \n candidate, campaign, \n or political organization",
      variable == "p_donate_blood" ~ "Donate blood",
      variable == "p_NOTA" ~ "None of these"
    ),
    variable = factor(variable, levels = variable, labels = variable)
  )

# Generate plot
ggplot(CES_participation, aes(x = reorder(variable, desc(variable)), y = mean, fill = residence))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  scale_fill_brewer(
    palette = "Greys",
    name = "Self-Identified\nResidence")+
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  labs(
    title = "Political Participation in 2020",
    subtitle = "By Self-Identified Place of Residence"
  )+
  ylim(0, .8)+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title  = element_text(size = 14, colour="black"),
    axis.text.x = element_text(size = 12, colour="black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, colour="black"),
    legend.position = 'bottom'
  )

## Figure 1b ####

CES_participation_ruca <- CCES_20_survey %>% 
  group_by(RUCAx) %>% 
  summarise_at(vars(starts_with("p_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  filter(!is.na(RUCAx)) %>% 
  mutate(
    variable = case_when(
      variable == "p_VOTED" ~ "Voted in the 2020 Election",
      variable == "p_meeting" ~ "Attend local political meetings",
      variable == "p_sign" ~ "Put up a political sign",
      variable == "p_work_campaign" ~ "Work for a candidate or campaign",
      variable == "p_protest" ~ "Attend a political protest, \n march or demonstration",
      variable == "p_contact" ~ "Contact a public official",
      variable == "p_donate_money" ~ "Donate money to a \n candidate, campaign, \n or political organization",
      variable == "p_donate_blood" ~ "Donate blood",
      variable == "p_NOTA" ~ "None of these"
    ),
    variable = factor(variable, levels = variable, labels = variable)
  )

ggplot(CES_participation_ruca, aes(x = reorder(variable, desc(variable)), y = mean, fill = RUCAx))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  scale_fill_brewer(
    palette = "Greys",
    name="RUCA\nCategory", 
    labels=c(
      "1" = "Urban",
      "2" = "City",
      "3" = "Town",
      "4" = "Rural"))+
  labs(
    title = "Political Participation in 2020",
    subtitle = "By RUCA Category"
  )+
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  ylim(0, .8)+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title  = element_text(size = 14, colour="black"),
    axis.text.x = element_text(size = 12, colour="black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, colour="black"),
    legend.position = 'bottom'
  )

# Social Media Participation

## Figure 2a ####

CES_sm_participation <- CCES_20_survey %>% 
  group_by(residence) %>% 
  summarise_at(vars(starts_with("sm_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  filter(!is.na(residence)) %>% 
  mutate(
    variable = case_when(
      variable == "sm_post" ~ "Posted Media\n about Politics",
      variable == "sm_comment" ~ "Posted Comment\n about Politics",
      variable == "sm_read" ~ "Read Story/\nWatched Video\n about Politics",
      variable == "sm_event" ~ "Followed a\n Political Event",
      variable == "sm_forward" ~ "Forwarded Media\n about Politics"
    ),
    variable = factor(variable, levels = variable, labels = variable)
  )

ggplot(CES_sm_participation, aes(x = reorder(variable, desc(variable)), y = mean, fill = residence))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  scale_fill_brewer(
    palette = "Greys",
    name = "Self-Identified\nResidence")+
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  labs(
    title = "Political Social Media Participation in 2020",
    subtitle = "By Self-Identified Place of Residence"
  )+
  ylim(0, .8)+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title  = element_text(size = 14, colour="black"),
    axis.text.x = element_text(size = 12, colour="black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, colour="black"),
    legend.position = 'bottom'
  )

## Figure 2b ####

CES_sm_participation_ruca <- CCES_20_survey %>% 
  group_by(RUCAx) %>% 
  summarise_at(vars(starts_with("sm_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  filter(!is.na(RUCAx)) %>% 
  mutate(
    variable = case_when(
      variable == "sm_post" ~ "Posted Media\n about Politics",
      variable == "sm_comment" ~ "Posted Comment\n about Politics",
      variable == "sm_read" ~ "Read Story/\nWatched Video\n about Politics",
      variable == "sm_event" ~ "Followed a\n Political Event",
      variable == "sm_forward" ~ "Forwarded Media\n about Politics"
    ),
    variable = factor(variable, levels = variable, labels = variable)
  )

ggplot(CES_sm_participation_ruca, aes(x = reorder(variable, desc(variable)), y = mean, fill = RUCAx))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  scale_fill_brewer(
    palette = "Greys",
    name="RUCA\nCategory", 
    labels=c(
      "1" = "Urban",
      "2" = "City",
      "3" = "Town",
      "4" = "Rural"))+
  labs(
    title = "Political Social Media Participation in 2020",
    subtitle = "By RUCA Category"
  )+
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  ylim(0, .8)+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title  = element_text(size = 14, colour="black"),
    axis.text.x = element_text(size = 12, colour="black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, colour="black"),
    legend.position = 'bottom'
  )

# *****************************************************************
# SUMMARY STATISTICS ####
# *****************************************************************

# The code below generates survey weighted summary statistcs
#   used in Supplemental Appendix C.

# General Political Behavior

Participation <- CCES_20_survey %>% 
  summarise_at(vars(starts_with("p_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "p_VOTED" ~ "Voted in the 2020 Election",
      variable == "p_meeting" ~ "Attend local political meetings",
      variable == "p_sign" ~ "Put up a political sign",
      variable == "p_work_campaign" ~ "Work for a candidate or campaign",
      variable == "p_protest" ~ "Attend a political protest, march or demonstration",
      variable == "p_contact" ~ "Contact a public official",
      variable == "p_donate_money" ~ "Donate money to a candidate, campaign, or political organization",
      variable == "p_donate_blood" ~ "Donate blood",
      variable == "p_NOTA" ~ "None of these"
    )
  )

# Social Media Political Behavior

Participation_sm <- CCES_20_survey %>% 
  summarise_at(vars(starts_with("sm_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "sm_post" ~ "Posted Media about Politics",
      variable == "sm_comment" ~ "Posted Comment about Politics",
      variable == "sm_read" ~ "Read Story/Watched Video about Politics",
      variable == "sm_event" ~ "Followed a Political Event",
      variable == "sm_forward" ~ "Forwarded Media about Politics"
    )
  )

# *****************************************************************
# CHI-SQUARED TESTS ####
# *****************************************************************

# *** The following code generates the results for the Chi-Squared
#   tests in Supplemental Appendix D. The first set uses 
#   self-identified place of residence and the second uses RUCA 
#   scores. Both processes are similar so only the first is 
#   commented and labelled***

## Self Identified Place of Residence ####

# First, get the variables that are of interest, which includes
#   the political participation (starts with "p_") and social media
#   (starts with "sm_") items.
behaviors <- CES_20_clean %>% 
  select(starts_with(c("p_", "sm_"))) 

# Using the chi_squ_svy() function, we can calculate the chi-squared
#   test using the variables from the original dataset, matched to the 
#   respective survey variable, using self-identified place of residence
#   as the predicting variable.
chi_2020 <- chi_squ_svy(behaviors, 1:14, design = CCES_20_survey)

# The following code allows us to parse the results from the list that the
#   previous code creates. For the Supplemental Appendix, we were 
#   interested in the estimator (chi-squared) and the significance (p-value)

# Create empty vector to store Chi-Squared estimate
x_sq <- c()
# Create empty vector to store p-value
x_p <- c()

# Use the following for loop to parse the chi-squared statistic
#   and p value from each of the analyses stored in the resulting
#   list from above.
for (i in 1:length(chi_2020)) {
  x_sq[i] <- chi_2020[[i]]$statistic
  x_p[i] <- chi_2020[[i]]$p.value
}

# The following code format the output from the for loop above
#   to resemble something that can be exported to Latex or word.
chi_sq_20_resid <- data.frame(
  Behaviors = labels_chi, 
  `X-Squared` = x_sq,
  `p.value` = x_p
) %>% 
  mutate(
    signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.05 ~ "**",
      p.value > 0.05 & p.value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    `2020 Self-Identified Residence` = paste0(
      round(`X.Squared`, 2), signif
    )
  ) %>% 
  select(Behaviors, `2020 Self-Identified Residence`)

## RUCA designation ####

chi_2020_ruca <- chi_squ_svy_ruca(behaviors, 1:14, design = CCES_20_survey)

x_sq <- c()
x_p <- c()

for (i in 1:length(chi_2020_ruca)) {
  x_sq[i] <- chi_2020_ruca[[i]]$statistic
  x_p[i] <- chi_2020_ruca[[i]]$p.value
}

chi_sq_20_ruca <- data.frame(
  Behaviors = labels_chi, 
  `X-Squared` = x_sq,
  `p.value` = x_p
) %>% 
  mutate(
    signif = case_when(
      p.value <= 0.001 ~ "***",
      p.value > 0.001 & p.value <= 0.05 ~ "**",
      p.value > 0.05 & p.value <= 0.1 ~ "*",
      TRUE ~ ""
    ),
    `2020 RUCA` = paste0(
      round(`X.Squared`, 2), signif
    )
  ) %>% 
  select(Behaviors, `2020 RUCA`)

# *****************************************************************
# ADDITIVE BEHAVIOR SERIES ####
# *****************************************************************

# The code in the following section replicates the additive scales
#   distributions represented in Supplemental Appendix Figures 1a 
#   (general political behavior) and 1b (social media).

# As with previous sections, the process is the same for both 
#   subsections, so the process will only be discussed in detail 
#   once, under the first subsection.

## General Political Behaviors ####

# For plotting, the following generates a survey weighted
#   total for each level in the series and the respective 
#   95% confidence interval.
dist_behavior <- CCES_20_survey %>% 
  mutate(
    behavior_series = as.factor(behavior_series)
  ) %>% 
  group_by(behavior_series) %>% 
  summarise(
    n = survey_total()
  ) %>% 
  mutate(
    lwr = n - 1.96*n_se,
    upr = n + 1.96*n_se
  ) %>% 
  filter(!is.na(behavior_series))

# Generate Plot
ggplot(dist_behavior, aes(x = behavior_series, y = n))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black", fill = "grey75")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  theme_bw()+
  xlab("Number of Behaviors")+
  ylab("Number of People")+
  labs(
    title = "Political Participation in 2020",
    subtitle = "General Political Behaviors"
  )+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title  = element_text(size = 14, colour="black"),
    axis.text.x = element_text(size = 12, colour="black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, colour="black"),
    legend.position = 'bottom'
  )

## Social Media Behaviors ####

dist_social <- CCES_20_survey %>% 
  mutate(
    social_series = as.factor(social_series)
  ) %>% 
  group_by(social_series) %>% 
  summarise(
    n = survey_total()
  ) %>% 
  mutate(
    lwr = n - 1.96*n_se,
    upr = n + 1.96*n_se
  ) %>% 
  filter(!is.na(social_series))

ggplot(dist_social, aes(x = social_series, y = n))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black", fill = "grey75")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  theme_bw()+
  xlab("Number of Behaviors")+
  ylab("Number of People")+
  labs(
    title = "Political Participation in 2020",
    subtitle = "Social Media"
  )+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title  = element_text(size = 14, colour="black"),
    axis.text.x = element_text(size = 12, colour="black", angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 12, colour="black"),
    legend.position = 'bottom'
  )

