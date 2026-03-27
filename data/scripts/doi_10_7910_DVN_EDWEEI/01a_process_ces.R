# Alexander F. Gazmararian
# agazmararian@gmail.com

library(haven)
library(here)
library(tidyverse)

ces <- read_dta(here("Data", "input", "survey", "ces_panel_2010-2014", "CCES_Panel_Full3waves_VV_V4.dta"))
ces <- ces %>% mutate(across(where(is.labelled), ~ as_factor(.x)))
ces <- ces %>% mutate(across(where(is.factor), ~ droplevels(.x)))

ces <- ces %>%
  rename(
    voted_10 = CC10_401,
    voted_12 = CC12_401,
    voted_14 = CC14_401,
    housevote_10 = CC10_412,
    housevote_12 = CC12_412,
    housevote_14 = CC14_412,
    jobsenvt_10 = CC10_325,
    jobsenvt_12 = CC12_325,
    jobsenvt_14 = CC14_325,
    religimp_10 = pew_religimp_10,
    religimp_12 = pew_religimp_12,
    religimp_14 = pew_religimp_14,
    zip_10 = reszip_post_10,
    zip_12 = reszip_post_12,
    zip_14 = reszip_post_14
  )

g <- ces %>%
  mutate(across(c(starttime_10, starttime_12, starttime_14), ~ as.character(.x))) %>%
  pivot_longer(
    cols = c(
      countyfips_10, countyfips_12, countyfips_14,
      inputstate_10, inputstate_12, inputstate_14,
      birthyr_10, birthyr_12, birthyr_14,
      gender_10, gender_12, gender_14,
      educ_10, educ_12, educ_14,
      race_10, race_12, race_14,
      pid3_10, pid3_12, pid3_14,
      pid7_10, pid7_12, pid7_14,
      ideo5_10, ideo5_12, ideo5_14,
      jobsenvt_10, jobsenvt_12, jobsenvt_14,
      voted_10, voted_12, voted_14,
      religimp_10, religimp_12, religimp_14,
      child18_10, child18_12, child18_14,
      zip_10, zip_12, zip_14,
      child18num_10, child18num_12, child18num_14,
      newsint_10, newsint_12, newsint_14,
      faminc_10, faminc_12, faminc_14,
      employ_10, employ_12, employ_14,
      housevote_10, housevote_12, housevote_14,
      votereg_10, votereg_12, votereg_14,
      marstat_10, marstat_12, marstat_14,
      ownhome_10, ownhome_12, ownhome_14,
      starttime_10, starttime_12, starttime_14,
      investor_10, investor_12, investor_14
    ),
    names_to = "variable", values_to = "value"
  ) %>%
  dplyr::select(caseid, variable, value, CC10_321, CC12_321, CC14_321) %>%
  separate(col = variable, into = c("variable", "year"), sep = "_") %>%
  mutate(year = case_when(year == "10" ~ 2010, year == "12" ~ 2012, year == "14" ~ 2014)) %>%
  pivot_wider(id_cols = c(caseid, year, CC10_321, CC12_321, CC14_321), names_from = variable, values_from = value)

g$countyfips <- as.numeric(g$countyfips)
g$starttime <- as.Date(g$starttime)

g <- g %>%
  mutate(
    gw = case_when(
      year == 2010 ~ CC10_321,
      year == 2012 ~ CC12_321,
      year == 2014 ~ CC14_321
    )
  )

g$college <- ifelse(g$educ %in% c("Post-grad", "4-year"), 1, 0)
g <- g %>%
  mutate(
    edu_scale = case_when(
      educ == "No HS" ~ 1,
      educ == "High school graduate" ~ 2,
      educ == "Some college" ~ 3,
      educ == "2-year" ~ 4,
      educ == "4-year" ~ 5,
      educ == "Post-grad" ~ 6
    )
  )

g$dem <- with(g, ifelse(pid3 == "Democrat", 1, 0))
g$rep <- with(g, ifelse(pid3 == "Republican", 1, 0))
g$pid3 <- factor(g$pid3)
g <- g %>%
  mutate(
    pid7_scale = case_when(
      pid7 == "Strong Republican" ~ 1,
      pid7 == "Not very strong Republican" ~ 2,
      pid7 == "Lean Republican" ~ 3,
      pid7 %in% c("Independent", "Not sure") ~ 4,
      pid7 == "Lean Democrat" ~ 5,
      pid7 == "Not very strong Democrat" ~ 6,
      pid7 == "Strong Democrat" ~ 7
    )
  )

g$employ <- with(g, ifelse(employ == "Full-time", 1, 0))
g %>% pivot_wider(id_cols = caseid, values_from = employ, names_from = year)

# how many people change their gender---none change their gender
g$female <- with(g, ifelse(gender == "Female", 1, 0))
g %>%
  group_by(caseid) %>%
  summarise(female = mean(female, na.rm = TRUE)) %>%
  filter(female != 0 & female != 1)

# create scale for environmental policy views
g <- g %>%
  mutate(
    envt_bin = case_when(
      jobsenvt %in% c("Environment somewhat more important", "Much more important to protect environment even if lose jobs and lower standard of living", "Much more important to protect environment even if we lose jobs or lower our standard of living") ~ 1,
      T ~ 0
    ),
    envt_scale = case_when(
      jobsenvt == "Much more important to protect jobs, even if the environment gets worse" ~ 1,
      jobsenvt == "Much more important to protect jobs, even if environment worse" ~ 1,
      jobsenvt == "Economy somewhat more important" ~ 2,
      jobsenvt == "About the same" ~ 3,
      jobsenvt == "Not sure" ~ 3,
      jobsenvt == "Environment somewhat more important" ~ 4,
      jobsenvt == "Much more important to protect environment even if lose jobs and lower standard of living" ~ 5,
      jobsenvt == "Much more important to protect environment even if we lose jobs or lower our standard of living" ~ 5
    )
  )

# convert income to a linear scale
g$faminc <- factor(g$faminc)
g <-
  g %>%
  mutate(
    faminc_scale = case_when(
      faminc == "less than $10,000" ~ 1,
      faminc == "Less than $10,000" ~ 1,
      faminc == "$10,000 - $14,999" ~ 2,
      faminc == "$15,000 - $19,999" ~ 2,
      faminc == "$10,000 - $19,999" ~ 2,
      faminc == "$20,000 - $24,999" ~ 3,
      faminc == "$20,000 - $29,999" ~ 3,
      faminc == "$25,000 - $29,999" ~ 3,
      faminc == "$30,000 - $39,999" ~ 4,
      faminc == "$40,000 - $49,999" ~ 5,
      faminc == "$50,000 - $59,999" ~ 6,
      faminc == "$60,000 - $69,999" ~ 7,
      faminc == "$70,000 - $79,999" ~ 8,
      faminc == "$80,000 - $99,999" ~ 9,
      faminc == "$100,000 - $119,999" ~ 10,
      faminc == "$120,000 - $149,999" ~ 11,
      faminc == "$150,000 or more" ~ 12,
      faminc == "$150,000 - $199,999" ~ 12,
      faminc == "$250,000 or more" ~ 12,
      faminc == "$350,000 - $499,999" ~ 12,
      faminc == "$500,000 or more" ~ 12,
      faminc == "$250,000 - $349,999" ~ 12,
      faminc == "$200,000 - $249,999" ~ 12,
      T ~ NA_real_
    )
  )
# impute missing income at mean value
g$faminc_scale_imp <- with(g, ifelse(is.na(faminc_scale), mean(faminc_scale, na.rm = TRUE), faminc_scale))
g <-
  g %>%
  mutate(
    income5 = case_when(
      faminc_scale %in% c(1, 2) ~ "Q1", # 10-19k
      faminc_scale %in% c(3, 4, 5, 6) ~ "Q2", # 20-59k
      faminc_scale %in% c(7, 8, 9) ~ "Q3", # 60-99k
      faminc_scale %in% c(10, 11) ~ "Q4", # 100-149k
      faminc_scale %in% c(12) ~ "Q5", # 150k-more
      faminc == "Prefer not to say" ~ "Not say",
      T ~ NA_character_
    )
  )

# create global warming belief scale
g <-
  g %>%
  mutate(
    gw_scale = case_when(
      gw == "Global climate change is not occurring; this is not a real issue." ~ 1,
      gw == "Concern about global climate change is exaggerated.  No action is necessary." ~ 2,
      gw == "We don't know enough about global climate change, and more research is necessary before we take any actions." ~ 3,
      gw == "There is enough evidence that climate change is taking place and some action should be taken." ~ 4,
      gw == "Global climate change has been established as a serious problem, and immediate action is necessary." ~ 5,
      T ~ NA_real_
    ),
    gw_binary = case_when(
      gw == "Global climate change is not occurring; this is not a real issue." ~ 0,
      gw == "Concern about global climate change is exaggerated.  No action is necessary." ~ 0,
      gw == "We don't know enough about global climate change, and more research is necessary before we take any actions." ~ 0,
      gw == "There is enough evidence that climate change is taking place and some action should be taken." ~ 1,
      gw == "Global climate change has been established as a serious problem, and immediate action is necessary." ~ 1,
      T ~ NA_real_
    )
  )

# convert birth year to numeric
g$birthyr <- as.numeric(g$birthyr)

# married
g$married <- ifelse(g$marstat == "Married", 1, 0)

# ideology scale
g <- g %>%
  mutate(
    ideo5_scale = case_when(
      ideo5 == "Very conservative" ~ 1,
      ideo5 == "Very Conservative" ~ 1,
      ideo5 == "Conservative" ~ 2,
      ideo5 == "Moderate" ~ 3,
      ideo5 == "Not sure" ~ 3,
      ideo5 == "Liberal" ~ 4,
      ideo5 == "Very liberal" ~ 5
    )
  )
g <-
  g %>%
  mutate(
    ideo3 = case_when(
      ideo5 %in% c("Conservative", "Very conservative", "Very Conservative") ~ "Conservative",
      ideo5 %in% c("Liberal", "Very liberal") ~ "Liberal",
      ideo5 %in% c("Moderate") ~ "Moderate",
      ideo5 %in% c("Not sure") ~ "Not Sure"
    )
  )

# religious importance
g <- g %>%
  mutate(
    religimp_scale = case_when(
      religimp == "Not at all important" ~ 1,
      religimp == "Not too important" ~ 2,
      religimp == "Somewhat important" ~ 3,
      religimp == "Very important" ~ 4
    )
  )

# home ownership
g$ownhome_bin <- ifelse(g$ownhome == "Own", 1, 0)

# News interest
g$newsint <- factor(g$newsint, ordered = TRUE, levels = c("Don't know", "Hardly at all", "Only now and then", "Some of the time", "Most of the time"))
g$newsint_num <- as.numeric(g$newsint)
g$newsint_bin <- ifelse(g$newsint_num == 5, 1, 0)

# children
g$child18 <- as.integer(g$child18 == "Yes")
g$child18num <- as.numeric(g$child18num)
g$child18num <- ifelse(is.na(g$child18num), 0, g$child18num) # Branching question
g$child18num_bin <- ifelse(g$child18num > 0, 1, 0)
g$treat_child <- ifelse(g$child18num_bin == 1 & g$child18 == 1, 1, 0)
treat_hist <- g %>%
  pivot_wider(names_from = year, values_from = treat_child) %>%
  rename(year10=`2010`,year12=`2012`,year14=`2014`) %>%
  dplyr::select(caseid,year10,year12,year14) %>%
  group_by(caseid) %>%
  summarize(across(year10:year14, ~ sum(.x, na.rm = TRUE))) %>%
  mutate(
    treat_hist = case_when(
      year10 == 0 & year12 == 0 & year14 == 0 ~ "0,0,0",
      year10 == 1 & year12 == 1 & year14 == 1 ~ "1,1,1",
      year10 == 1 & year12 == 0 & year14 == 0 ~ "1,0,0",
      year10 == 0 & year12 == 0 & year14 == 1 ~ "0,0,1",
      year10 == 1 & year12 == 1 & year14 == 0 ~ "1,1,0",
      year10 == 0 & year12 == 1 & year14 == 1 ~ "0,1,1",
      year10 == 1 & year12 == 0 & year14 == 1 ~ "1,0,1",
      year10 == 0 & year12 == 1 & year14 == 0 ~ "0,1,0"
    )
  )
g <- left_join(g, treat_hist, by = "caseid")
g <-
  g %>%
  mutate(
    newparent = case_when(
      # Becoming a parent in 2014, and then panel ends
      treat_hist == "0,0,1" & year == 2014 ~ 1,
      # Becoming parent in 2012, and then treated onward
      treat_hist == "0,1,1" & year == 2012 ~ 1,
      treat_hist == "0,1,1" & year == 2014 ~ 1,
      is.na(treat_hist) ~ NA_real_,
      T ~ 0
    )
  )


# Matching preparation
# Income
g$income_q1 <- ifelse(g$income5 == "Q1", 1, 0)
g$income_q2 <- ifelse(g$income5 == "Q2", 1, 0)
g$income_q3 <- ifelse(g$income5 == "Q3", 1, 0)
g$income_q4 <- ifelse(g$income5 == "Q4", 1, 0)
g$income_q5 <- ifelse(g$income5 == "Q5", 1, 0)
g$income_ns <- ifelse(g$income5 == "Not Say", 1, 0)

# Partisanship
g$pid7_stgdem <- ifelse(g$pid7 == "Strong Democrat", 1, 0)
g$pid7_leandem <- ifelse(g$pid7 == "Lean Democrat", 1, 0)
g$pid7_weakdem <- ifelse(g$pid7 == "Not very strong Democrat", 1, 0)
g$pid7_weakrep <- ifelse(g$pid7 == "Not very strong Republican", 1, 0)
g$pid7_leanrep <- ifelse(g$pid7 == "Lean Republican", 1, 0)
g$pid7_stgrep <- ifelse(g$pid7 == "Strong Republican", 1, 0)

# Ideology
g$ideo_vcon <- ifelse(g$ideo5 == "Very conservative", 1, 0)
g$ideo_con <- ifelse(g$ideo5 == "Conservative", 1, 0)
g$ideo_lib <- ifelse(g$ideo5 == "Liberal", 1, 0)
g$ideo_vlib <- ifelse(g$ideo5 == "Very liberal", 1, 0)
g$notsureideo <- as.integer(g$ideo3 == "Not Sure")

g$lib <- ifelse(g$ideo3 == "Liberal", 1, 0)
g$con <- ifelse(g$ideo3 == "Conservative", 1, 0)

# Religiosity
g$relig_1 <- ifelse(g$religimp == "Not at all important", 1, 0)
g$relig_2 <- ifelse(g$religimp == "Not too important", 1, 0)
g$relig_3 <- ifelse(g$religimp == "Somewhat important", 1, 0)
g$relig_4 <- ifelse(g$religimp == "Very important", 1, 0)

# Race
g$black <- ifelse(g$race == "Black", 1, 0)
g$latino <- ifelse(g$race == "Hispanic", 1, 0)

# Education
g$edu_1 <- as.integer(g$educ %in% c("No HS", "High school graduate"))
g$edu_2 <- as.integer(g$educ %in% c("2-year", "Some college"))
g$edu_3 <- as.integer(g$educ %in% c("4-year", "Post-grad"))

# Baseline beliefs
skeptic_id <- subset(g, year == 2010 & gw_scale <= 2, select = caseid)
g$skeptic <- with(g, ifelse(caseid %in% skeptic_id$caseid, 1, 0))

undecided_id <- subset(g, year == 2010 & gw_scale %in% c(3, 4), select = caseid)
g$undecided <- with(g, ifelse(caseid %in% undecided_id$caseid, 1, 0))

belief_id <- subset(g, year == 2010 & gw_scale == 5, select = caseid)
g$believer <- with(g, ifelse(caseid %in% belief_id$caseid, 1, 0))

g$edu_2 <- as.numeric(g$edu_2)
g$edu_3 <- as.numeric(g$edu_3)
g$ideo3 <- factor(g$ideo3)
g$income5 <- factor(g$income5)

g$religimp <- factor(g$religimp, ordered = TRUE, levels = c("Not at all important", "Not too important", "Somewhat important", "Very important"))

saveRDS(g, here("Data", "inter", "ces", "ces10-14.rds"))
message("Processed CES data and saved to Data/inter/ces/ces10-14.rds")
