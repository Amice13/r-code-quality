
################################################################################
#                                                                              #
#                       Hidden Discount Paper - FINAL                          #
#                                                                              #
################################################################################

# Load libraries 

library(here); library(dplyr); library(rio); library(lubridate); 
library(fastDummies); library(ggplot2); library(haven);
library(tidyr); library(flextable); library(readxl); library(stringr)

# Load data (April, 2024 version of MSCCSP data)
# Data spans from 1/1/1999 - 6/30/2023

data <- import(here("Data & Syntax", "MD_sentencing_guidelines_data.csv"))

# Recoding blank values to NA

data[data == ""] <- NA

################################################################################
#                                     Data Cleaning                            #
################################################################################

##################################
# Demographic and Time Variables #
##################################

# Note that for race, different indicators were added in 2019
# For Hispanic defendants, need to use ethnicity indicator. 
# However, ethnicity indicator isn't always mutually exclusive from race
# Here, including any Hispanic defendant as Hispanic, regardless of race
# To do so, need to code Hispanic first so that these defendants are not 
# overwritten by their racial identification. For age, the two digit year format 
# causes some problems. Some defendants were sentenced in 1999 and born in the 
# 1920s, so have to force any DOBs greater than the system date or the sentence 
# date to be interpreted as 1900s. Ultimately, there are still 23 events
# where the DOB and sentence appear to occur in the same year or one year apart. 
# That is, unless they are actually 100 years apart, but this is unlikely. 
# Thus, setting age 0 or 1 equal to NA

data <- data %>%
  mutate(missing_Race = apply(data[,6:14], 1, function(x) all(x == 9)),
         Race_r = case_when(Race == 3 | Ethnicity == 1 ~ "Hispanic",
                            Race == 1 | Black == 1 ~ "Black",
                            Race == 2 | White == 1 ~ "White",
                            Race == 4 | Asian == 1 ~ "Asian",
                            Race == 5 | Pacific == 1 ~ "Pacific_Isle",
                            Race == 6 | Native_Amer == 1 ~ "Native_Am",
                            Race == 8 | Unknown_Race == 1 ~ as.character(NA), 
                            missing_Race == "TRUE" ~ as.character(NA),
                            TRUE ~ "Other"),
         Race_rc = case_when(Race_r %in% c("Asian", "Pacific_Isle", "Native_Am") ~ "Other",
                             TRUE ~ Race_r),
         RaceEth_r = case_when((Race == 1 | Black == 1) & Ethnicity != 1 ~ "Black_NH",
                               (Race == 1 | Black == 1) & Ethnicity == 1 ~ "Black_H",
                               (Race == 2 | White == 1) & Ethnicity != 1 ~ "White_NH",
                               (Race == 2 | White == 1) & Ethnicity == 1 ~ "White_H",
                               (Race == 4 | Asian == 1) & Ethnicity != 1 ~ "Asian_NH",
                               (Race == 4 | Asian == 1) & Ethnicity == 1 ~ "Asian_H",
                               (Race == 5 | Pacific == 1) & Ethnicity != 1 ~ "Pacific_NH",
                               (Race == 5 | Pacific == 1) & Ethnicity == 1 ~ "Pacific_H",
                               (Race == 6 | Native_Amer == 1) & Ethnicity != 1 ~ "Native_NH",
                               (Race == 6 | Native_Amer == 1) & Ethnicity == 1 ~ "Native_H",
                               (Race == 7 | Other_Race == 1) & Ethnicity != 1 ~ "Other_NH",
                               (Race == 7 | Other_Race == 1) & Ethnicity == 1 ~ "Other_H",
                               Race == 8 | Unknown_Race == 1 ~ as.character(NA), 
                               missing_Race == "TRUE" ~ as.character(NA),
                               TRUE ~ "Other_NH"),
         RaceEth_rc = case_when(RaceEth_r %in% c("Asian_H", "Native_H", "Pacific_H") ~ "Other_H",
                                RaceEth_r %in% c("Asian_NH", "Native_NH", "Pacific_NH") ~ "Other_NH",
                                TRUE ~ RaceEth_r),
         Sex_r = case_when(Sex == 1 ~ "Male",
                           Sex == 2 ~ "Female")) %>%
  mutate(across(c(DOB, Sentence_Date),
                .fns = ~ as.Date(., format = "%m/%d/%y"),
                .names = "{col}_r"),
         across(c(DOB_r, Sentence_Date_r),
                .fns = ~ ifelse(. > Sys.Date() | . > Sentence_Date_r,
                                format(., "19%y-%m-%d"),
                                format(., "%Y-%m-%d"))),
         Sentence_Year_r = str_sub(Sentence_Date_r, start = 1, end = 4),
         Sentence_Month_r = str_sub(Sentence_Date_r, start = 6, end = 7),
         Age_r = trunc((DOB_r %--% Sentence_Date_r)/years(1)),
         Age_r = replace(Age_r, Age_r %in% c(0, 1), NA))

#########################
# Event-Level Variables #
#########################

# Note, for victim fields, treating missing values as 0 
# This is so sample size isn't lost and because most of these are likely
# cases where there was no victim anyway. Note also that there are a few
# hundred events with multiple values for "relationship". Here, I am just
# coding 1 if any offense/case within an event indicated a prior relationship
# with the CJS.

data <- data %>%
  mutate(Disposition_r = case_when(Disposition == 1 ~ "ABA Plea",
                                   Disposition == 2 ~ "Non-ABA Plea",
                                   Disposition == 3 ~ "Blind Plea",
                                   Disposition == 4 ~ "Bench Trial",
                                   Disposition == 5 ~ "Jury Trial",
                                   Disposition == 8 ~ "Revocation"),
         Disposition_rc = case_when(Disposition %in% c(4, 5) ~ "Trial",
                                    TRUE ~ Disposition_r),
         Representation_r = case_when(Representation == 1 ~ "Private",
                                      Representation == 2 ~ "Public",
                                      Representation == 3 ~ "Court Appointed",
                                      Representation == 4 ~ "Self"),
         Representation_rc = case_when(Representation == 1 ~ "Private",
                                       Representation == 2 ~ "Public",
                                       TRUE ~ "Other"),
         Prior_r = case_when(Prior == 0 ~ "None",
                             Prior == 1 ~ "Minor",
                             Prior == 3 ~ "Moderate",
                             Prior == 5 ~ "Major"),
         Relationship_r = case_when(Relationship == 0 ~ "None",
                                    Relationship == 1 ~ "Supervision"),
         Violations_r = case_when(Violations == 0 ~ 0,
                                  Violations == 1 ~ 1),
         Victim_r = case_when(Victim == 1 ~ 1,
                              TRUE ~ 0),
         Written_r = case_when(Written == 1 ~ 1,
                               TRUE ~ 0),
         Oral_r = case_when(Oral == 1 ~ 1,
                            TRUE ~ 0),
         Impact_r = case_when(Written == 1 | Oral == 1 ~ 1,
                              TRUE ~ 0)) %>%
  group_by(GLSID) %>%
  mutate(Relationship_r = ifelse(any(Relationship_r == "Supervision"), 1, 0)) %>%
  ungroup()

###########################
# Offense-Level Variables #
###########################

# Now creating variables based on offense-level information
# For each variable, first making offense-level indicators if needed, and then 
# case-level indicators grouped by GLSID:
# 1. First, creating non-mutually exclusive offense type dummies
# 2. Then, creating indicator of most serious offense category (1 is most severe)
# 3. Next creating total number of convictions
# 4. Making indicators for whether there was any victim injury or vulnerability, 
#    and also making indicator for whether the defendant used a weapon. 
#    Note that these fields only exist for person offenses, so coding missing 
#    values to 0 on these
# 6. Making indicator for whether case has a non-suspendable mandatory minimum
# 7. Making indicator for number of cases involved in sentencing event
# 8. Finally, ensuring that any cases with victim injury or vulnerability are coded as
#    1 for the victim indicator

data <- data %>%
  mutate(Chrg_Severity = case_when(Seriousness == "I" ~ 1,
                                   Seriousness == "II" ~ 2,
                                   grepl("III", Seriousness) ~ 3,
                                   Seriousness == "IV" ~ 4,
                                   Seriousness == "V" ~ 5,
                                   Seriousness == "VI" ~ 6,
                                   Seriousness == "VII" ~ 7)) %>%
  group_by(GLSID) %>%
  mutate(Person_r = ifelse(sum(Offense_Type == "Person", na.rm = TRUE) > 0, 1, 0),
         Drug_r = ifelse(sum(Offense_Type == "Drug", na.rm = TRUE) > 0, 1, 0),
         Property_r = ifelse(sum(Offense_Type == "Property", na.rm = TRUE) > 0, 1, 0),
         Other_r = ifelse(sum(Offense_Type == "Other", na.rm = TRUE) > 0, 1, 0),
         missing_off = ifelse(all(is.na(Offense_Type)), 1, 0)) %>%
  mutate(MaxSeverity_r = ifelse(all(is.na(Chrg_Severity)), NA,
                                min(Chrg_Severity, na.rm = TRUE)),
         MaxSeverity_r = as.character(as.roman(MaxSeverity_r)),
         N_Conv_r = n(),
         Injury_r = ifelse(sum(Injury, na.rm = TRUE) > 0, 1, 0),
         Weapon_r = ifelse(sum(Weapon, na.rm = TRUE) > 0, 1, 0),
         Vulnerability_r = ifelse(sum(Vulnerability, na.rm = TRUE) > 0, 1, 0),
         Minimum_r = ifelse(all(is.na(Minimum)), NA,
                            sum(Minimum == 1, na.rm = TRUE)),
         Minimum_r = replace(Minimum_r, Minimum_r > 1, 1),
         MultiCase_r = n_distinct(Docket, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate_at(vars(Person_r:Other_r), .funs = ~ ifelse(missing_off == 1, NA, .)) %>%
  mutate(Victim_r = replace(Victim_r, Injury_r == 1 | Vulnerability_r == 1, 1))

#################################
# Suspended Sentence Proportion #
#################################

# There are some cases where "Total" is missing but the individual sentence 
# length components are not. Thus, creating an alternate total sentence based
# on the sum of the individual sentencing components. First, have to convert
# credit for time served to days.

data$Credit_r <- round(data$Credit/30.4167, 2)
data$Alt_Sent <- apply(data[, c(37:39, 214)], 1, function(x) sum(x, na.rm = FALSE))

# 1. Recoding total sentence to equal alternate sentence anytime total sentence 
#    is less than alternate sentence. Also making a second total sentence that
#.   only considers the sum of Jail_Prison and Suspended. This second version
#.   is our main total sentence specification. The first is a sensitivity analysis.
# 2. Coding any remaining 0s as NA. 
# 3. Creating alternate version of active sentence which is the sum of 
#    jail_incarceration, credit, and home detention. Only adding rows where 
#    active sentence, credit, and home detention are non-missing. 
# 4. Creating two versions of the suspended sentence proportion:
#    The first is based on the recoded active and total sentences. The second
#    is based only on the standard Jail_Prison and Suspended components, along
#    with their sum. 
# 5. Finally, omitting life sentences (jail_prison of >= 720) because there is 
#    little to no discretionary decision regarding suspended time there

data <- data %>%
  mutate(Total_r = ifelse(!is.na(Alt_Sent) & Total < Alt_Sent, Alt_Sent, Total),
         Total_r = replace(Total_r, Total_r == 0, NA),
         Total_r2 = Jail_Prison + Suspended,
         Total_r2 = replace(Total_r2, Total_r2 == 0, NA)) %>%
  rowwise() %>%
  mutate(Active_r = ifelse(is.na(Jail_Prison) & is.na(Credit_r) & is.na(Home), NA, 
                           sum(c(Jail_Prison, Credit_r, Home), na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(SSP_r = (1 - (Active_r/Total_r)),
         SSP_r2 = Suspended/Total_r2) %>%
  mutate_at(vars(SSP_r:SSP_r2), ~ replace(., Jail_Prison >= 720, NA))

################################################################################
#                          Collapsing Data to Case-Level                       #
################################################################################

# Slicing one observation per GLSID. Note that all variables used/selected
# do not vary with GLSID at this point. Then, filtering out revocations, 
# sentence modifications, and cases missing either the DV or one of our focal 
# IVs (e.g., discount, race, gender, age, disposition)
# Then, limiting the df to event-level variables of interest
# Finally, sorting variables so that date and numeric fields come before factors

GLSID_df <- data %>%
  group_by(GLSID) %>%
  slice_head() %>%
  ungroup() %>%
  filter(Disposition != 8 & Event_Type != 2 & !is.na(SSP_r2) & !is.na(Race_rc) &
           !is.na(Sex_r) & !is.na(Age_r) & !is.na(Disposition_rc)) %>%
  select(GLSID, Jurisdiction, Low, Race_rc, RaceEth_rc:Sex_r, Sentence_Year_r, 
         Age_r, Disposition_rc:Impact_r, Person_r:Property_r, 
         MaxSeverity_r:MultiCase_r, Total_r:SSP_r2)

########################
# Formatting variables #
########################

# Structuring factors

GLSID_df[, c(2,4:7,9:22,24:28)] <- lapply(
  GLSID_df[, c(2,4:7,9:22,24:28)], as.factor
  )

# Releveling factors

GLSID_df <- GLSID_df %>%
  mutate(Jurisdiction = relevel(Jurisdiction, ref = 24),
         Race_rc = relevel(Race_rc, ref = "White"),
         RaceEth_rc = relevel(RaceEth_rc, ref = "White_NH"),
         Sex_r = relevel(Sex_r, ref = "Male")) %>%
  mutate(Representation_rc = factor(Representation_rc, levels = c("Public", "Private", "Other")),
         MaxSeverity_r = factor(MaxSeverity_r, levels = c("VII", "VI", "V", "IV", "III", "II", "I")),
         Disposition_rc = factor(Disposition_rc, levels = c("ABA Plea", "Non-ABA Plea", "Blind Plea", "Trial")),
         Prior_r = factor(Prior_r, levels = c("None", "Minor", "Moderate", "Major")))

# Finally, omitting year 1999 because there are 
# singlar non-zero values for some covariates
# Then changing reference level to year with largest N to reduce multi-collinearity

GLSID_df <- GLSID_df %>%
  filter(Sentence_Year_r != "1999") %>%
  mutate(Sentence_Year_r = factor(relevel(Sentence_Year_r, ref = "2002")))

# Save clean .dta file

write_dta(GLSID_df, here("Data & Syntax", "Guideline_Data_CLEAN_FINAL.dta"))

# RUN STATA ANALSIS SYNTAX BEFORE PROCEEDING!!!!!!!!!!!!!!

################################################################################
#                           Main Tables and Figures                            #
################################################################################

################
# Descriptives #
################

Descriptives <- dummy_cols(GLSID_df, 
                           select_columns = c("Race_rc", "Sex_r", "Prior_r", 
                                              "Relationship_r", "Violations_r", 
                                              "Disposition_rc", "Person_r", "Property_r", 
                                              "Drug_r", "MaxSeverity_r", "Weapon_r", 
                                              "Minimum_r", "Representation_rc", 
                                              "Victim_r", "Injury_r", 
                                              "Impact_r", "Vulnerability_r"), 
                           remove_first_dummy = TRUE, 
                           remove_selected_columns = FALSE,
                           ignore_na = TRUE)

Descriptives <- Descriptives %>%
  select(SSP_r2, Race_rc_Black:Sex_r_Female, 
         Age_r, `Disposition_rc_Non-ABA Plea`:Disposition_rc_Trial, 
         Prior_r_Minor:Prior_r_Major, Relationship_r_1:Violations_r_1, 
         MaxSeverity_r_VI:MaxSeverity_r_I, Low, Person_r_1:Drug_r_1, 
         N_Conv_r, Weapon_r_1, Minimum_r_1, Representation_rc_Private,
         Representation_rc_Other, Victim_r_1:Vulnerability_r_1) %>%
  na.omit()

#############
# Histogram #
#############

ggplot(Descriptives, aes(x = SSP_r2)) +
  geom_histogram(aes(y = after_stat(count/nrow(Descriptives))), 
                 colour="black", fill="gray84", bins = 15, binwidth = 0.05) +
  geom_density(aes(y = after_stat(count*0.0000004)), alpha = 0.1, fill = "white") +
  labs(x = "Suspended Sentence Proportion", y = "Proportion of Events") +
  theme_light()

######################
# Descriptives table #
######################

means <- sapply(Descriptives, function(x) round(mean(x), 2))
SDs <- sapply(Descriptives, function(x) round(sd(x), 2))
vars <- c("Proportion suspended", "Black", "Hispanic", "Other",
          "Female", "Age", "Non-binding plea agreement", "Plea, no agreement",
          "Bench or jury trial", "Minor", "Moderate", "Major",
          "CJS supervision", "Prior probation violations", "Category VI",
          "Category V", "Category IV", "Category III", "Category II",
          "Category I", "Lower guideline recommendation", "Any person offense",
          "Any property offense", "Any drug offense", "Number of convictions",
          "Weapon used", "Mandatory minimum", "Private attorney",
          "Other counsel", "Victim involved", "Victim injury", 
          "Victim impact statement", "Victim vulnerability")

tbl1 <- data.frame(vars, means, SDs) %>%
  add_row(vars = "Race (ref = White)", .before = 2) %>%
  add_row(vars = "Mode of conviction (ref = Binding plea)", .before = 8) %>%
  add_row(vars = "Prior record (ref = None)", .before = 12) %>%
  add_row(vars = "Maximum charge severity (ref = Category VII)", .before = 18) %>%
  add_row(vars = "Defense counsel type (ref = Public defender)", .before = 32) %>%
  mutate(SDs = case_when(!vars %in% c("Proportion suspended", "Age",
                                      "Lower guideline recommendation",
                                      "Number of convictions") ~ NA,
                         TRUE ~ SDs))

flextbl1 <- flextable(tbl1) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Descriptive statistics for MSCCSP Data, 2020-2023 (N = 175,620)", colwidths = 3) %>%
  set_header_labels(vars = "Variables", means = "Mean/Proportion", SDs = "SD") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  align(i = c(3:5, 9:11, 13:15, 19:24, 33:34), j = 1, align = "right") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextbl1

# save_as_docx(flextbl1, path = "Table1.docx")

####################
# Regression Table #
####################

regres <- read_excel(here("Data & Syntax", "Results_FINAL.xlsx"), sheet = 1)
zeroinf <- read_excel(here("Data & Syntax", "Results_FINAL.xlsx"), sheet = 2)
prop <- read_excel(here("Data & Syntax", "Results_FINAL.xlsx"), sheet = 3)
oneinf <- read_excel(here("Data & Syntax", "Results_FINAL.xlsx"), sheet = 4)

MEMs <- data.frame(var = zeroinf$...1, zero_MEM = zeroinf$b,
                   prop_MEM = prop$b, one_MEM = oneinf$b) %>%
  mutate_at(vars(zero_MEM:one_MEM), ~ replace(., grepl("1b", var), NA))

# Clean main regression results and merge AMEs
# Then exponentiating coefficients, creating sig stars, and pasting them to coefficients
# Then ordering necessary variables and dropping unnecessary variables 

regres <- regres %>%
  select(mod = ...1, var = ...2, b, rob_SE = se, p = pvalue) %>%
  mutate(b = replace(b, grepl("1b", var), NA)) %>%
  filter(!grepl("Sentence_Year|Jurisdiction", var) & mod != "ln_phi") %>%
  pivot_wider(names_from = mod, values_from = b:p) %>%
  left_join(MEMs, by = "var") %>%
  mutate(across(c(b_zeroinflate, b_proportion, b_oneinflate), ~ round(exp(.), 3)),
         across(c(rob_SE_zeroinflate, rob_SE_proportion, rob_SE_oneinflate,
                  zero_MEM, prop_MEM, one_MEM), ~ round(., 3)),
         sig_zero = case_when(p_zeroinflate < 0.10 & p_zeroinflate > 0.05 ~ "+",
                              p_zeroinflate < 0.05 & p_zeroinflate > 0.01 ~ "*",
                              p_zeroinflate < 0.01 ~ "**"),
         sig_prop = case_when(p_proportion < 0.10 & p_proportion > 0.05 ~ "+",
                              p_proportion < 0.05 & p_proportion > 0.01 ~ "*",
                              p_proportion < 0.01 ~ "**"),
         sig_one = case_when(p_oneinflate < 0.10 & p_oneinflate > 0.05 ~ "+",
                             p_oneinflate < 0.05 & p_oneinflate > 0.01 ~ "*",
                             p_oneinflate < 0.01 ~ "**"),
         bsig_zero = ifelse(!is.na(sig_zero), paste(b_zeroinflate, sig_zero, sep = ""),
                            b_zeroinflate),
         bsig_prop = ifelse(!is.na(sig_prop), paste(b_proportion, sig_prop, sep = ""), 
                            b_proportion),
         bsig_one = ifelse(!is.na(sig_one), paste(b_oneinflate, sig_one, sep = ""), 
                           b_oneinflate)) %>%
  relocate(c(bsig_zero, b_zeroinflate, sig_zero, rob_SE_zeroinflate, p_zeroinflate, zero_MEM), 
           .after = var) %>%
  relocate(c(bsig_prop, b_proportion, sig_prop, rob_SE_proportion, p_proportion, prop_MEM), 
           .after = zero_MEM) %>%
  relocate(c(bsig_one, b_oneinflate, sig_one, rob_SE_oneinflate, p_oneinflate, one_MEM), 
           .after = prop_MEM) %>%
  select(-c(b_zeroinflate, sig_zero, p_zeroinflate,
            b_proportion, sig_prop, p_proportion, 
            b_oneinflate, sig_one, p_oneinflate))

# Move intercept to first row and remove reference row for binary variables

regres <- rbind(regres[51, ], regres[-51, ])
regres <- regres[-c(6,18,20,30,32,34,37,39,44,46,48,50), ]

# Renaming variables

regres$var <- c("Intercept", "Race (ref = White)", "Black", "Hispanic", "Other", 
                "Female", "Age", "Age^2", "Mode of conviction (ref = Binding plea)", 
                "Non-binding plea", "Plea, no agreement", "Bench or jury trial", 
                "Prior record (ref = None)", "Minor", "Moderate", "Major", 
                "CJS supervision", "Probation violation", "Max charge severity (ref = Category VII)",
                "Category VI", "Category V", "Category IV", "Category III",
                "Category II", "Category I",  "Lower guideline recommendation", 
                "Person offense", "Property offense", "Drug offense", 
                "Number of convictions", "Weapon used", "Mandatory minimum", 
                "Defense type (ref = Public)", "Private", "Other", "Victim", 
                "Victim injury", "Victim impact statement", "Victim vulnerability")

# Now making flextable 

flextbl2 <- flextable(regres) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Zero-one inflated beta regression results", colwidths = 10) %>%
  set_header_labels(var = "", bsig_zero = "B", rob_SE_zeroinflate = "Robust SE", 
                    zero_MEM = "MEM", bsig_prop = "B", rob_SE_proportion = "Robust SE",
                    prop_MEM = "MEM", bsig_one = "B", rob_SE_oneinflate = "Robust SE",
                    one_MEM = "MEM") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  align(i = c(3:5, 10:12, 14:16, 20:25, 34:35), j = 1, align = "right") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextbl2

# save_as_docx(flextbl2, path = "Table2.docx")

###########################
# Race by Sex Predictions #
###########################

preds1 <- read_excel(here("Data & Syntax", "Predictions_FINAL.xlsx"), sheet = 1)
preds1 <- preds1[1:7,] %>% select(-c(...5:...7))
preds1t <- data.frame(t(preds1[,-1]))
colnames(preds1t) <- preds1$...1
preds1 <- preds1t; rm(preds1t)
preds1 <- preds1 %>%
  mutate(Sex = 0) %>%
  select(Race = Race_rc, Sex, Margin, ll, ul)

preds1.1 <- read_excel(here("Data & Syntax", "Predictions_FINAL.xlsx"), sheet = 1)
preds1.1 <- preds1.1[8:16,]
preds1.1t <- data.frame(t(preds1.1[,-1]))
colnames(preds1.1t) <- preds1.1$...1
preds1.1 <- preds1.1t; rm(preds1.1t)
preds1.1 <- preds1.1 %>%
  select(Race = Race_rc, Sex = Sex_r, Margin, ll, ul)

preds1 <- rbind(preds1, preds1.1) %>%
  mutate(Race = as.factor(Race),
         Race = recode_factor(Race, '1' = "White", '2' = "Black", '3' = "Hispanic"),
         Sex = as.factor(Sex),
         Sex = recode_factor(Sex, '0' = "All Defendants", '1' = "Male", '2' = "Female")) %>%
  mutate_at(vars(Margin:ul), .funs = ~ round(as.numeric(.), 3)) 
rm(preds1.1) 

ggplot(preds1, aes(x = Race, y = Margin)) +
  geom_bar(stat = "identity", color = "black", 
           fill = rep(c("gray44", "gray80", "gray100"), 3), 
           width = 0.7) +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0.08, color = "black") +
  scale_y_continuous(limits = c(0.0, 1.0)) +
  facet_wrap(~ Sex, ncol = 3) +
  labs(title = "", x = "", y = "Suspended Sentence Proportion") +
  theme_light()

#######################
# Race by Sex and Age #
#######################

preds2 <- read_excel(here("Data & Syntax", "Predictions_FINAL.xlsx"), sheet = 2) %>%
  filter(!grepl("_at", ...1)) 

preds2t <- data.frame(t(preds2[,-1]))
colnames(preds2t) <- preds2$...1
preds2 <- preds2t; rm(preds2t)

preds2 <- preds2 %>%
  rename(Race = Race_rc, Sex = Sex_r, Age = Age_r) %>%
  mutate(Race = as.factor(Race),
         Race = recode_factor(Race, '1' = "White", '2' = "Black", '3' = "Hispanic"),
         Sex = as.factor(Sex),
         Sex = recode_factor(Sex, '1' = "Male", '2' = "Female"),
         Age = as.factor(Age)) %>%
  select(Age:Margin, ll:ul)

ggplot(preds2, aes(x = Age, y = Margin, shape = Race, group = Race, fill = Race)) +
  geom_line(color = "black") +
  geom_point(size = 2.5) +
  facet_wrap(~ Sex, ncol = 2) +
  scale_shape_manual(values = c(21, 22, 24)) +
  scale_fill_manual(values = c(rep("white", 3))) +
  geom_ribbon(aes(ymin = ll, ymax = ul, color = "gray64"), 
              fill = c(rep("gray84", 30)), 
              alpha = 0.2, linewidth = 0.1,
              show.legend = FALSE) +
  scale_y_continuous(limits = c(0.0, 1.0)) +
  labs(title = "", y = "Suspended Sentence Proportion") +
  theme_light()

##############################
# Race by Disposition Method #
##############################

preds3 <- read_excel(here("Data & Syntax", "Predictions_FINAL.xlsx"), sheet = 3)
preds3 <- preds3[1:8,] %>% select(-c(...14:...25))
preds3t <- data.frame(t(preds3[,-1]))
colnames(preds3t) <- preds3$...1
preds3 <- preds3t; rm(preds3t)
preds3 <- preds3 %>%
  mutate(Sex = 0) %>%
  select(Race = Race_rc, Sex, 
         Disposition = Disposition_rc, 
         Margin, ll, ul)

preds3.1 <- read_excel(here("Data & Syntax", "Predictions_FINAL.xlsx"), sheet = 3)
preds3.1 <- preds3.1[9:18,]
preds3.1t <- data.frame(t(preds3.1[,-1]))
colnames(preds3.1t) <- preds3.1$...1
preds3.1 <- preds3.1t; rm(preds3.1t)
preds3.1 <- preds3.1 %>%
  select(Race = Race_rc, Sex = Sex_r, 
         Disposition = Disposition_rc, 
         Margin, ll, ul)

preds3 <- rbind(preds3, preds3.1) %>%
  mutate(Race = as.factor(Race),
         Race = recode_factor(Race, '1' = "White", '2' = "Black", '3' = "Hispanic"),
         Sex = as.factor(Sex),
         Sex = recode_factor(Sex, '0' = "All Defendants", '1' = "Male", '2' = "Female"),
         Disposition = as.factor(Disposition),
         Disposition = recode_factor(Disposition, '1' = "Binding Plea",
                                     '2' = "Non-Binding Plea",
                                     '3' = "Plea, No Agreement",
                                     '4' = "Trial")) %>%
  mutate_at(vars(Margin:ul), .funs = ~ round(as.numeric(.), 3)) 
rm(preds3.1) 

ggplot(preds3, aes(x = Disposition, y = Margin, fill = Race)) +
  geom_bar(position = position_dodge(), color = "black", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0.2, position = position_dodge(0.7), color = "black") +
  scale_fill_manual(values = c("gray44", "gray80", "gray100")) +
  scale_y_continuous(limits = c(0.0, 1.0)) +
  facet_wrap(~ Sex, ncol = 3) +
  labs(title = "", x = "", y = "Suspended Sentence Proportion") +
  theme_light()

####################################
# Total vs. active sentence length #
####################################

preds4 <- read_excel(here("Data & Syntax", "Predictions_FINAL.xlsx"), sheet = 4)
preds4 <- preds4[c(1:3, 6:8),] %>% select(...1, b, ll, ul) %>%
  mutate(Race = rep(c("White", "Black", "Hispanic"), 2),
         Type = c(rep("Total Sentence", 3), rep("Active Sentence", 3))) %>%
  mutate(Race = factor(Race, levels = c("White", "Black", "Hispanic")),
         Type = factor(Type, levels = c("Total Sentence", "Active Sentence"))) %>%
  mutate_at(vars(b:ul), .funs = ~ as.numeric(.)) %>%
  select(-...1) %>%
  relocate(Race:Type, .before = b)

ggplot(preds4, aes(x = Type, y = b, fill = Race)) +
  geom_bar(position = position_dodge(), color = "black", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = ll, ymax = ul), width = 0.08, color = "black",
                position = position_dodge(0.7)) +
  scale_fill_manual(values = c("gray44", "gray80", "gray100")) +
  labs(title = "", x = "", y = "Months of Incarceration") +
  theme_light()

################################
# Race by Sentence Year w/ CIs #
################################

preds5 <- import_list(here("Data & Syntax", "Years_w_CI_FINAL.xlsx"), rbind = TRUE) %>%
  pivot_wider(id_cols = "_file", names_from = ...1, values_from = Black:Other) %>%
  select(file = "_file", Black_m = Black_d_Margin, Black_ll, Black_ul,
         Hispanic_m = Hispanic_d_Margin, Hispanic_ll, Hispanic_ul) %>%
  mutate(year = c(2002, 2000, 2001, seq(2003, 2023)))

preds5 <- reshape(as.data.frame(preds5), direction = "long", idvar = "year", 
                  timevar = "race", times = c("Black", "Hispanic"),
                  varying = list(c("Black_m", "Hispanic_m"),
                                 c("Black_ll", "Hispanic_ll"),
                                 c("Black_ul", "Hispanic_ul")),
                  v.names = c("effect", "ll", "ul"))

ggplot(preds5, aes(x = year, y = effect, shape = race, fill = race)) +
  geom_hline(yintercept = 0, lty = "dashed") +
  geom_line() +
  geom_point(size = 2.5) +
  geom_ribbon(aes(ymin = ll, ymax = ul), 
              fill = c(rep("gray84", 48)),
              alpha = 0.1, linewidth = 0.1,
              color = "gray64", show.legend = FALSE) +
  scale_y_continuous(limits = c(-0.4, 0.4)) +
  scale_shape_manual(values = c(22, 24)) +
  scale_fill_manual(values = c(rep("white", 2))) +
  labs(title = "", x = "Year", y = "Marginal Effect at the Mean", shape = "Race", fill = "Race") +
  theme_light()

################################################################################
#                             Supplemental Analyses                            #
################################################################################

#################################
# Race by Sex Second Difference #
#################################

supp1 <- read_excel(here("Data & Syntax", "Second_Diffs_FINAL.xlsx"), sheet = 1) %>%
  rename(Contrast = ...1) %>%
  mutate_at(vars(est:p), .funs = ~ round(., 3)) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 1) %>%
  mutate(Contrast = c("Gender difference by race",
                      "Black vs.White", "Hispanic vs. White",
                      "Black vs. Hispanic"))

flextblS1 <- flextable(supp1) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Table S1. Race by Gender (Second Differences)", 
                 colwidths = 5) %>%
  set_header_labels(Contrast = "", est = "Est.", se = "SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextblS1

# save_as_docx(flextblS1, path = "TableS1.docx")

########################################
# Race by Sex and Age Third Difference #
########################################

supp2 <- read_excel(here("Data & Syntax", "Second_Diffs_FINAL.xlsx"), sheet = 2) %>%
  rename(Contrast = ...1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 5) %>%
  mutate_at(vars(est:p), .funs = ~ round(., 3)) %>%
  mutate(Contrast = c("Age difference (18-30) by race and gender",
                      "(Black male vs female) - (White male vs. female)",
                      "(Hispanic male vs. female) - (White male vs. female)",
                      "(Black male vs. female) - (Hispanic male vs. female)",
                      "Age difference (30-50) by race and gender",
                      "(Black male vs female) - (White male vs. female)",
                      "(Hispanic male vs. female) - (White male vs. female)",
                      "(Black male vs. female) - (Hispanic male vs. female)"))

flextblS2 <- flextable(supp2) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Table S2. Race by Gender and Age (Third Differences)", 
                 colwidths = 5) %>%
  set_header_labels(Contrast = "", est = "Est.", se = "SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  surround(i = 5, border.top = fp_border_default()) %>%
  autofit()

flextblS2

# save_as_docx(flextblS2, path = "TableS2.docx")

################################################
# Race by Mode of Conviction Second Difference #
################################################

supp3 <- read_excel(here("Data & Syntax", "Second_Diffs_FINAL.xlsx"), sheet = 3) %>%
  rename(Contrast = ...1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 5) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 9) %>%
  mutate_at(vars(est:p), .funs = ~ round(., 3)) %>%
  mutate(Contrast = c("Race difference by mode of conviction (non-binding plea vs. binding plea)",
                      "Black vs. White",
                      "Hispanic vs. White",
                      "Black vs. Hispanic",
                      "Race difference by mode of conviction (plea, no agreement vs. binding plea)",
                      "Black vs. White",
                      "Hispanic vs. White",
                      "Black vs. Hispanic",
                      "Race difference by mode of conviction (trial vs. binding plea)",
                      "Black vs. White",
                      "Hispanic vs. White",
                      "Black vs. Hispanic"))

flextblS3 <- flextable(supp3) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Table S3. Race by Mode of Conviction (Second Differences)", 
                 colwidths = 5) %>%
  set_header_labels(Contrast = "", est = "Est.", se = "SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  surround(i = c(1, 5, 9), border.top = fp_border_default()) %>%
  autofit()

flextblS3

# save_as_docx(flextblS3, path = "TableS3.docx")

########################################################
# Race by Sex and Mode of Conviction Third Differences #
########################################################

supp4 <- read_excel(here("Data & Syntax", "Second_Diffs_FINAL.xlsx"), sheet = 4) %>%
  rename(Contrast = ...1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 5) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 9) %>%
  mutate_at(vars(est:p), .funs = ~ round(., 3)) %>%
  mutate(Contrast = c("Gender difference by race and mode of conviction (non-binding vs. binding plea)",
                      "(Black non-binding vs. binding) - (White non-binding vs. binding)",
                      "(Hispanic non-binding vs. binding) - (White non-binding vs. binding)",
                      "(Black non-binding vs. binding) - (Hispanic non-binding vs. binding)",
                      "Gender difference by race and mode of conviction (plea, no agreement vs. binding plea)",
                      "(Black no agreement vs. binding) - (White no agreement vs. binding)",
                      "(Hispanic no agreement vs. binding) - (White no agreement vs. binding)",
                      "(Black no agreement vs. binding) - (Hispanic no agreement vs. binding)",
                      "Gender difference by race and mode of conviction (trial vs. binding plea)",
                      "(Black trial vs. binding) - (White trial vs. binding)",
                      "(Hispanic trial vs. binding) - (White trial vs. binding)",
                      "(Black trial vs. binding) - (Hispanic trial vs. binding)"))

flextblS4 <- flextable(supp4) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Table S4. Race by Gender and Mode of Conviction (Third Differences)", 
                 colwidths = 5) %>%
  set_header_labels(Contrast = "", est = "Est.", se = "SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  surround(i = c(1, 5, 9), border.top = fp_border_default()) %>%
  autofit()

flextblS4

# save_as_docx(flextblS4, path = "TableS4.docx")

####################################
# Race and Year Second Differences #
####################################

supp5 <- read_excel(here("Data & Syntax", "Second_Diffs_FINAL.xlsx"), sheet = 5) %>%
  rename(Contrast = ...1) %>%
  add_row(Contrast = NA, se = NA, z = NA, p = NA, .before = 1) %>%
  mutate_at(vars(est:p), .funs = ~ round(., 3)) %>%
  mutate(Contrast = c("Race difference by year (year 2023 vs. 2000)",
                      "Black vs. White",
                      "Hispanic vs. White",
                      "Black vs. Hispanic"))

flextblS5 <- flextable(supp5) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Table S5. Race by Year (Second Differences)", 
                 colwidths = 5) %>%
  set_header_labels(Contrast = "", est = "Est.", se = "SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  surround(i = 1, border.top = fp_border_default()) %>%
  autofit()

flextblS5

# save_as_docx(flextblS5, path = "TableS5.docx")

###########################
# Model for log TS length #
###########################

supp6 <- read_excel(here("Data & Syntax", "Sensitivity_Mods_FINAL.xlsx"), sheet = 1)

# Clean main regression results
# Then exponentiating coefficients, creating sig stars, and pasting them to coefficients
# Then ordering necessary variables and dropping unnecessary variables 

supp6 <- supp6 %>%
  rename(var = ...1) %>%
  filter(!grepl("Sentence_Year|Jurisdiction", var)) %>%
  mutate_at(vars(b:pvalue), ~ round(., 3)) %>%
  mutate(b = replace(b, b == 0 & var != "c.Age_0#c.Age_0", NA),
         sig = case_when(pvalue < 0.10 & pvalue > 0.05 ~ "+",
                         pvalue < 0.05 & pvalue > 0.01 ~ "*",
                         pvalue < 0.01 ~ "**"),
         bsig = ifelse(!is.na(sig), paste(b, sig, sep = ""), b)) %>%
  select(var, bsig, se) 

# Move intercept to first row and remove reference row for binary variables

supp6 <- rbind(supp6[51, ], supp6[-51, ])
supp6 <- supp6[-c(6,18,20,30,32,34,37,39,44,46,48,50), ]

# Rename variables

supp6$var <- c("Intercept", "Race (ref = White)", "Black", 
                "Hispanic", "Other", "Female", "Age", "Age^2", "Mode of conviction (ref = Binding plea)", 
                "Non-binding plea", "Plea, no agreement", "Bench or jury trial", 
                "Prior record (ref = None)", "Minor", "Moderate", "Major", 
                "CJS supervision", "Probation violation", "Max charge severity (ref = Category VII)",
                "Category VI", "Category V", "Category IV", "Category III",
                "Category II", "Category I",  "Lower guideline recommendation", 
                "Person offense", "Property offense", "Drug offense", 
                "Number of convictions", "Weapon used", "Mandatory minimum", 
                "Defense type (ref = Public)", "Private", "Other", "Victim", 
                "Victim injury", "Victim impact statement", "Victim vulnerability")

flextblS6 <- flextable(supp6) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Linear regression results for logged total sentence length (N = 175,620; R2 = 0.49)", 
                 colwidths = 3) %>%
  set_header_labels(var = "", bsig = "b", pvalue = "p") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  align(i = c(3:5, 10:12, 14:16, 20:25, 34:35), j = 1, align = "right") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextblS6

# save_as_docx(flextblS6, path = "TableS6.docx")

#########################
# Model w/ Adult Sample #
#########################

supp7.1 <- read_excel(here("Data & Syntax", "Sensitivity_Mods_FINAL.xlsx"), sheet = 2)

# Clean main regression results
# Then exponentiating coefficients, creating sig stars, and pasting them to coefficients
# Then ordering necessary variables and dropping unnecessary variables 

supp7.1 <- supp7.1 %>%
  select(mod = ...1, var = ...2, b, rob_SE = se, p = pvalue) %>%
  mutate(b = replace(b, grepl("1b", var), NA)) %>%
  filter(!grepl("Sentence_Year|Jurisdiction", var) & mod != "ln_phi") %>%
  pivot_wider(names_from = mod, values_from = b:p) %>%
  mutate(across(c(b_zeroinflate, b_proportion, b_oneinflate), ~ round(exp(.), 3)),
         across(c(rob_SE_zeroinflate, rob_SE_proportion, rob_SE_oneinflate), ~ round(., 3)),
         sig_zero = case_when(p_zeroinflate < 0.10 & p_zeroinflate > 0.05 ~ "+",
                              p_zeroinflate < 0.05 & p_zeroinflate > 0.01 ~ "*",
                              p_zeroinflate < 0.01 ~ "**"),
         sig_prop = case_when(p_proportion < 0.10 & p_proportion > 0.05 ~ "+",
                              p_proportion < 0.05 & p_proportion > 0.01 ~ "*",
                              p_proportion < 0.01 ~ "**"),
         sig_one = case_when(p_oneinflate < 0.10 & p_oneinflate > 0.05 ~ "+",
                             p_oneinflate < 0.05 & p_oneinflate > 0.01 ~ "*",
                             p_oneinflate < 0.01 ~ "**"),
         bsig_zero = ifelse(!is.na(sig_zero), paste(b_zeroinflate, sig_zero, sep = ""),
                            b_zeroinflate),
         bsig_prop = ifelse(!is.na(sig_prop), paste(b_proportion, sig_prop, sep = ""), 
                            b_proportion),
         bsig_one = ifelse(!is.na(sig_one), paste(b_oneinflate, sig_one, sep = ""), 
                           b_oneinflate)) %>%
  relocate(c(bsig_zero, b_zeroinflate, sig_zero, rob_SE_zeroinflate, p_zeroinflate), 
           .after = var) %>%
  relocate(c(bsig_prop, b_proportion, sig_prop, rob_SE_proportion, p_proportion), 
           .after = p_zeroinflate) %>%
  relocate(c(bsig_one, b_oneinflate, sig_one, rob_SE_oneinflate, p_oneinflate), 
           .after = p_proportion) %>%
  select(-c(b_zeroinflate, sig_zero, p_zeroinflate,
            b_proportion, sig_prop, p_proportion, 
            b_oneinflate, sig_one, p_oneinflate))

# Move intercept to first row and remove reference row for binary variables

supp7.1 <- rbind(supp7.1[51, ], supp7.1[-51, ])
supp7.1 <- supp7.1[-c(6,18,20,30,32,34,37,39,44,46,48,50), ]

# Renaming variables

supp7.1$var <- c("Intercept", "Race (ref = White)", "Black", "Hispanic", "Other", 
               "Female", "Age", "Age^2", "Mode of conviction (ref = Binding plea)", 
               "Non-binding plea", "Plea, no agreement", "Bench or jury trial", 
               "Prior record (ref = None)", "Minor", "Moderate", "Major", 
               "CJS supervision", "Probation violation", "Max charge severity (ref = Category VII)",
               "Category VI", "Category V", "Category IV", "Category III",
               "Category II", "Category I",  "Lower guideline recommendation", 
               "Person offense", "Property offense", "Drug offense", 
               "Number of convictions", "Weapon used", "Mandatory minimum", 
               "Defense type (ref = Public)", "Private", "Other", "Victim", 
               "Victim injury", "Victim impact statement", "Victim vulnerability")

# Now making flextable 

flextblS7.1 <- flextable(supp7.1) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Zero-one inflated beta regression results using only adults 23 and over (N = 130,331)", 
                 colwidths = 7) %>%
  set_header_labels(var = "", bsig_zero = "B", rob_SE_zeroinflate = "Robust SE", 
                    bsig_prop = "B", rob_SE_proportion = "Robust SE",
                    bsig_one = "B", rob_SE_oneinflate = "Robust SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  align(i = c(3:5, 10:12, 14:16, 20:25, 34:35), j = 1, align = "right") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextblS7.1

# save_as_docx(flextblS7.1, path = "TableS71.docx")

########################################
# Model with credit and home detention #
########################################

supp7.2 <- read_excel(here("Data & Syntax", "Sensitivity_Mods_FINAL.xlsx"), sheet = 3)

# Clean main regression results
# Then exponentiating coefficients, creating sig stars, and pasting them to coefficients
# Then ordering necessary variables and dropping unnecessary variables 

supp7.2 <- supp7.2 %>%
  select(mod = ...1, var = ...2, b, rob_SE = se, p = pvalue) %>%
  mutate(b = replace(b, grepl("1b", var), NA)) %>%
  filter(!grepl("Sentence_Year|Jurisdiction", var) & mod != "ln_phi") %>%
  pivot_wider(names_from = mod, values_from = b:p) %>%
  mutate(across(c(b_zeroinflate, b_proportion, b_oneinflate), ~ round(exp(.), 3)),
         across(c(rob_SE_zeroinflate, rob_SE_proportion, rob_SE_oneinflate), ~ round(., 3)),
         sig_zero = case_when(p_zeroinflate < 0.10 & p_zeroinflate > 0.05 ~ "+",
                              p_zeroinflate < 0.05 & p_zeroinflate > 0.01 ~ "*",
                              p_zeroinflate < 0.01 ~ "**"),
         sig_prop = case_when(p_proportion < 0.10 & p_proportion > 0.05 ~ "+",
                              p_proportion < 0.05 & p_proportion > 0.01 ~ "*",
                              p_proportion < 0.01 ~ "**"),
         sig_one = case_when(p_oneinflate < 0.10 & p_oneinflate > 0.05 ~ "+",
                             p_oneinflate < 0.05 & p_oneinflate > 0.01 ~ "*",
                             p_oneinflate < 0.01 ~ "**"),
         bsig_zero = ifelse(!is.na(sig_zero), paste(b_zeroinflate, sig_zero, sep = ""),
                            b_zeroinflate),
         bsig_prop = ifelse(!is.na(sig_prop), paste(b_proportion, sig_prop, sep = ""), 
                            b_proportion),
         bsig_one = ifelse(!is.na(sig_one), paste(b_oneinflate, sig_one, sep = ""), 
                           b_oneinflate)) %>%
  relocate(c(bsig_zero, b_zeroinflate, sig_zero, rob_SE_zeroinflate, p_zeroinflate), 
           .after = var) %>%
  relocate(c(bsig_prop, b_proportion, sig_prop, rob_SE_proportion, p_proportion), 
           .after = p_zeroinflate) %>%
  relocate(c(bsig_one, b_oneinflate, sig_one, rob_SE_oneinflate, p_oneinflate), 
           .after = p_proportion) %>%
  select(-c(b_zeroinflate, sig_zero, p_zeroinflate,
            b_proportion, sig_prop, p_proportion, 
            b_oneinflate, sig_one, p_oneinflate))

# Move intercept to first row and remove reference row for binary variables

supp7.2 <- rbind(supp7.2[51, ], supp7.2[-51, ])
supp7.2 <- supp7.2[-c(6,18,20,30,32,34,37,39,44,46,48,50), ]

# Renaming variables

supp7.2$var <- c("Intercept", "Race (ref = White)", "Black", "Hispanic", "Other", 
               "Female", "Age", "Age^2", "Mode of conviction (ref = Binding plea)", 
               "Non-binding plea", "Plea, no agreement", "Bench or jury trial", 
               "Prior record (ref = None)", "Minor", "Moderate", "Major", 
               "CJS supervision", "Probation violation", "Max charge severity (ref = Category VII)",
               "Category VI", "Category V", "Category IV", "Category III",
               "Category II", "Category I",  "Lower guideline recommendation", 
               "Person offense", "Property offense", "Drug offense", 
               "Number of convictions", "Weapon used", "Mandatory minimum", 
               "Defense type (ref = Public)", "Private", "Other", "Victim", 
               "Victim injury", "Victim impact statement", "Victim vulnerability")

# Now making flextable 

flextblS7.2 <- flextable(supp7.2) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Zero-one inflated beta regression results including credit and home detention (N = 175,620)", 
                 colwidths = 7) %>%
  set_header_labels(var = "", bsig_zero = "B", rob_SE_zeroinflate = "Robust SE", 
                    bsig_prop = "B", rob_SE_proportion = "Robust SE",
                    bsig_one = "B", rob_SE_oneinflate = "Robust SE") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  align(i = c(3:5, 10:12, 14:16, 20:25, 34:35), j = 1, align = "right") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextblS7.2

# save_as_docx(flextblS7.2, path = "TableS72.docx")

############################################
# Model with race and ethnicity separately #
############################################

supp7.3 <- read_excel(here("Data & Syntax", "Sensitivity_Mods_FINAL.xlsx"), sheet = 4)

# Clean main regression results
# Then exponentiating coefficients, creating sig stars, and pasting them to coefficients
# Then ordering necessary variables and dropping unnecessary variables 

supp7.3 <- supp7.3 %>%
  select(mod = ...1, var = ...2, b, rob_SE = se, p = pvalue) %>%
  mutate(b = replace(b, grepl("1b", var), NA)) %>%
  filter(!grepl("Sentence_Year|Jurisdiction", var) & mod != "ln_phi") %>%
  pivot_wider(names_from = mod, values_from = b:p) %>%
  mutate(across(c(b_zeroinflate, b_proportion, b_oneinflate), ~ round(exp(.), 3)),
         across(c(rob_SE_zeroinflate, rob_SE_proportion, rob_SE_oneinflate), ~ round(., 3)),
         sig_zero = case_when(p_zeroinflate < 0.10 & p_zeroinflate > 0.05 ~ "+",
                              p_zeroinflate < 0.05 & p_zeroinflate > 0.01 ~ "*",
                              p_zeroinflate < 0.01 ~ "**"),
         sig_prop = case_when(p_proportion < 0.10 & p_proportion > 0.05 ~ "+",
                              p_proportion < 0.05 & p_proportion > 0.01 ~ "*",
                              p_proportion < 0.01 ~ "**"),
         sig_one = case_when(p_oneinflate < 0.10 & p_oneinflate > 0.05 ~ "+",
                             p_oneinflate < 0.05 & p_oneinflate > 0.01 ~ "*",
                             p_oneinflate < 0.01 ~ "**"),
         bsig_zero = ifelse(!is.na(sig_zero), paste(b_zeroinflate, sig_zero, sep = ""),
                            b_zeroinflate),
         bsig_prop = ifelse(!is.na(sig_prop), paste(b_proportion, sig_prop, sep = ""), 
                            b_proportion),
         bsig_one = ifelse(!is.na(sig_one), paste(b_oneinflate, sig_one, sep = ""), 
                           b_oneinflate)) %>%
  relocate(c(bsig_zero, b_zeroinflate, sig_zero, rob_SE_zeroinflate, p_zeroinflate), 
           .after = var) %>%
  relocate(c(bsig_prop, b_proportion, sig_prop, rob_SE_proportion, p_proportion), 
           .after = p_zeroinflate) %>%
  relocate(c(bsig_one, b_oneinflate, sig_one, rob_SE_oneinflate, p_oneinflate), 
           .after = p_proportion) %>%
  select(-c(b_zeroinflate, sig_zero, p_zeroinflate,
            b_proportion, sig_prop, p_proportion, 
            b_oneinflate, sig_one, p_oneinflate))

# Move intercept to first row and remove reference row for binary variables

supp7.3 <- rbind(supp7.3[53, ], supp7.3[-53, ])
supp7.3 <- supp7.3[-c(8,20,22,32,34,36,39,41,46,48,50,52), ]

# Renaming variables

supp7.3$var <- c("Intercept", "Race/Ethnicity (ref = White non-Hispanic)", "White Hispanic", 
               "Black non-Hispanic", "Black Hispanic", "Other non-Hispanic", 
               "Other Hispanic", "Female", "Age", "Age^2", "Mode of conviction (ref = Binding plea)", 
               "Non-binding plea", "Plea, no agreement", "Bench or jury trial", 
               "Prior record (ref = None)", "Minor", "Moderate", "Major", 
               "CJS supervision", "Probation violation", "Max charge severity (ref = Category VII)",
               "Category VI", "Category V", "Category IV", "Category III",
               "Category II", "Category I",  "Lower guideline recommendation", 
               "Person offense", "Property offense", "Drug offense", 
               "Number of convictions", "Weapon used", "Mandatory minimum", 
               "Defense type (ref = Public)", "Private", "Other", "Victim", 
               "Victim injury", "Victim impact statement", "Victim vulnerability")

# Now making flextable 

flextblS7.3 <- flextable(supp7.3) %>%
  theme_booktabs(bold_header = FALSE) %>%
  add_header_row(values = "Zero-one inflated beta regression results including racial and ethnic combinations (N = 175,338)", 
                 colwidths = 7) %>%
  set_header_labels(var = "", bsig_zero = "B", rob_SE_zeroinflate = "Robust SE", 
                    bsig_prop = "B", rob_SE_proportion = "Robust SE",
                    bsig_one = "B", rob_SE_oneinflate = "Robust SE") %>%
  add_footer_row(values = "Note: Jurisdiction and year fixed effects, as well as coefficients involved in the estimation of the scaling parameter () omitted for parsimony", 
                 colwidths = 7) %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 9, part = "footer") %>%
  align(i = 1, part = "header", align = "left") %>% 
  align(j = 1, align = "left") %>%
  align(i = c(3:7, 12:14, 16:18, 22:27, 36:37), j = 1, align = "right") %>%
  hline_top(border = fp_border_default(width = 0), part = "header") %>%
  autofit()

flextblS7.3

# save_as_docx(flextblS7.3, path = "TableS73.docx")




