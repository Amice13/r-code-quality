#Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(psych)
library(readxl)
library(tidyverse)

g <- read.csv(here("Data", "input", "lucid", "EGAP Book Survey_January 20, 2024_11.11.csv"))

# Data quality checks
g <- g[-c(1,2), ]
g$Q_RecaptchaScore <- as.numeric(g$Q_RecaptchaScore)
g$term <- ifelse(g$Q_RecaptchaScore < .5, "security", g$term)
g$gc <- ifelse(g$Q_RecaptchaScore < .5, 4, g$gc)

g <- g %>% rename(dur = Duration..in.seconds.)
g$dur <- as.numeric(g$dur) / 60

# Socio-Demographic covariates
g <- g %>%
  rename(
    # Embedded covariates
    gender_lucid = gender,
    age_lucid = age,
    hhi_lucid = hhi,
    ethnicity_lucid = ethnicity,
    hispanic_lucid = hispanic,
    edu_lucid = education,
    pid_lucid = political_party,
    zip_lucid = zip,
    
    # Reported covariates
    sex = gender_rep.,
    hispanic = hispanic_rep,
    zipcode = zip_rep
  )

# Sex
g$gender_lucid <- ifelse(g$gender_lucid == "2", "Female", "Male")
g$female <- as.integer(g$sex == "Female")
g$sex_lucid_mis <- as.integer(with(g, sex != gender_lucid))

# Age
g$age_lucid <- as.numeric(g$age_lucid)
g <- g %>% rename(yearborn = yearborn._1)
g$age <- 2023 - as.numeric(g$yearborn)
g$age_lucid_mis <- with(g, as.integer(abs(age_lucid - age) > 2))
g$term <- ifelse(g$age < 18, "under18", g$term)
g$gc <- ifelse(g$age < 18, 3, g$gc)

# Hispanic
g$hispanic <- as.integer(g$hispanic == "Yes")
g$hispanic_lucid <- as.integer(g$hispanic_lucid != "1")
g$hispanic_lucid_mis <- as.integer(g$hispanic != g$hispanic_lucid)

# Race
g$black <- as.integer(g$race == "Black or African American")
g$black_lucid <- as.integer(g$ethnicity_lucid == "2")
g$black_lucid_mis <- as.integer(g$black != g$black_lucid)

g$aapi <- as.integer(grepl("Asian|Pacific", g$race))
g$aapi_lucid <- as.integer(as.numeric(g$ethnicity_lucid) >= 4 & as.numeric(g$ethnicity_lucid) <= 14)
g$aapi_lucid_mis <- as.integer(g$aapi != g$aapi_lucid)

g$white <- as.integer(g$race == "White")

# Education
g$college <- as.integer(grepl("Advanced|Bach", g$highested))
g$college_lucid <- as.integer(as.numeric(g$edu_lucid) %in% c(6,7,8))
g$college_lucid_mis <- as.integer(g$college != g$college_lucid)

g <- g %>%
  mutate(
    edu3 = case_when(
      highested %in% c("No High School", "Some High School", "High School Diploma or GED") ~ "High School or less",
      highested %in% c("Some college course work but non-degree or certificate", "Technical Certificate", "Associate Degree") ~ "Some College",
      highested %in% c("Bachelor’s Degree", "Advanced degree (post-college, such as JD or MBA)") ~ "BA or higher"
    )
  )
g$edu3 <- relevel(factor(g$edu3), ref = "High School or less")

# ZIP code comparison
g$zipcode <- substr(g$zipcode, 1, 5)
g$zipcode_lucid_mis <- with(g, as.integer(zip_lucid != zipcode))

# Create count of inconsistent Lucid responses
g$race_lucid_mis <- with(g, as.integer(black_lucid_mis == 1 | aapi_lucid_mis == 1))
g$lucid_mis <- with(g, zipcode_lucid_mis + sex_lucid_mis + age_lucid_mis + hispanic_lucid_mis + race_lucid_mis + college_lucid_mis)

g$term <- ifelse(g$lucid_mis > 2, "lucidmis", g$term)
g$gc <- ifelse(g$lucid_mis > 2, 3, g$gc)
g$term <- ifelse(g$Progress != 100, "Incomplete", g$term)

# Final data quality screen
g <- subset(g, gc == 1)

# Set up covariates
g$children <- as.integer(g$children == "Yes")
g$employ_bin <- ifelse(g$employ %in% c("Working full time now", "Working part time now"), 1, 0)

# Income
g$income_imp <- with(g, ifelse(income == "Prefer not to say", NA, income))
g$income_imp <- factor(g$income_imp, ordered = TRUE, levels = c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999", "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999", "$500,000 or more"))

median_level <- levels(g$income_imp)[median(as.integer(g$income_imp), na.rm = TRUE)]
g$income_imp <- ifelse(is.na(g$income_imp), median_level, levels(g$income_imp))
g$income_imp <- factor(g$income_imp, ordered = TRUE, levels = c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999", "$100,000 - $119,999", "$120,000 - $149,999", "$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999", "$500,000 or more"))

g <- g %>%
  mutate(
    income5 = case_when(
      #Income quintiles
      #https://www.census.gov/content/dam/Census/library/publications/2023/demo/p60-279.pdf
      income_imp %in% c("Less than $10,000", "$10,000 - $19,999", "$20,000 - $29,999") ~ "Q1",#30,000 =20th percentile
      income_imp %in% c("$30,000 - $39,999", "$40,000 - $49,999", "$50,000 - $59,999") ~ "Q2",#58,020 =40th percentile
      income_imp %in% c("$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $99,999") ~ "Q3",#94,000 = 60th percentile
      income_imp %in% c("$100,000 - $119,999", "$120,000 - $149,999") ~ "Q4",#153,000=80th percentile
      income_imp %in% c("$150,000 - $199,999", "$200,000 - $249,999", "$250,000 - $349,999", "$350,000 - $499,999", "$500,000 or more") ~ "Q5"
      ),
    #https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwiK6qSb8saDAxVFRTABHfLsAZMQFnoECBMQAw&url=https%3A%2F%2Fwww.census.gov%2Flibrary%2Fpublications%2F2023%2Fdemo%2Fp60-279.html%23%3A~%3Atext%3DHighlights%2Cand%2520Table%2520A%252D1).&usg=AOvVaw1pHbMRbyprlFZQrZHYtiL4&opi=89978449
    #Real median household income was $74,580 in 2022, a 2.3 percent decline from the 2021 estimate of $76,330 (Figure 1 and Table A-1).
    income_bin = as.integer(as.numeric(income_imp) >= 8)
  )
g$income5 <- relevel(factor(g$income5), ref = "Q1")

g$house_bin <- as.integer(g$house %in% c("Pay mortgage", "Own home with no payments due"))
g$rural <- as.integer(g$agglom == "A rural area")

# Fossil fuel energy reliance
g <- g %>%
  mutate(
    ffsector = case_when(
      sector1 == "No, none of the above" & sector1 != "" ~ 0,
      sector2 == "No, none of the above" & sector2 != "" ~ 0,
      sector3 == "No, none of the above" & sector3 != "" ~ 0,
      sector1 == "" & sector2 == "" & sector3 == "" ~ 0,
      T ~ 1
    )
  )

# Energy consumption
g <- g %>% rename(ac = heat_or_cool)
g <- g %>%
  mutate(
    heat_bill = case_when(
      ac == "Neither" ~ 0,
      ac == "Cooling" ~ 0,
      heat == "I don't know" ~ NA_real_,
      heat == "Less than $20" ~ 10,
      heat == "$20-$75" ~ (20+75)/2,
      heat == "$75-$125" ~ (125+75)/2,
      heat == "$125-$200" ~ (200+125)/2,
      heat == "$200-$250" ~ (200+250)/2,
      heat == "$250-$300" ~ (250+300)/2,
      heat == "More than $300" ~ 300
    ),
    heat_bill = case_when(is.na(heat_bill) ~ mean(heat_bill, na.rm = TRUE), T ~ heat_bill),
    heat_bin = as.integer(heat_bill > median(heat_bill)),
    
    cool_bill = case_when(
      ac == "Neither" ~ 0,
      ac == "Heating" ~ 0,
      cool == "I don't know" ~ NA_real_,
      cool == "Less than $20" ~ 10,
      cool == "$20-$75" ~ (20+75)/2,
      cool == "$75-$125" ~ (125+75)/2,
      cool == "$125-$200" ~ (200+125)/2,
      cool == "$200-$250" ~ (200+250)/2,
      cool == "$250-$300" ~ (250+300)/2,
      cool == "More than $300" ~ 300
    ),
    cool_bill = case_when(is.na(cool_bill) ~ mean(cool_bill, na.rm = TRUE), T ~ cool_bill),
    cool_bin = as.integer(cool_bill > median(cool_bill))
  )

# Gas consumption
g <- g %>%
  mutate(
    gasdrive_bill = case_when(
      gasdrive == "Less than $5" ~ 2.5,
      gasdrive == "$5-$25" ~ (25+5)/2,
      gasdrive == "$25-$75" ~ (75+25)/2,
      gasdrive == "$75-$125" ~ (125+75)/2,
      gasdrive == "$125-$175" ~ (175+125)/2,
      gasdrive == "$175-$225" ~ (225+175)/2,
      gasdrive == "More than $225" ~ 225
    ),
    drive_bin = as.integer(gasdrive_bill > median(gasdrive_bill))
  )

# Frequent flyer
g$flyer_num <- as.numeric(gsub(" or more", "", g$flyer))
g$flyer_bin <- as.integer(g$flyer != "0")

# Meat consumption
g$meat <- factor(g$meat, ordered = TRUE, levels = c("Never", "Less than once a week", "One to four times per week", "Almost or at least daily"))
g$meat_num <- as.numeric(g$meat)
g$meat_bin <- as.integer(g$meat %in% c("Almost or at least daily", "One to four times per week"))

# Mode of transport
g <- g %>%
  mutate(
    across(c(transport_1,transport_2,transport_3),
           .fns = list("car" = ~ as.integer(.x == "Car or Motorbike")),
           .names = "{.col}_{.fn}")
  )
g$car_bin <- with(g, as.integer(transport_1 == "Car or Motorbike" | transport_2 == "Car or Motorbike" | transport_3 == "Car or Motorbike"))

# Political and ideological covariates
# Religion
imp_scale <- c("Not at all important", "Slightly important", "Moderately important", "Very important", "Extremely important")
g <- g %>% mutate(across(c(religionimport, god), ~ factor(.x, ordered = TRUE, levels = imp_scale)))
g <- g %>% mutate(across(c(religionimport, god), .fns = list("num" = ~ as.numeric(.x)), .names = "{.col}_{.fn}"))

g$relig <- with(g, (scale(religionimport_num)[,1] + scale(god_num)[,1])/2)
g$relig <- scale(g$relig)[,1]

# Trust
g$trust <- factor(g$trust, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
g$trust_num <- as.numeric(g$trust)

g <- g %>% rename(trustsci = trustscience)
g$trustsci <- factor(g$trustsci, ordered = TRUE, levels = c("Not at all", "A little", "A moderate amount", "A lot", "Completely"))
g$trustsci_num <- as.numeric(g$trustsci)

# Ideology
g$ideo5 <- factor(g$ideo5, levels = c("Very conservative", "Conservative", "Moderate", "Liberal", "Very liberal", "Not sure"))
g <- g %>%
  mutate(
    ideo3 = case_when(
      ideo5 %in% c("Conservative", "Very conservative") ~ "Conservative",
      ideo5 %in% c("Liberal", "Very liberal") ~ "Liberal",
      ideo5 %in% c("Moderate") ~ "Moderate",
      ideo5 %in% c("Not sure") ~ "Not sure"
    )
  )
g$ideo3 <- relevel(factor(g$ideo3), ref = "Moderate")

# Partisanship
g <- g %>%
  mutate(
    pid7 = case_when(
      demstrength == "Strong Democrat" ~ "Strong Democrat",
      demstrength == "Not so strong Democrat" ~ "Not so strong Democrat",
      repstrength == "Strong Republican" ~ "Strong Republican",
      repstrength == "Not so strong Republican" ~ "Not so strong Republican",
      indeplean == "Democratic Party" ~ "Lean Democrat",
      indeplean == "Republican Party" ~ "Lean Republican",
      indeplean == "Neither" ~ "Independent",
      indeplean == "Not sure" ~ "Not sure"
    ),
    pid7_scale = dplyr::case_when(
      pid7 == "Strong Democrat" ~ 1,
      pid7 == "Not so strong Democrat" ~ 2,
      pid7 == "Lean Democrat" ~ 3,
      pid7 == "Independent" ~ 4,
      pid7 == "Not sure" ~ 4,
      pid7 == "Lean Republican" ~ 5,
      pid7 == "Not so strong Republican" ~ 6,
      pid7 == "Strong Republican" ~ 7
    )
  )

g <- g %>%
  mutate(
    pid3 = case_when(
      pid7_scale %in% c(1,2,3) ~ "Democrat",
      pid7_scale == 4 ~ "Neither",
      pid7_scale %in% c(5,6,7) ~ "Republican"
    )
  )
g$pid3 <- relevel(factor(g$pid3), ref = "Democrat")

# Climate Change Beliefs
g <- g %>%
  mutate(
    gwknow = factor(
      gwknow,
      ordered = TRUE,
      levels = c("Never heard of it", "A little", "A moderate amount", "A lot", "A great deal")
    ),
    gwknow_num = as.numeric(gwknow),
    gwknow_bin = as.integer(gwknow_num >= 4),
    gwknow_block = as.integer(gwknow %in% c("A great deal", "A lot", "A moderate amount")),
    
    gwimport = factor(
      gwimport,
      ordered = TRUE,
      levels = c("Not at all important", "Not too important", "Somewhat important", "Very important", "Extremely important")
    ),
    gwimport_num = as.numeric(gwimport),
    gwimport_bin = as.integer(gwimport_num >= 4),
    
    gwworry = factor(
      gwworry,
      ordered = TRUE,
      levels = c("Not at all worried", "Not very worried", "Somewhat worried", "Very worried")
    ),
    gwworry_num = as.numeric(gwworry),
    gwworry_bin = as.integer(gwworry_num >= 3),
    
    harmfuture_num = case_when(
      harmfuture == "Not at all" ~ -3,
      harmfuture == "Only a little" ~ -2,
      harmfuturedk == "Global warming will not harm me much if at all" ~ -1,
      harmfuture == "A moderate amount" ~ 0,
      harmfuturedk == "Global warming will harm me a lot" ~ 1,
      harmfuture == "A lot" ~ 2,
      harmfuture == "A great deal" ~ 3
    ),
    harmfuture_bin = as.integer(harmfuture %in% c("A great deal", "A lot")),
    
    harmpersonal_num = case_when(
      harmpersonal == "Not at all" ~ -3,
      harmpersonal == "Only a little" ~ -2,
      harmpersonaldk == "Global warming will not harm me much if at all" ~ -1,
      harmpersonal == "A moderate amount" ~ 0,
      harmpersonaldk == "Global warming will harm me a lot" ~ 1,
      harmpersonal == "A lot" ~ 2,
      harmpersonal == "A great deal" ~ 3
    ),
    harmpersonal_bin = as.integer(harmpersonal %in% c("A great deal", "A lot"))
  )

g$gwbeliefs <- with(g, (gwimport_bin + gwworry_bin + harmfuture_bin + harmpersonal_bin) / 5)

# Knowledge about climate change
g$knowtiming_bin <- as.integer(g$knowtiming == "More than 25 years")

g <- g %>%
  mutate(
    across(c(knowmitigate, knowadapt), ~ factor(.x, ordered = TRUE, levels = c("Never heard of it", "A little", "A moderate amount", "A lot", "A great deal"))),
    across(c(knowmitigate, knowadapt), .fns = list("num" = ~ as.numeric(.x)), .names = "{.col}_{.fn}")
  )

g$knowdiff <- as.integer(g$mitigatevsadapt == "Mitigation tries to prevent climate change by reducing emissions, whereas adaptation tries to reduce damage from climate change")
g$knowindex <- with(g, ((gwknow_num/5) + (knowmitigate_num/5) + (knowadapt_num/5) + knowtiming_bin + knowdiff) / 5)

# International contributions and co-benefits
g$globalcontr <- factor(g$globalcontr, ordered = TRUE, levels = c("Almost nothing", "A little", "A moderate amount", "A lot", "A great deal"))
g$globalcontr_num <- as.numeric(g$globalcontr)
g$globalcontr_bin <- as.integer(g$globalcontr_num >= 4)

g <- g %>% rename(cobenefits = mitigatebenefit)
g$cobenefits <- factor(g$cobenefits, ordered = TRUE, levels = c("Not at all", "A little", "A moderate amount", "A lot", "A great deal"))
g$cobenefits_num <- as.numeric(g$cobenefits)
g$cobenefits_bin <- as.integer(g$cobenefits_num >= 4)

g$sanction <- factor(g$sanction, ordered = TRUE, levels = c("Not at all effective", "Slightly effective", "Moderately effective", "Very effective", "Extremely effective"))
g$sanction_num <- as.numeric(g$sanction)
g$sanction_bin <- as.integer(g$sanction_num >= 4)

g <- g %>% rename(alreadymit = mitigate)
g$alreadymit <- factor(g$alreadymit, ordered = TRUE, levels = c("Almost nothing", "A little", "A moderate amount", "A lot", "A great deal"))
g$alreadymit_num <- as.numeric(g$alreadymit)
g$alreadymit_bin <- as.integer(g$alreadymit_num >= 4)

g <- g %>% rename(alreadyadapt = adapt)
g$alreadyadapt <- factor(g$alreadyadapt, ordered = TRUE, levels = c("Almost nothing", "A little", "A moderate amount", "A lot", "A great deal"))
g$alreadyadapt_num <- as.numeric(g$alreadyadapt)
g$alreadyadapt_bin <- as.integer(g$alreadyadapt_num >= 4)

g$harmtiming <- factor(g$harmtiming, ordered = TRUE, levels = c("Never", "In 100 years", "In 50 years", "In 25 years", "In 10 years", "They are being harmed now"))
g$harmtiming_num <- as.numeric(g$harmtiming)

g$experience <- factor(g$experience, ordered = TRUE, levels = c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree"))
g$experience_num <- as.numeric(g$experience)
g$experience_bin <- as.integer(g$experience_num >= 4)

# Climate experience variables
g <- g %>%
  rename(
    exp_hur = experienceevent_1,
    exp_fire = experienceevent_2,
    exp_flood = experienceevent_3,
    exp_drought = experienceevent_4,
    exp_heat = experienceevent_5,
    exp_slr = experienceevent_6,
    
    expfreq_hur = experiencefreq_1,
    expfreq_fire = experiencefreq_2,
    expfreq_flood = experiencefreq_3,
    expfreq_drought = experiencefreq_4,
    expfreq_heat = experiencefreq_5,
    expfreq_slr = experiencefreq_6
  )

g <- g %>%
  mutate(
    across(c(exp_hur, exp_fire, exp_flood, exp_drought, exp_heat, exp_slr),
           .fns = list("bin" = ~ as.integer(.x == "Yes")),
           .names = "{.col}_{.fn}"),
  )

g <- g %>%
  mutate(
    across(c(expfreq_hur, expfreq_fire, expfreq_flood, expfreq_drought, expfreq_heat, expfreq_slr),
           .fns = list("num" = ~ case_when(
             .x == "More than 3 times" ~ 3,
             .x == "2 times" ~ 2,
             .x == "Once" ~ 1,
             T ~ 0
             )
             ),
           .names = "{.col}_{.fn}")
  )

g$disasters <- with(g, expfreq_hur_num + expfreq_fire_num + expfreq_flood_num + expfreq_drought_num + expfreq_heat_num + expfreq_slr_num)
g$disasters_bin <- as.integer(g$disasters > median(g$disasters))

# Climate Vulnerability Beliefs
g <- g %>%
  mutate(
    vulnoverall = factor(vulnoverall, levels = c("Don't know", "Benefit from global warming", "Feel no effects of global warming", "Harmed by global warming")),
    vulnoverall_num = case_when(
      vulnoverall == "Benefit from global warming" & grepl("Greatly", vulnoverallstrength) ~ -3,
      vulnoverall == "Benefit from global warming" & grepl("Somewhat", vulnoverallstrength) ~ -2,
      vulnoverall == "Benefit from global warming" & grepl("Barely", vulnoverallstrength) ~ -1,
      vulnoveralldk == "Benefit from global warming" ~ -1,
      vulnoveralldk == "Feel no effects of global warming" ~ 0,
      vulnoverall == "Feel no effects of global warming" ~ 0,
      vulnoveralldk == "Harmed by global warming" ~ 1,
      vulnoverall == "Harmed by global warming" & grepl("Barely", vulnoverallstrength) ~ 1,
      vulnoverall == "Harmed by global warming" & grepl("Somewhat", vulnoverallstrength) ~ 2,
      vulnoverall == "Harmed by global warming" & grepl("Greatly", vulnoverallstrength) ~ 3
    )
  )

g$vulnoverall <- gsub("from global warming|of global warming|by global warming", "", g$vulnoverall)
g$vulnoverall <- trimws(g$vulnoverall)
g$vulnoverall <- ifelse(g$vulnoverall == "Feel no effects", "No effect", g$vulnoverall)
g$vulnoverall <- factor(g$vulnoverall, levels = c("Don't know", "Benefit", "No effect", "Harmed"))

g <- g %>%
  mutate(
    vulndisaster = factor(vulndisaster, levels = c("Don't know", "Decrease", "Not change", "Increase")),
    vulndisaster_num = case_when(
      vulndisaster == "Decrease" & grepl("a great deal", vulndisasterstrength) ~ -3,
      vulndisaster == "Decrease" & grepl("somewhat", vulndisasterstrength) ~ -2,
      vulndisaster == "Decrease" & grepl("a little", vulndisasterstrength) ~ -1,
      vulndisasterdk == "Decrease" ~ -1,
      vulndisasterdk == "No change" ~ 0,
      vulndisaster == "Not change" ~ 0,
      vulndisasterdk == "Increase" ~ 1,
      vulndisaster == "Increase" & grepl("a little", vulndisasterstrength) ~ 1,
      vulndisaster == "Increase" & grepl("somewhat", vulndisasterstrength) ~ 2,
      vulndisaster == "Increase" & grepl("a great deal", vulndisasterstrength) ~ 3
    )
  )

g <- subset(g, select = -c(vulnincome:vulnbizstrength_Click.Count))

g <- g %>%
  mutate(
    vulnmove = factor(vulnmove, ordered = TRUE, levels = c("Don't know", "Not at all likely", "Not too likely", "Moderately likely", "Very likely", "Extremely likely")),
    vulnmove_num = case_when(
      vulnmove == "Not at all likely" ~ -3,
      vulnmove == "Not too likely" ~ -2,
      vulnmovedk == "Unlikely to have to move" ~ -1,
      vulnmove == "Moderately likely" ~ 0,
      vulnmovedk == "Likely to have to move" ~ 1,
      vulnmove == "Very likely" ~ 2,
      vulnmove == "Extremely likely" ~ 3
    )
  )

g <- g %>%
  mutate(
    vulnmovecom = factor(vulnmovecom, levels = c("Don't know", "Not at all likely", "Not too likely", "Moderately likely", "Very likely", "Extremely likely")),
    vulnmovecom_num = case_when(
      vulnmovecom == "Not at all likely" ~ -3,
      vulnmovecom == "Not too likely" ~ -2,
      vulnmovecomdk == "Unlikely to have to move" ~ -1,
      vulnmovecom == "Moderately likely" ~ 0,
      vulnmovecomdk == "Likely to have to move" ~ 1,
      vulnmovecom == "Very likely" ~ 2,
      vulnmovecom == "Extremely likely" ~ 3
    )
  )

g <- g %>%
  mutate(
    vulnmove_cat = case_when(
      vulnmove == "Don't know" ~ "Don't know",
      vulnmove %in% c("Not at all likely", "Not too likely") ~ "Not likely",
      vulnmove == "Moderately likely" ~ "Moderately likely",
      vulnmove %in% c("Very likely", "Extremely likely") ~ "Very Likely"
    ),
    vulnmove_cat = factor(vulnmove_cat, levels = c("Don't know", "Not likely", "Moderately likely", "Very Likely"))
  )

# Income vulnerability  
g <- g %>% rename(
  vulnincome = newincomevuln,
  vulnincomestrength = newincomevulnstrengt,
  vulnincomedk = newincomevulndk
  )

g <- g %>%
  mutate(
    vulnincome = factor(vulnincome, levels = c("Don't know", "Richer", "No effect", "Poorer")),
    vulnincome_num = case_when(
      vulnincome == "Richer" & vulnincomestrength == "A great deal ${q://QID240/ChoiceGroup/SelectedChoices}" ~ -3,
      vulnincome == "Richer" & vulnincomestrength == "Somewhat ${q://QID240/ChoiceGroup/SelectedChoices}" ~ -2,
      vulnincome == "Richer" & vulnincomestrength == "A little ${q://QID240/ChoiceGroup/SelectedChoices}" ~ -1,
      vulnincomedk == "Richer" ~ -1,
      vulnincomedk == "No effect" ~ 0,
      vulnincome == "No effect" ~ 0,
      vulnincomedk == "Poorer" ~ 1,
      vulnincome == "Poorer" & vulnincomestrength == "A little ${q://QID240/ChoiceGroup/SelectedChoices}" ~ 1,
      vulnincome == "Poorer" & vulnincomestrength == "Somewhat ${q://QID240/ChoiceGroup/SelectedChoices}" ~ 2,
      vulnincome == "Poorer" & vulnincomestrength == "A great deal ${q://QID240/ChoiceGroup/SelectedChoices}" ~ 3
    )
  )

# Living desirability vulnerability
g <- g %>%
  rename(vulnlive = livevuln) %>%
  mutate(
    vulnlive_num = case_when(
      vulnlive == "More desirable" & livevulnstrength == "Much ${q://QID246/ChoiceGroup/SelectedChoices}" ~ -3,
      vulnlive == "More desirable" & livevulnstrength == "Somewhat ${q://QID246/ChoiceGroup/SelectedChoices}" ~ -2,
      vulnlive == "More desirable" & livevulnstrength == "A little ${q://QID246/ChoiceGroup/SelectedChoices}" ~ -1,
      livevulndk == "More desirable" ~ -1,
      livevulndk == "No effect" ~ 0,
      vulnlive == "No effect" ~ 0,
      livevulndk == "Less desirable" ~ 1,
      vulnlive == "Less desirable" & livevulnstrength == "A little ${q://QID246/ChoiceGroup/SelectedChoices}" ~ 1,
      vulnlive == "Less desirable" & livevulnstrength == "Somewhat ${q://QID246/ChoiceGroup/SelectedChoices}" ~ 2,
      vulnlive == "Less desirable" & livevulnstrength == "Much ${q://QID246/ChoiceGroup/SelectedChoices}" ~ 3
    )
  )
g$vulnlive <- factor(g$vulnlive, levels = c("Don't know", "More desirable", "No effect", "Less desirable"))

# Business desirability vulnerability
g <- g %>% rename(
  vulnbiz = newbisvuln.,
  vulnbizstrength = newbisvulnstrength,
  vulnbizdk = newbisvulndk
  )

g <- g %>%
  mutate(
    vulnbis = factor(vulnbiz, levels = c("Don't know", "More desirable", "No effect", "Less desirable")),
    vulnbis_num = case_when(
      vulnbis == "More desirable" & vulnbizstrength == "A great deal ${q://QID266/ChoiceGroup/SelectedChoices}" ~ -3,
      vulnbis == "More desirable" & vulnbizstrength == "Somewhat ${q://QID266/ChoiceGroup/SelectedChoices}" ~ -2,
      vulnbis == "More desirable" & vulnbizstrength == "A little ${q://QID266/ChoiceGroup/SelectedChoices}" ~ -1,
      vulnbizdk == "More desirable" ~ -1,
      vulnbizdk == "No effect" ~ 0,
      vulnbis == "No effect" ~ 0,
      vulnbizdk == "Less desirable" ~ 1,
      vulnbis == "Less desirable" & vulnbizstrength == "A little ${q://QID266/ChoiceGroup/SelectedChoices}" ~ 1,
      vulnbis == "Less desirable" & vulnbizstrength == "Somewhat ${q://QID266/ChoiceGroup/SelectedChoices}" ~ 2,
      vulnbis == "Less desirable" & vulnbizstrength == "A great deal ${q://QID266/ChoiceGroup/SelectedChoices}" ~ 3
    )
  )

# Create vulnerability index
g$vulnindex <- with(g, (scale(vulnoverall_num)[,1] + scale(vulndisaster_num)[,1] + scale(vulnincome_num)[,1] + scale(vulnlive_num)[,1] + scale(vulnbis_num)[,1] + scale(vulnmove_num)[,1] + scale(vulnmovecom_num)[,1]) / 7)
g$vulnindex <- scale(g$vulnindex)[,1]

# Confidence measures
g <- g %>%
  rename(
    vulnincomesure = newincomesure,
    vulnlivesure = livevulnsure,
    vulnbissure = newbisvulnsure
  ) %>%
  mutate(
    across(
      c(vulnoverallsure, vulndisastersure, vulnincomesure, vulnlivesure, vulnbissure, vulnmovecomsure),
      .fns = list("num" = ~ case_when(
        .x == "Completely sure" ~ 5,
        .x == "Very sure" ~ 4,
        .x == "Moderately sure" ~ 3,
        .x == "Not too sure" ~ 2,
        .x == "Not at all sure" ~ 1)
        ),
      .names = "{.col}_{.fn}"
    ),
    across(
      c(vulnoverallsure, vulndisastersure, vulnincomesure, vulnlivesure, vulnbissure, vulnmovecomsure),
      ~ factor(.x, ordered = TRUE, levels = c("Not at all sure", "Not too sure", "Moderately sure", "Very sure", "Completely sure")
      )
  )
  )

# Policy efficacy beliefs
g <- g %>%
  mutate(
    across(c(effect_adapt, effect_mitigate), ~ factor(.x, ordered = TRUE, levels = c("Not effective at all", "Slightly effective", "Moderately effective", "Very effective", "Extremely effective"))),
    across(c(effect_adapt, effect_mitigate), .fns = list("num" = ~ as.numeric(.x), "bin" = ~ as.integer(as.numeric(.x) > 3)), .names = "{.col}_{.fn}")
  )
g$effect_mitigate_lab <- ifelse(g$effect_mitigate_bin == 1, "Mitigation Policy More Effective", "Mitigation Policy Less Effective")
g$effect_adapt_lab <- ifelse(g$effect_adapt_bin == 1, "Adaptation Policy More Effective", "Adaptation Policy Less Effective")

# Issue knowledge
g <- g %>%
  rename(
    knowissues_ai = knowissues_1,
    knowissues_img = knowissues_2,
    knowissues_mil = knowissues_3,
    knowissues_pov = knowissues_4
  )
g <- g %>%
  mutate(
    across(c(knowissues_ai, knowissues_img, knowissues_mil, knowissues_pov), ~ factor(.x, ordered = TRUE, levels = c("Never heard of it", "A little", "A moderate amount", "A lot", "A great deal"))),
    across(c(knowissues_ai, knowissues_img, knowissues_mil, knowissues_pov), .fns = list("num" = ~ as.numeric(.x)), .names = "{.col}_{.fn}")
  )

g <- g %>%
  rename(
    importissue_ai = importissue_1,
    importissue_img = importissue_2,
    importissue_mil = importissue_3,
    importissue_pov = importissue_4
  )
g <- g %>%
  mutate(
    across(c(importissue_ai, importissue_img, importissue_mil, importissue_pov), ~ factor(.x, ordered = TRUE, levels = c("Not at all important", "Not too important", "Somewhat important", "Very important", "Extremely important"))),
    across(c(importissue_ai, importissue_img, importissue_mil, importissue_pov), .fns = list("num" = ~ as.numeric(.x)), .names = "{.col}_{.fn}")
  )

g$polknow <- with(g, (scale(knowissues_ai_num)[,1] + scale(knowissues_mil_num)[,1] + scale(knowissues_img_num)[,1] + scale(knowissues_pov_num)) / 4)
g$polknow <- scale(g$polknow)[,1]

# Identify respondent counties from ZIP codes
g$zipcode <- as.numeric(g$zipcode)
g$zip_lucid <- as.numeric(g$zip_lucid)

# Pair ZIP Codes with Counties
countyzip <- read_xlsx(here("Data", "crosswalks", "county_2_zip", "ZCTA County Bridge 2018.xlsx"))
names(countyzip) <- c("ztca", "fips", "county", "fips_bea", 'bea_name') 
countyzip$fips <- as.numeric(countyzip$fips)

zipcross <- read_xlsx(here("Data", "crosswalks", "zip_2_zcta", "ZIPCodetoZCTACrosswalk2021UDS.xlsx"))
sample_zip <- left_join(countyzip, zipcross, by = c("ztca" = "ZCTA"), relationship = "many-to-many")
sample_zip <- subset(sample_zip, select = c(fips, ZIP_CODE))
sample_zip$ZIP_CODE <- as.numeric(sample_zip$ZIP_CODE)

# Handle multiple counties per ZIP code by taking the first match
sample_zip <- sample_zip %>%
  filter(!is.na(ZIP_CODE) & !is.na(fips)) %>%
  group_by(ZIP_CODE) %>%
  slice_head(n = 1) %>%
  ungroup()

g <- left_join(g, sample_zip, by = c("zipcode" = "ZIP_CODE"))
g$fips <- as.numeric(g$fips)

# Load objective data on climate vulnerability
damages <- read.csv(here("Data", "input", "usa", "hsiang_damage", "county_damages_by_sector.csv"))
names(damages) <- c("state", "county", "fips", "pop2012", "income2012", "agdamage", "mortality", "energycosts", "laborlowrisk", "laborhighrisk", "coastaldamage", "propertycrime", "violentcrime", "totaldamage")

damages <- subset(damages, select = c(fips:totaldamage))
damages <- damages %>%
  mutate(
    across(c(pop2012:totaldamage), .fns = list("z" = ~ scale(.x)[,1]), .names = "{.col}_{.fn}"),
    across(c(agdamage:totaldamage), .fns = list("bin" = ~ case_when(
      .x > 0 ~ 1,
      is.na(.x) ~ 0,
      T ~ 0
    )), .names = "{.col}_{.fn}")
  )

g <- left_join(g, damages, by = "fips")

# Final coding for labels
g$heat_bin <- with(g, ifelse(heat_bin == 1, "Above Median Heat Bill", "Below Median Heat Bill"))
g$cool_bin <- with(g, ifelse(cool_bin == 1, "Above Median Cooling Bill", "Below Median Cooling Bill"))
g$drive_bin <- with(g, ifelse(drive_bin == 1, "Above Median Gas Expenses", "Below Median Gas Expenses"))
g$fly_bin <- with(g, ifelse(flyer == 1, "More than 1 flight", "No flights in past year"))
g$rural <- with(g, ifelse(rural == 1, "Rural", "Non-rural"))
g$ffsector <- ifelse(g$ffsector == 1, "Energy Sector Employment", "Non-Energy Employment")
g$ffsector <- relevel(factor(g$ffsector), ref = "Non-Energy Employment")
g$ffsector_label <- as.character(g$ffsector)
g$meat_bin <- ifelse(g$meat_bin == 1, "High Beef Consumption", "Low Beef Consumption")

# Create indicators for summary statistics
g$dem <- as.integer(g$pid3 == "Democrat")
g$rep <- as.integer(g$pid3 == "Republican")
g$incomeq1 <- as.integer(g$income5 == "Q1")
g$incomeq2 <- as.integer(g$income5 == "Q2")
g$incomeq3 <- as.integer(g$income5 == "Q3")
g$incomeq4 <- as.integer(g$income5 == "Q4")
g$incomeq5 <- as.integer(g$income5 == "Q5")
g$eduhsless <- as.integer(g$edu3 == "High School or less")
g$eduba <- as.integer(g$edu3 == "BA or higher")
g$edusomecol <- as.integer(g$edu3 == "Some College")
g$student <- as.integer(g$employ == "Student")
g$retired <- as.integer(g$employ == "Retired")
g$ffsector_bin <- as.integer(g$ffsector == "Energy Sector Employment")

g <- g %>%
  mutate(
    age18to24 = as.integer(age <= 24),
    age25to34 = as.integer(age >= 25 & age <= 34),
    age35to44 = as.integer(age >= 35 & age <= 44),
    age45to64 = as.integer(age >= 45 & age <= 64),
    age65over = as.integer(age >= 65)
  )

g$rural_bin <- as.integer(g$rural=="Rural")

g$Northeast <- as.integer(g$region == "1")
g$Midwest <- as.integer(g$region == "2")  
g$South <- as.integer(g$region == "3")
g$West <- as.integer(g$region == "4")

# Create indicator labels for plots
g$damage_label <- ifelse(g$totaldamage_bin == 1, "Future Damages", "Potential Net-Benefits")
g$damagefacet <- ifelse(g$totaldamage_bin==1, "Damage", "Benefit")
g$disasters_label <- ifelse(g$disasters_bin == 1, "Above Median Disasters", "Below Median Disasters")
g$knowindex_bin <- with(g, ifelse(knowindex > median(knowindex), "Above Median Climate Knowledge", "Below Median Climate Knowledge"))
g$sanction_label <- ifelse(g$sanction_bin == 1, "Extrinsic Reciprocity Effective", "Extrinsic Reciprocity NOT Effective")

g$existingpolicy <- with(g, (scale(alreadyadapt_num)[,1] + scale(alreadymit_num)[,1])/2)
g$existingpolicy <- scale(g$existingpolicy)[,1]
g$existingpolicy_bin <- with(g, ifelse(existingpolicy > median(existingpolicy), "More Existing Policy", "Less Existing Policy"))

g$gwbeliefs_bin <- ifelse(g$gwbeliefs > median(g$gwbeliefs), "Above Median Climate Beliefs", "Below Median Climate Beliefs")
g$polknow_bin <- ifelse(g$polknow > median(g$polknow), "Greater Policy Knowledge", "Lesser Policy Knowledge")

g$age_z <- scale(g$age)[,1]

# Subset to variables needed for downstream analyses
analysis_vars <- c(
  # Survey metadata
  "EndDate",
  
  # Outcome variables
  "vulnindex",
  
  # Key predictors (objective climate damage)
  "totaldamage", "totaldamage_z", "totaldamage_bin",
  
  # Key predictors (disaster experience)
  "disasters_bin",
  
  # Demographic controls
  "age_z", "female", "white", "black", "hispanic", "aapi",
  
  # Education variables
  "edu3", "eduhsless", "edusomecol", "eduba",
  
  # Political variables
  "pid3", "dem", "rep", "ideo3",
  
  # Other individual characteristics
  "trust_num", "relig", "ffsector", "rural", "rural_bin", "gwknow_block",
  
  # Geographic variables
  "region", "Northeast", "Midwest", "South", "West",
  
  # Age categories for sample description
  "age18to24", "age25to34", "age35to44", "age45to64", "age65over",
  
  # Income quintiles for sample description
  "incomeq1", "incomeq2", "incomeq3", "incomeq4", "incomeq5",
  
  # Employment variables for sample description
  "employ_bin", "student", "retired"
)

g <- g %>% 
  dplyr::select(all_of(analysis_vars))

# Only sample with complete vulnerability battery
g <- na.omit(g)
message(paste0("N=", nrow(g)))
saveRDS(g, here("Data", "inter", "survey", "lucid", "202401_survey.rds"))
message("Processed survey data and saved to Data/inter/survey/lucid/202401_survey.rds")
