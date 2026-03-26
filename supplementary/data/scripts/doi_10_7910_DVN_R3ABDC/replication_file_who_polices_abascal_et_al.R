################
# The following replication file corresponds to "Who Polices which Boundaries? 
# How Racial Self-Identification Affects External Classification" 
# by Maria Abascal, Amada Armenta, W. M. Halm, and Daniel J. Hopkins.
# The code was created by William Halm using R 4.4.2 on May 21, 2025.

# This code first loads in and cleans the data set "CleanedPooledREPS_04252023_WH.csv"
# that has been provided for replication.
# The code then creates the tables for the main text, appendix, and online supplement
# before creating all of the figures.
# Please note that creating the figures will require creating some of the tables first,
# as noted in the appropriate places in the file.
# Please note that the figures create labels that can appear quite different 
# depending on the dimensions of your computer screen.
################

library(descr)
library(cregg)
library(grid)
library(xtable)
library(psych)
library(texreg)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pBrackets)

# Load in/clean data----
## replace with computer file path:
r4 <- read.csv("C:/Users/wmhal/Dropbox/Amada - Dan - Maria/Data/Conjoint data for analysis/CleanedPooledREPS_04252023_WH.csv")
r3 <- r4

## categorical variables as factor variables: profile characteristics
r3$gender <- factor(r3$gender, levels = c("F", "M"))
r3$ethnicity <- factor(r3$ethnicity, levels = c("Latino","White"))
r3$skin_tone <- factor(r3$skin_tone, levels = c("Light","Medium ","Dark"))
r3$skin_tone_rev <- factor(r3$skin_tone, levels = c("Dark","Medium ","Light"))
r3$birthparents_ethnicID <- factor(r3$birthparents_ethnicID, levels = c("White (parents)", "Asian/Asian-American","Black/African-American","Hispanic/Latino(a/x)","Middle Eastern or North African"))
r3$birthparents_ethnicID_white <- factor(r3$birthparents_ethnicID, levels = c("Black/African-American","White (parents)", "Asian/Asian-American","Hispanic/Latino(a/x)","Middle Eastern or North African"))
r3$ancestry_binary <- r3$birthparents_ethnicID %>%
  recode(
    "Asian/Asian-American" = "Non-White parents",
    "Black/African-American" = "Non-White parents",
    "Hispanic/Latino(a/x)" = "Non-White parents",
    "Middle Eastern or North African" = "Non-White parents"
  )
r3$ancestry_binary <- factor(r3$ancestry_binary, levels = c("Non-White parents","White (parents)"))
r3$age <- factor(r3$age, levels = c("17","18","19","20","21"))
r3$occup_status <- factor(r3$occup_status, levels = c("Low","Medium","High"))
r3$Religion <- factor(r3$religion, levels = c("Christian","Muslim","Not available"))
r3$english <- factor(r3$english, levels = c("Don't speak English at home","Speak English at home"))
r3$selfID_ethnic <- factor(r3$selfID_ethnic, levels = c("White (self)","Asian","Black","Hispanic","MENA"))
r3$selfID_ethnic_white <- factor(r3$selfID_ethnic, levels = c("Black","White (self)","Asian","Hispanic","MENA"))
r3$self_binary <- r3$selfID_ethnic %>%
  recode(
    "Asian" = "Non-White",
    "Black" = "Non-White",
    "Hispanic" = "Non-White",
    "MENA" = "Non-White"
  )
r3$self_binary <- factor(r3$self_binary, levels = c("Non-White","White (self)"))
### which image was used in the profile?
r3$image_ID <- factor(r3$image_ID, levels = c("LF201", "LF206", "LF210", "LM212", "LM228", "LM231", "WF203", "WF246", "WM022", "WM233"))
r3 <- r3 %>%
  mutate(Latina1 = case_when(image_ID == "LF201" ~ "Latina1",
                             image_ID != "LF201" ~ "Not Latina1"),
         Latina2 = case_when(image_ID == "LF206" ~ "Latina2",
                             image_ID != "LF206" ~ "Not Latina2"),
         Latina3 = case_when(image_ID == "LF210" ~ "Latina3",
                             image_ID != "LF210" ~ "Not Latina3"),
         Latino1 = case_when(image_ID == "LM212" ~ "Latino1",
                             image_ID != "LM212" ~ "Not Latino1"),
         Latino2 = case_when(image_ID == "LM228" ~ "Latino2",
                             image_ID != "LM228" ~ "Not Latino2"),
         Latino3 = case_when(image_ID == "LM231" ~ "Latino3",
                             image_ID != "LM231" ~ "Not Latino3"),
         WhiteWoman1 = case_when(image_ID == "WF203" ~ "WhiteWoman1",
                                 image_ID != "WF203" ~ "Not WhiteWoman1"),
         WhiteWoman2 = case_when(image_ID == "WF246" ~ "WhiteWoman2",
                                 image_ID != "WF246" ~ "Not WhiteWoman2"),
         WhiteMan1 = case_when(image_ID == "WM022" ~ "WhiteMan1",
                               image_ID != "WM022" ~ "Not WhiteMan1"),
         WhiteMan2 = case_when(image_ID == "WM233" ~ "WhiteMan2",
                               image_ID != "WM233" ~ "Not WhiteMan2"))
### indicator variables for image
r3$Latina1 <- factor(r3$Latina1, levels = c("Not Latina1", "Latina1"))
r3$Latina2 <- factor(r3$Latina2, levels = c("Not Latina2", "Latina2"))
r3$Latina3 <- factor(r3$Latina3, levels = c("Not Latina3", "Latina3"))
r3$Latino1 <- factor(r3$Latino1, levels = c("Not Latino1", "Latino1"))
r3$Latino2 <- factor(r3$Latino2, levels = c("Not Latino2", "Latino2"))
r3$Latino3 <- factor(r3$Latino3, levels = c("Not Latino3", "Latino3"))
r3$WhiteWoman1 <- factor(r3$WhiteWoman1)
r3$WhiteWoman2 <- factor(r3$WhiteWoman2)
r3$WhiteMan1 <- factor(r3$WhiteMan1)
r3$WhiteMan2 <- factor(r3$WhiteMan2)

## categorical variables as factor variables: respondent/sample characteristics
r3$resrace <- factor(r3$resrace, levels = c("Wht","Asn","Blk","Hisp"))
r3$Gender <- factor(r3$Gender, levels = c("Female","Male"))
r3$Income <- factor(r3$Income, levels = c("Less than $40K","$40-80K", "$80K or more"))
r3$Nativity <- factor(r3$Nativity, levels = c("US-born","Foreign-born"))
r3$Condition <- factor(r3$Condition, levels = c("Anonymous Survey","Scholarship Application"))
r3$party <- factor(r3$party, levels = c("Democrat","Republican","Other"))
r3$educ <- factor(r3$educ, levels = c("Less than high school","High school","Some college or more"))
r3$Age <- factor(r3$Age, levels = c("29 years or younger", "30 years or older"))
r3$Sample <- factor(r3$Sample, levels = c("Student", "Dynata"))

## indicator variables for self-ID
r3$selfIDWhite[r3$selfID_ethnic == "White (self)"] <- 1
r3$selfIDWhite[r3$selfID_ethnic != "White (self)"] <- 0

r3$selfIDBlack[r3$selfID_ethnic == "Black"] <- 1
r3$selfIDBlack[r3$selfID_ethnic != "Black"] <- 0

r3$selfIDHispanic[r3$selfID_ethnic == "Hispanic"] <- 1
r3$selfIDHispanic[r3$selfID_ethnic != "Hispanic"] <- 0

r3$selfIDAsian[r3$selfID_ethnic == "Asian"] <- 1
r3$selfIDAsian[r3$selfID_ethnic != "Asian"] <- 0

r3$selfIDMENA[r3$selfID_ethnic == "MENA"] <- 1
r3$selfIDMENA[r3$selfID_ethnic != "MENA"] <- 0

## indicator variables for ancestry
r3$ancestryWhite[r3$birthparents_ethnicID == "White (parents)"] <- 1
r3$ancestryWhite[r3$birthparents_ethnicID != "White (parents)"] <- 0

r3$ancestryBlack[r3$birthparents_ethnicID == "Black/African-American"] <- 1
r3$ancestryBlack[r3$birthparents_ethnicID != "Black/African-American"] <- 0

r3$ancestryHispanic[r3$birthparents_ethnicID == "Hispanic/Latino(a/x)"] <- 1
r3$ancestryHispanic[r3$birthparents_ethnicID != "Hispanic/Latino(a/x)"] <- 0

r3$ancestryAsian[r3$birthparents_ethnicID == "Asian/Asian-American"] <- 1
r3$ancestryAsian[r3$birthparents_ethnicID != "Asian/Asian-American"] <- 0

r3$ancestryMENA[r3$birthparents_ethnicID == "Middle Eastern or North African"] <- 1
r3$ancestryMENA[r3$birthparents_ethnicID != "Middle Eastern or North African"] <- 0

## indicator variables for whether respondents agree (strongly or somewhat) 
### that a profile belongs to a given ethnoracial category
r3$White_agree_prct <- r3$White_agree %>%
  recode(
    "1" = "0",
    "2" = "0",
    "3" = "0",
    "4" = "1",
    "5" = "1"
  )
r3$White_agree_prct <- as.numeric(r3$White_agree_prct)

r3$Black_agree_prct <- r3$Black_agree %>%
  recode(
    "1" = "0",
    "2" = "0",
    "3" = "0",
    "4" = "1",
    "5" = "1"
  )
r3$Black_agree_prct <- as.numeric(r3$Black_agree_prct)

r3$Hispanic_agree_prct <- r3$Hispanic_agree %>%
  recode(
    "1" = "0",
    "2" = "0",
    "3" = "0",
    "4" = "1",
    "5" = "1"
  )
r3$Hispanic_agree_prct <- as.numeric(r3$Hispanic_agree_prct)

r3$Asian_agree_prct <- r3$Asian_agree %>%
  recode(
    "1" = "0",
    "2" = "0",
    "3" = "0",
    "4" = "1",
    "5" = "1"
  )
r3$Asian_agree_prct <- as.numeric(r3$Asian_agree_prct)

r3$MENA_agree_prct <- r3$MENA_agree %>%
  recode(
    "1" = "0",
    "2" = "0",
    "3" = "0",
    "4" = "1",
    "5" = "1"
  )
r3$MENA_agree_prct <- as.numeric(r3$MENA_agree_prct)

## character version of indicator variables for creating tables
r3$White_agree_bin <- as.character(r3$White_agree) %>%
  recode(
    "5" = "Agree",
    "4" = "Agree",
    "3" = "Not agree",
    "2" = "Not agree",
    "1" = "Not agree"
  )
freq(r3$White_agree_bin)
r3$Black_agree_bin <- as.character(r3$Black_agree) %>%
  recode(
    "5" = "Agree",
    "4" = "Agree",
    "3" = "Not agree",
    "2" = "Not agree",
    "1" = "Not agree"
  )
freq(r3$Black_agree_bin)
r3$Hispanic_agree_bin <- as.character(r3$Hispanic_agree) %>%
  recode(
    "5" = "Agree",
    "4" = "Agree",
    "3" = "Not agree",
    "2" = "Not agree",
    "1" = "Not agree"
  )
freq(r3$Hispanic_agree_bin)
r3$Asian_agree_bin <- as.character(r3$Asian_agree) %>%
  recode(
    "5" = "Agree",
    "4" = "Agree",
    "3" = "Not agree",
    "2" = "Not agree",
    "1" = "Not agree"
  )
freq(r3$Asian_agree_bin)
r3$MENA_agree_bin <- as.character(r3$MENA_agree) %>%
  recode(
    "5" = "Agree",
    "4" = "Agree",
    "3" = "Not agree",
    "2" = "Not agree",
    "1" = "Not agree"
  )
freq(r3$MENA_agree_bin)

## subsets
# four respondent race subsets
r3_White <- subset(r3, resrace=="Wht")
r3_Black <- subset(r3, resrace=="Blk")
r3_Hisp <- subset(r3, resrace=="Hisp")
r3_Asian <- subset(r3, resrace=="Asn")

r3$White_only <- ifelse(r3$White_agree - r3$Black_agree > 1 & r3$White_agree - r3$Hispanic_agree > 1 & r3$White_agree - r3$Asian_agree > 1 & r3$White_agree - r3$MENA_agree > 1, 1, 0)
freq(r3$White_only)
r3$Black_only <- ifelse(r3$Black_agree - r3$White_agree > 1 & r3$Black_agree - r3$Hispanic_agree > 1 & r3$Black_agree - r3$Asian_agree > 1 & r3$Black_agree - r3$MENA_agree > 1, 1, 0)
freq(r3$Black_only)
r3$Hispanic_only <- ifelse(r3$Hispanic_agree - r3$White_agree > 1 & r3$Hispanic_agree - r3$Black_agree > 1 & r3$Hispanic_agree - r3$Asian_agree > 1 & r3$Hispanic_agree - r3$MENA_agree > 1, 1, 0)
freq(r3$Hispanic_only)
r3$Asian_only <- ifelse(r3$Asian_agree - r3$White_agree > 1 & r3$Asian_agree - r3$Black_agree > 1 & r3$Asian_agree - r3$Hispanic_agree > 1 & r3$Asian_agree - r3$MENA_agree > 1, 1, 0)
freq(r3$Asian_only)
r3$MENA_only <- ifelse(r3$MENA_agree - r3$White_agree > 1 & r3$MENA_agree - r3$Black_agree > 1 & r3$MENA_agree - r3$Hispanic_agree > 1 & r3$MENA_agree - r3$Asian_only > 1, 1, 0)
freq(r3$MENA_only)


# Tables----
## Because the tables create the figures, create the tables first
## Find code for figures at the end of the script.

# Tables for Main Paper----

## Table 1
r3_stu <- r3 %>%
  filter(Sample == "Student")
r3_dyn <- r3 %>%
  filter(Sample == "Dynata")

### REPS (student) Sample
r3_stu$rYOB <- NA
for (i in 1:nrow(r3_stu)){
  r3_stu$rYOB[i] <- 2006 - r3_stu$Q4_birthyear[i]
}
r3_stu$r_age <- NA
for (i in 1:nrow(r3_stu)){
  r3_stu$r_age[i] <- 2022 - r3_stu$rYOB[i]
}
r3_stu$r_age <- as.numeric(r3_stu$r_age)
r3_stu$r_age_cat[r3_stu$r_age < 30] <- "under 30"
r3_stu$r_age_cat[r3_stu$r_age >= 30 & r3_stu$r_age < 44] <- "30 to 44"
r3_stu$r_age_cat[r3_stu$r_age >= 45 & r3_stu$r_age < 64] <- "45 to 64"
r3_stu$r_age_cat[r3_stu$r_age >= 65] <- "65 and older"
freq(r3_stu$r_age_cat)

r3_stu$Q3_race_ethnic <- as.character(r3_stu$Q3_race_ethnic)
r3_stu$resrace <- r3_stu$Q3_race_ethnic %>%
  na_if("") %>%
  recode(
    "1" = "Asian or Asian American",
    "2" = "Black or African American",
    "3" = "Hispanic or Latino",
    "4" = "Middle Eastern",
    "5" = "Native American",
    "6" = "White",
    "7" = "Another racial or ethnic group"
  )
freq(r3_stu$resrace)

r3_stu$Q5_gender <- as.character(r3_stu$Q5_gender)
r3_stu$resgender <- r3_stu$Q5_gender %>%
  na_if("") %>%
  recode(
    "1" = "Female",
    "2" = "Male",
    "3" = "Other"
  )
freq(r3_stu$resgender)

r3_stu$Q26_level_edu <- as.character(r3_stu$Q26_level_edu)
r3_stu$reseduc <- r3_stu$Q26_level_edu %>%
  na_if("") %>%
  recode(
    "1" = "Less than high school",
    "2" = "High school",
    "3" = "Some college",
    "4" = "Bachelor's degree or higher"
  )
freq(r3_stu$reseduc)

r3_stu$Q19_partyID <- as.character(r3_stu$Q19_partyID)
r3_stu$resparty <- r3_stu$Q19_partyID %>%
  na_if("5") %>%
  recode(
    "1" = "Dem",
    "2" = "Rep",
    "3" = "Ind",
    "4" = "Some other party"
  )
freq(r3_stu$resparty)

### Dynata sample
# not all Dynata respondents completed every task, so subset for unique respondents
dyn_unique <- r3_dyn[!duplicated(r3_dyn$Respondent_ID), ]

# convert birth year selection to actual year, then find age
dyn_unique$rYOB <- NA
for (i in 1:nrow(dyn_unique)){
  dyn_unique$rYOB[i] <- 2006 - dyn_unique$Q4_birthyear[i]
}
dyn_unique$r_age <- NA
for (i in 1:nrow(dyn_unique)){
  dyn_unique$r_age[i] <- 2022 - dyn_unique$rYOB[i]
}
dyn_unique$r_age <- as.numeric(dyn_unique$r_age)
dyn_unique$r_age_cat[dyn_unique$r_age < 30] <- "under 30"
dyn_unique$r_age_cat[dyn_unique$r_age >= 30 & dyn_unique$r_age < 44] <- "30 to 44"
dyn_unique$r_age_cat[dyn_unique$r_age >= 45 & dyn_unique$r_age < 64] <- "45 to 64"
dyn_unique$r_age_cat[dyn_unique$r_age >= 65] <- "65 and older"
freq(dyn_unique$r_age_cat)

dyn_unique$Q3_race_ethnic <- as.character(dyn_unique$Q3_race_ethnic)
dyn_unique$resrace <- dyn_unique$Q3_race_ethnic %>%
  na_if("") %>%
  recode(
    "1" = "Asian or Asian American",
    "2" = "Black or African American",
    "3" = "Hispanic or Latino",
    "4" = "Middle Eastern",
    "5" = "Native American",
    "6" = "White",
    "7" = "Another racial or ethnic group"
  )
freq(dyn_unique$resrace)

dyn_unique$Q5_gender <- as.character(dyn_unique$Q5_gender)
dyn_unique$resgender <- dyn_unique$Q5_gender %>%
  na_if("") %>%
  recode(
    "1" = "Female",
    "2" = "Male",
    "3" = "Other"
  )
freq(dyn_unique$resgender)

dyn_unique$Q26_level_edu <- as.character(dyn_unique$Q26_level_edu)
dyn_unique$reseduc <- dyn_unique$Q26_level_edu %>%
  na_if("") %>%
  recode(
    "1" = "Less than high school",
    "2" = "High school",
    "3" = "Some college",
    "4" = "Bachelor's degree or higher"
  )
freq(dyn_unique$reseduc)

dyn_unique$Q19_partyID <- as.character(dyn_unique$Q19_partyID)
dyn_unique$resparty <- dyn_unique$Q19_partyID %>%
  na_if("5") %>%
  recode(
    "1" = "Dem",
    "2" = "Rep",
    "3" = "Ind",
    "4" = "Some other party"
  )
freq(dyn_unique$resparty)

### Both samples stats + table
data1 <- data.frame("Category" = character(),    # Create empty data frame
                    "Student Sample" = character(),
                    "Dynata Sample" = character(),
                    stringsAsFactors = FALSE,
                    check.names = FALSE)

data1[1, ] <- list("Age", "", "")
data1[2, ] <- list("Under 30", 97.0, 29.5)
data1[3, ] <- list("30-44", 2.7, 33.2)
data1[4, ] <- list("45-64", 0.3, 23.1)
data1[5, ] <- list("65+", "", 14.2)

data1[6, ] <- list("Race", "", "")
data1[7, ] <- list("Asian or Asian American", 27.8, 18.5)
data1[8, ] <- list("Black or African American", 5.0, 31.7)
data1[9, ] <- list("Hispanic or Latino", 32.5, 18.4)
data1[10, ] <- list("Middle Eastern", 5.7, "")
data1[11, ] <- list("Native American", 0.8, "")
data1[12, ] <- list("White", 24.2, 31.4)
data1[13, ] <- list("Another racial or ethnic group", 4.0, "")

data1[14, ] <- list("Gender", "", "")
data1[15, ] <- list("Male", 31.3, 42.1)
data1[16, ] <- list("Female", 67.7, 57.5)
data1[17, ] <- list("Other", 1.0, 0.4)

data1[18, ] <- list("Education Level", "", "")
data1[19, ] <- list("Less than high school", "", 4.5)
data1[20, ] <- list("High school", 21.1, 34.5)
data1[21, ] <- list("Some college", 71.3, 30.7)
data1[22, ] <- list("Bachelor's degree or higher", 7.6, 30.3)

data1[23, ] <- list("Party ID", "", "")
data1[24, ] <- list("Democratic", 69.1, 51.1)
data1[25, ] <- list("Republican", 7.8, 22.8)
data1[26, ] <- list("Independent", 20.2, 24.5)
data1[27, ] <- list("Other", 3.0, 1.5)

print(xtable(data1), include.rownames=FALSE)

### Other info

# Dynata survey was fielded from May 24th through July 8th, 2022
# 1719 observations that provided a conjoint response
# 540 White 545 Black 318 Asian and 316 Hispanic

# student sample had 1016 observations that provided a conjoint response
# drawn from UCLA, UC Riverside, UC Irvine, Howard University

## Table 2 is a summary of profile attributes and levels and is not reproducible in R

## Table 3
library(plyr)
table_3 <- r3 %>%
  filter(!is.na(resrace)) %>%
  group_by(resrace) %>%
  dplyr::summarize(mean_White = mean(White_agree, na.rm = T),
                   mean_Black = mean(Black_agree, na.rm = T),
                   mean_Hispanic = mean(Hispanic_agree, na.rm = T),
                   mean_Asian = mean(Asian_agree, na.rm = T),
                   mean_MENA = mean(MENA_agree, na.rm = T))
### rearrange row order in Overleaf

## Table 4
### first row
ancW_selfW <- subset(r3, ancestryWhite == 1 & selfIDWhite == 1)
freq(ancW_selfW$White_agree_prct)
ancW_selfB <- subset(r3, ancestryWhite == 1 & selfIDBlack == 1)
freq(ancW_selfB$White_agree_prct)
freq(ancW_selfB$Black_agree_prct)
ancW_selfH <- subset(r3, ancestryWhite == 1 & selfIDHispanic== 1)
freq(ancW_selfH$White_agree_prct)
freq(ancW_selfH$Hispanic_agree_prct)
ancW_selfA <- subset(r3, ancestryWhite == 1 & selfIDAsian == 1)
freq(ancW_selfA$White_agree_prct)
freq(ancW_selfA$Asian_agree_prct)
ancW_selfM <- subset(r3, ancestryWhite == 1 & selfIDMENA == 1)
freq(ancW_selfM$White_agree_prct)
freq(ancW_selfM$MENA_agree_prct)

### second row
ancB_selfW <- subset(r3, ancestryBlack == 1 & selfIDWhite == 1)
freq(ancB_selfW$Black_agree_prct)
freq(ancB_selfW$White_agree_prct)
ancB_selfB <- subset(r3, ancestryBlack == 1 & selfIDBlack == 1)
freq(ancB_selfB$Black_agree_prct)
ancB_selfH <- subset(r3, ancestryBlack == 1 & selfIDHispanic== 1)
freq(ancB_selfH$Black_agree_prct)
freq(ancB_selfH$Hispanic_agree_prct)
ancB_selfA <- subset(r3, ancestryBlack == 1 & selfIDAsian == 1)
freq(ancB_selfA$Black_agree_prct)
freq(ancB_selfA$Asian_agree_prct)
ancB_selfM <- subset(r3, ancestryBlack == 1 & selfIDMENA == 1)
freq(ancB_selfM$Black_agree_prct)
freq(ancB_selfM$MENA_agree_prct)

### third row
ancH_selfW <- subset(r3, ancestryHispanic == 1 & selfIDWhite == 1)
freq(ancH_selfW$Hispanic_agree_prct)
freq(ancH_selfW$White_agree_prct)
ancH_selfB <- subset(r3, ancestryHispanic == 1 & selfIDBlack == 1)
freq(ancH_selfB$Hispanic_agree_prct)
freq(ancH_selfB$Black_agree_prct)
ancH_selfH <- subset(r3, ancestryHispanic == 1 & selfIDHispanic== 1)
freq(ancH_selfH$Hispanic_agree_prct)
ancH_selfA <- subset(r3, ancestryHispanic == 1 & selfIDAsian == 1)
freq(ancH_selfA$Hispanic_agree_prct)
freq(ancH_selfA$Asian_agree_prct)
ancH_selfM <- subset(r3, ancestryHispanic == 1 & selfIDMENA == 1)
freq(ancH_selfM$Hispanic_agree_prct)
freq(ancH_selfM$MENA_agree_prct)

### fourth row
ancA_selfW <- subset(r3, ancestryAsian == 1 & selfIDWhite == 1)
freq(ancA_selfW$Asian_agree_prct)
freq(ancA_selfW$White_agree_prct)
ancA_selfB <- subset(r3, ancestryAsian == 1 & selfIDBlack == 1)
freq(ancA_selfB$Asian_agree_prct)
freq(ancA_selfB$Black_agree_prct)
ancA_selfH <- subset(r3, ancestryAsian == 1 & selfIDHispanic== 1)
freq(ancA_selfH$Asian_agree_prct)
freq(ancA_selfH$Hispanic_agree_prct)
ancA_selfA <- subset(r3, ancestryAsian == 1 & selfIDAsian == 1)
freq(ancA_selfA$Asian_agree_prct)
ancA_selfM <- subset(r3, ancestryAsian == 1 & selfIDMENA == 1)
freq(ancA_selfM$Asian_agree_prct)
freq(ancA_selfM$MENA_agree_prct)

### fifth row
ancM_selfW <- subset(r3, ancestryMENA == 1 & selfIDWhite == 1)
freq(ancM_selfW$MENA_agree_prct)
freq(ancM_selfW$White_agree_prct)
ancM_selfB <- subset(r3, ancestryMENA == 1 & selfIDBlack == 1)
freq(ancM_selfB$MENA_agree_prct)
freq(ancM_selfB$Black_agree_prct)
ancM_selfH <- subset(r3, ancestryMENA == 1 & selfIDHispanic== 1)
freq(ancM_selfH$MENA_agree_prct)
freq(ancM_selfH$Hispanic_agree_prct)
ancM_selfA <- subset(r3, ancestryMENA == 1 & selfIDAsian == 1)
freq(ancM_selfA$MENA_agree_prct)
freq(ancM_selfA$Asian_agree_prct)
ancM_selfM <- subset(r3, ancestryMENA == 1 & selfIDMENA == 1)
freq(ancM_selfM$MENA_agree_prct)

### The code for Figure 1 and Figure 2 can be found at the end of this replication file

# Appendix Tables----


## Table A1
r3_agree <- select(r3, c("White_agree","Black_agree","Hispanic_agree","Asian_agree","MENA_agree"))
cor.plot(r3_agree,numbers=TRUE, main="Corr's between agreement a profile belongs to a racial group", labels = c("White","Black","Hispanic","Asian","MENA"), scale = T, stars = T)


## Table A2
r3_agreements <- r3 %>%
  dplyr::select(X.1,White_agree_prct, White_agree_bin, Black_agree_prct, Black_agree_bin, Hispanic_agree_prct, Hispanic_agree_bin, Asian_agree_prct, Asian_agree_bin, MENA_agree_prct, MENA_agree_bin)

White_agree_df <- subset(r3_agreements, White_agree_prct == 1, select = c(White_agree_prct, Black_agree_prct, Hispanic_agree_prct, Asian_agree_prct, MENA_agree_prct))
agree_W_others <- as.data.frame(colMeans(White_agree_df, na.rm = T))
agree_W_others$row_numbers = c(1:5)
Black_agree_df <- subset(r3_agreements, Black_agree_prct == 1, select = c(White_agree_prct, Black_agree_prct, Hispanic_agree_prct, Asian_agree_prct, MENA_agree_prct))
agree_B_others <- as.data.frame(colMeans(Black_agree_df, na.rm = T))
agree_B_others$row_numbers = c(1:5)
Hispanic_agree_df <- subset(r3_agreements, Hispanic_agree_prct == 1, select = c(White_agree_prct, Black_agree_prct, Hispanic_agree_prct, Asian_agree_prct, MENA_agree_prct))
agree_H_others <- as.data.frame(colMeans(Hispanic_agree_df, na.rm = T))
agree_H_others$row_numbers = c(1:5)
Asian_agree_df <- subset(r3_agreements, Asian_agree_prct == 1, select = c(White_agree_prct, Black_agree_prct, Hispanic_agree_prct, Asian_agree_prct, MENA_agree_prct))
agree_A_others <- as.data.frame(colMeans(Asian_agree_df, na.rm = T))
agree_A_others$row_numbers = c(1:5)
MENA_agree_df <- subset(r3_agreements, MENA_agree_prct == 1, select = c(White_agree_prct, Black_agree_prct, Hispanic_agree_prct, Asian_agree_prct, MENA_agree_prct))
agree_M_others <- as.data.frame(colMeans(MENA_agree_df, na.rm = T))
agree_M_others$row_numbers = c(1:5)

# multiply every row by 100, round to two decimal points
# change column names

agree_1 <- left_join(agree_W_others, agree_B_others, by = "row_numbers")
agree_2 <- left_join(agree_1, agree_H_others, by = "row_numbers")
agree_3 <- left_join(agree_2, agree_A_others, by = "row_numbers")
agree_all <- left_join(agree_3, agree_M_others, by = "row_numbers")
agree_all <- agree_all %>% dplyr::select(-row_numbers)
agree_all <- agree_all * 100
agree_all2 <- round(agree_all, digits = 2)
colnames(agree_all2) <- c("Agree White","Agree Black", "Agree Hispanic", "Agree Asian", "Agree MENA")
rownames(agree_all2) <- c("Agree White","Agree Black", "Agree Hispanic", "Agree Asian", "Agree MENA")
# transpose
agree_all3 <- data.frame(t(agree_all2))
colnames(agree_all3) <- c("Agree White","Agree Black", "Agree Hispanic", "Agree Asian", "Agree MENA")

table_agreements <- print(xtable(agree_all3, caption = "Agreement that a profile belongs to a racial category given agreement that it belongs to another racial category"), caption.placement = 'top', include.rownames=TRUE)

## Table A3
WA_comb <- cj(r3, `White_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
BA_comb <- cj(r3, `Black_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
HA_comb <- cj(r3, `Hispanic_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
AA_comb <- cj(r3, `Asian_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
MA_comb <- cj(r3, `MENA_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

detach("package:plyr", unload=TRUE)
WA_comb_OL <- WA_comb[,c('feature','level','estimate','std.error','p')]
WA_comb_OL <- rename(WA_comb_OL, Variable = feature)
WA_comb_OL <- rename(WA_comb_OL, Level = level)
WA_comb_OL <- rename(WA_comb_OL, Estimate = estimate)
WA_comb_OL <- rename(WA_comb_OL, SE = std.error)
BA_comb_OL <- BA_comb[,c('feature','level','estimate','std.error','p')]
BA_comb_OL <- rename(BA_comb_OL, Variable = feature)
BA_comb_OL <- rename(BA_comb_OL, Level = level)
BA_comb_OL <- rename(BA_comb_OL, Estimate = estimate)
BA_comb_OL <- rename(BA_comb_OL, SE = std.error)
HA_comb_OL <- HA_comb[,c('feature','level','estimate','std.error','p')]
HA_comb_OL <- rename(HA_comb_OL, Variable = feature)
HA_comb_OL <- rename(HA_comb_OL, Level = level)
HA_comb_OL <- rename(HA_comb_OL, Estimate = estimate)
HA_comb_OL <- rename(HA_comb_OL, SE = std.error)
AA_comb_OL <- AA_comb[,c('feature','level','estimate','std.error','p')]
AA_comb_OL <- rename(AA_comb_OL, Variable = feature)
AA_comb_OL <- rename(AA_comb_OL, Level = level)
AA_comb_OL <- rename(AA_comb_OL, Estimate = estimate)
AA_comb_OL <- rename(AA_comb_OL, SE = std.error)
MA_comb_OL <- MA_comb[,c('feature','level','estimate','std.error','p')]
MA_comb_OL <- rename(MA_comb_OL, Variable = feature)
MA_comb_OL <- rename(MA_comb_OL, Level = level)
MA_comb_OL <- rename(MA_comb_OL, Estimate = estimate)
MA_comb_OL <- rename(MA_comb_OL, SE = std.error)

WA_comb_OL$Level <- as.character(WA_comb_OL$Level)
for(i in 1:nrow(WA_comb_OL)){
  if(isTRUE(grepl("Wh",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "White"
  }
  else if(isTRUE(grepl("Bl",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "Black/African American"
  }
  else if(isTRUE(grepl("As",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "Asian/Asian American"
  }
  else if(isTRUE(grepl("Hisp",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "Hispanic/Latino(a/x)"
  }
  else if(isTRUE(grepl("MENA",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "Middle Eastern or North African"
  }
  else if(isTRUE(grepl("Don't",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "No"
  }
  else if(isTRUE(grepl("Speak",as.character(WA_comb_OL$Level[i])))){
    WA_comb_OL$Level[i] <- "Yes"
  }
  else{
    next
  }
}
i = 0
WA_comb_OL$Variable <- as.character(WA_comb_OL$Variable)
for(i in 1:nrow(WA_comb_OL)){
  if(isTRUE(WA_comb_OL$Variable[i]=="gender")){
    WA_comb_OL$Variable[i] <- "Image gender"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="ethnicity")){
    WA_comb_OL$Variable[i] <- "Image race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="skin_tone")){
    WA_comb_OL$Variable[i] <- "Skin color"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="birthparents_ethnicID")){
    WA_comb_OL$Variable[i] <- "Parents' background"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="age")){
    WA_comb_OL$Variable[i] <- "Age (years)"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="selfID_ethnic")){
    WA_comb_OL$Variable[i] <- "Self-identification"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="occup_status")){
    WA_comb_OL$Variable[i] <- "Parents' occupational status"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="english")){
    WA_comb_OL$Variable[i] <- "Speaks English at home"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="resrace")){
    WA_comb_OL$Variable[i] <- "Race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="educ")){
    WA_comb_OL$Variable[i] <- "Educational attainment"
  }
  else if(isTRUE(WA_comb_OL$Variable[i]=="party")){
    WA_comb_OL$Variable[i] <- "Party identification"
  }
  else{
    next
  }
}

gen_models <- list(BA_comb_OL, HA_comb_OL, AA_comb_OL, MA_comb_OL)

for(x in 1:length(gen_models)){
  gen_models[[x]]$Level <- as.character(gen_models[[x]]$Level)
  for(i in 1:nrow(gen_models[[x]])){
    if(isTRUE(grepl("Wh",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models[[x]]$Variable <- as.character(gen_models[[x]]$Variable)
  for(j in 1:nrow(gen_models[[x]])){
    if(isTRUE(gen_models[[x]]$Variable[j]=="gender")){
      gen_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="ethnicity")){
      gen_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="skin_tone")){
      gen_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="age")){
      gen_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="occup_status")){
      gen_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="english")){
      gen_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="resrace")){
      gen_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="educ")){
      gen_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="party")){
      gen_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
BA_comb_OL <- gen_models[[1]]
HA_comb_OL <- gen_models[[2]]
AA_comb_OL <- gen_models[[3]]
MA_comb_OL <- gen_models[[4]]

### Add stars
WA_comb_OL$stars <- symnum(WA_comb_OL$p, 
                           symbols   = c("***","**","*","+", ""),
                           cutpoints = c(0, .001,.01,.05, .1, 1),
                           corr      = FALSE
)
WA_comb_OL$stars[WA_comb_OL$stars == ""] <- NA
WA_comb_OL$stars[WA_comb_OL$stars == "?"] <- NA
WA_comb_OL = subset(WA_comb_OL, select = -c(Variable, p))
WA_comb_OL <- WA_comb_OL %>% 
  mutate(ID = row_number())
WA_comb_OL <- WA_comb_OL[c("ID", "Level", "Estimate", "stars", "SE")]
WA_comb_OL <- WA_comb_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_comb_OL <- WA_comb_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_comb_OL$stars <- symnum(BA_comb_OL$p, 
                           symbols   = c("***","**","*","+", ""),
                           cutpoints = c(0, .001,.01,.05, .1, 1),
                           corr      = FALSE
)
BA_comb_OL$stars[BA_comb_OL$stars == ""] <- NA
BA_comb_OL$stars[BA_comb_OL$stars == "?"] <- NA
BA_comb_OL = subset(BA_comb_OL, select = -c(Variable, p))
BA_comb_OL <- BA_comb_OL %>% 
  mutate(ID = row_number())
BA_comb_OL <- BA_comb_OL[c("ID", "Level", "Estimate", "stars", "SE")]
BA_comb_OL <- BA_comb_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_comb_OL <- BA_comb_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_comb_OL$stars <- symnum(HA_comb_OL$p, 
                           symbols   = c("***","**","*","+", ""),
                           cutpoints = c(0, .001,.01,.05, .1, 1),
                           corr      = FALSE
)
HA_comb_OL$stars[HA_comb_OL$stars == ""] <- NA
HA_comb_OL$stars[HA_comb_OL$stars == "?"] <- NA
HA_comb_OL = subset(HA_comb_OL, select = -c(Variable, p))
HA_comb_OL <- HA_comb_OL %>% 
  mutate(ID = row_number())
HA_comb_OL <- HA_comb_OL[c("ID", "Level", "Estimate", "stars", "SE")]
HA_comb_OL <- HA_comb_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_comb_OL <- HA_comb_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_comb_OL$stars <- symnum(AA_comb_OL$p, 
                           symbols   = c("***","**","*","+", ""),
                           cutpoints = c(0, .001,.01,.05, .1, 1),
                           corr      = FALSE
)
AA_comb_OL$stars[AA_comb_OL$stars == ""] <- NA
AA_comb_OL$stars[AA_comb_OL$stars == "?"] <- NA
AA_comb_OL = subset(AA_comb_OL, select = -c(Variable, p))
AA_comb_OL <- AA_comb_OL %>% 
  mutate(ID = row_number())
AA_comb_OL <- AA_comb_OL[c("ID", "Level", "Estimate", "stars", "SE")]
AA_comb_OL <- AA_comb_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_comb_OL <- AA_comb_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_comb_OL$stars <- symnum(MA_comb_OL$p, 
                           symbols   = c("***","**","*","+", ""),
                           cutpoints = c(0, .001,.01,.05, .1, 1),
                           corr      = FALSE
)
MA_comb_OL$stars[MA_comb_OL$stars == ""] <- NA
MA_comb_OL$stars[MA_comb_OL$stars == "?"] <- NA
MA_comb_OL = subset(MA_comb_OL, select = -c(Variable, p))
MA_comb_OL <- MA_comb_OL %>% 
  mutate(ID = row_number())
MA_comb_OL <- MA_comb_OL[c("ID", "Level", "Estimate", "stars", "SE")]
MA_comb_OL <- MA_comb_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_comb_OL <- MA_comb_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

### Put all five tables into one
library(plyr)
tablea3_cleaner2 <- join(WA_comb_OL,BA_comb_OL,by="ID")
tablea3_cleaner2 <- join(tablea3_cleaner2,HA_comb_OL,by="ID")
tablea3_cleaner2 <- join(tablea3_cleaner2,AA_comb_OL,by="ID")
tablea3_cleaner2 <- join(tablea3_cleaner2,MA_comb_OL,by="ID")
tablea3_cleaner3 = subset(tablea3_cleaner2, select = -c(1,5,8,11,14))
View(tablea3_cleaner3)
### rearrange row order in Excel

## Table A4
WA_comb_W <- cj(r3_White, `White_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
BA_comb_W <- cj(r3_White, `Black_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
HA_comb_W <- cj(r3_White, `Hispanic_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
AA_comb_W <- cj(r3_White, `Asian_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
MA_comb_W <- cj(r3_White, `MENA_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")

detach("package:plyr", unload=TRUE)
WA_comb_OL_W <- WA_comb_W[,c('feature','level','estimate','std.error','p')]
WA_comb_OL_W <- rename(WA_comb_OL_W, Variable = feature)
WA_comb_OL_W <- rename(WA_comb_OL_W, Level = level)
WA_comb_OL_W <- rename(WA_comb_OL_W, Estimate = estimate)
WA_comb_OL_W <- rename(WA_comb_OL_W, SE = std.error)
BA_comb_OL_W <- BA_comb_W[,c('feature','level','estimate','std.error','p')]
BA_comb_OL_W <- rename(BA_comb_OL_W, Variable = feature)
BA_comb_OL_W <- rename(BA_comb_OL_W, Level = level)
BA_comb_OL_W <- rename(BA_comb_OL_W, Estimate = estimate)
BA_comb_OL_W <- rename(BA_comb_OL_W, SE = std.error)
HA_comb_OL_W <- HA_comb_W[,c('feature','level','estimate','std.error','p')]
HA_comb_OL_W <- rename(HA_comb_OL_W, Variable = feature)
HA_comb_OL_W <- rename(HA_comb_OL_W, Level = level)
HA_comb_OL_W <- rename(HA_comb_OL_W, Estimate = estimate)
HA_comb_OL_W <- rename(HA_comb_OL_W, SE = std.error)
AA_comb_OL_W <- AA_comb_W[,c('feature','level','estimate','std.error','p')]
AA_comb_OL_W <- rename(AA_comb_OL_W, Variable = feature)
AA_comb_OL_W <- rename(AA_comb_OL_W, Level = level)
AA_comb_OL_W <- rename(AA_comb_OL_W, Estimate = estimate)
AA_comb_OL_W <- rename(AA_comb_OL_W, SE = std.error)
MA_comb_OL_W <- MA_comb_W[,c('feature','level','estimate','std.error','p')]
MA_comb_OL_W <- rename(MA_comb_OL_W, Variable = feature)
MA_comb_OL_W <- rename(MA_comb_OL_W, Level = level)
MA_comb_OL_W <- rename(MA_comb_OL_W, Estimate = estimate)
MA_comb_OL_W <- rename(MA_comb_OL_W, SE = std.error)

WA_comb_OL_W$Level <- as.character(WA_comb_OL_W$Level)
for(i in 1:nrow(WA_comb_OL_W)){
  if(isTRUE(grepl("Wh",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "White"
  }
  else if(isTRUE(grepl("Bl",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "Black/African American"
  }
  else if(isTRUE(grepl("As",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "Asian/Asian American"
  }
  else if(isTRUE(grepl("Hisp",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "Hispanic/Latino(a/x)"
  }
  else if(isTRUE(grepl("MENA",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "Middle Eastern or North African"
  }
  else if(isTRUE(grepl("Don't",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "No"
  }
  else if(isTRUE(grepl("Speak",as.character(WA_comb_OL_W$Level[i])))){
    WA_comb_OL_W$Level[i] <- "Yes"
  }
  else{
    next
  }
}
i = 0
WA_comb_OL_W$Variable <- as.character(WA_comb_OL_W$Variable)
for(i in 1:nrow(WA_comb_OL_W)){
  if(isTRUE(WA_comb_OL_W$Variable[i]=="gender")){
    WA_comb_OL_W$Variable[i] <- "Image gender"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="ethnicity")){
    WA_comb_OL_W$Variable[i] <- "Image race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="skin_tone")){
    WA_comb_OL_W$Variable[i] <- "Skin color"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="birthparents_ethnicID")){
    WA_comb_OL_W$Variable[i] <- "Parents' background"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="age")){
    WA_comb_OL_W$Variable[i] <- "Age (years)"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="selfID_ethnic")){
    WA_comb_OL_W$Variable[i] <- "Self-identification"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="occup_status")){
    WA_comb_OL_W$Variable[i] <- "Parents' occupational status"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="english")){
    WA_comb_OL_W$Variable[i] <- "Speaks English at home"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="resrace")){
    WA_comb_OL_W$Variable[i] <- "Race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="educ")){
    WA_comb_OL_W$Variable[i] <- "Educational attainment"
  }
  else if(isTRUE(WA_comb_OL_W$Variable[i]=="party")){
    WA_comb_OL_W$Variable[i] <- "Party identification"
  }
  else{
    next
  }
}

gen_models_W <- list(BA_comb_OL_W, HA_comb_OL_W, AA_comb_OL_W, MA_comb_OL_W)

for(x in 1:length(gen_models_W)){
  gen_models_W[[x]]$Level <- as.character(gen_models_W[[x]]$Level)
  for(i in 1:nrow(gen_models_W[[x]])){
    if(isTRUE(grepl("Wh",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models_W[[x]]$Level[i]))){
      gen_models_W[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models_W[[x]]$Variable <- as.character(gen_models_W[[x]]$Variable)
  for(j in 1:nrow(gen_models_W[[x]])){
    if(isTRUE(gen_models_W[[x]]$Variable[j]=="gender")){
      gen_models_W[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="ethnicity")){
      gen_models_W[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="skin_tone")){
      gen_models_W[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models_W[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="age")){
      gen_models_W[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models_W[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="occup_status")){
      gen_models_W[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="english")){
      gen_models_W[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="resrace")){
      gen_models_W[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="educ")){
      gen_models_W[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models_W[[x]]$Variable[j]=="party")){
      gen_models_W[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}

BA_comb_OL_W <- gen_models_W[[1]]
HA_comb_OL_W <- gen_models_W[[2]]
AA_comb_OL_W <- gen_models_W[[3]]
MA_comb_OL_W <- gen_models_W[[4]]

WA_comb_OL_W$stars <- symnum(WA_comb_OL_W$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_comb_OL_W$stars[WA_comb_OL_W$stars == ""] <- NA
WA_comb_OL_W$stars[WA_comb_OL_W$stars == "?"] <- NA
WA_comb_OL_W = subset(WA_comb_OL_W, select = -c(Variable, p))
WA_comb_OL_W <- WA_comb_OL_W %>% 
  mutate(ID = row_number())
WA_comb_OL_W <- WA_comb_OL_W[c("ID", "Level", "Estimate", "stars", "SE")]
WA_comb_OL_W <- WA_comb_OL_W %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_comb_OL_W <- WA_comb_OL_W %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_comb_OL_W$stars <- symnum(BA_comb_OL_W$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_comb_OL_W$stars[BA_comb_OL_W$stars == ""] <- NA
BA_comb_OL_W$stars[BA_comb_OL_W$stars == "?"] <- NA
BA_comb_OL_W = subset(BA_comb_OL_W, select = -c(Variable, p))
BA_comb_OL_W <- BA_comb_OL_W %>% 
  mutate(ID = row_number())
BA_comb_OL_W <- BA_comb_OL_W[c("ID", "Level", "Estimate", "stars", "SE")]
BA_comb_OL_W <- BA_comb_OL_W %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_comb_OL_W <- BA_comb_OL_W %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_comb_OL_W$stars <- symnum(HA_comb_OL_W$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_comb_OL_W$stars[HA_comb_OL_W$stars == ""] <- NA
HA_comb_OL_W$stars[HA_comb_OL_W$stars == "?"] <- NA
HA_comb_OL_W = subset(HA_comb_OL_W, select = -c(Variable, p))
HA_comb_OL_W <- HA_comb_OL_W %>% 
  mutate(ID = row_number())
HA_comb_OL_W <- HA_comb_OL_W[c("ID", "Level", "Estimate", "stars", "SE")]
HA_comb_OL_W <- HA_comb_OL_W %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_comb_OL_W <- HA_comb_OL_W %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_comb_OL_W$stars <- symnum(AA_comb_OL_W$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_comb_OL_W$stars[AA_comb_OL_W$stars == ""] <- NA
AA_comb_OL_W$stars[AA_comb_OL_W$stars == "?"] <- NA
AA_comb_OL_W = subset(AA_comb_OL_W, select = -c(Variable, p))
AA_comb_OL_W <- AA_comb_OL_W %>% 
  mutate(ID = row_number())
AA_comb_OL_W <- AA_comb_OL_W[c("ID", "Level", "Estimate", "stars", "SE")]
AA_comb_OL_W <- AA_comb_OL_W %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_comb_OL_W <- AA_comb_OL_W %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_comb_OL_W$stars <- symnum(MA_comb_OL_W$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_comb_OL_W$stars[MA_comb_OL_W$stars == ""] <- NA
MA_comb_OL_W$stars[MA_comb_OL_W$stars == "?"] <- NA
MA_comb_OL_W = subset(MA_comb_OL_W, select = -c(Variable, p))
MA_comb_OL_W <- MA_comb_OL_W %>% 
  mutate(ID = row_number())
MA_comb_OL_W <- MA_comb_OL_W[c("ID", "Level", "Estimate", "stars", "SE")]
MA_comb_OL_W <- MA_comb_OL_W %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_comb_OL_W <- MA_comb_OL_W %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

library(plyr)
tablea4_cleaner2 <- join(WA_comb_OL_W,BA_comb_OL_W,by="ID")
tablea4_cleaner2 <- join(tablea4_cleaner2,HA_comb_OL_W,by="ID")
tablea4_cleaner2 <- join(tablea4_cleaner2,AA_comb_OL_W,by="ID")
tablea4_cleaner2 <- join(tablea4_cleaner2,MA_comb_OL_W,by="ID")
tablea4_cleaner3 = subset(tablea4_cleaner2, select = -c(1,5,8,11,14))
View(tablea4_cleaner3)
### rearrange row order in Excel

## Table A5
WA_comb_B <- cj(r3_Black, `White_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
BA_comb_B <- cj(r3_Black, `Black_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
HA_comb_B <- cj(r3_Black, `Hispanic_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
AA_comb_B <- cj(r3_Black, `Asian_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
MA_comb_B <- cj(r3_Black, `MENA_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")

detach("package:plyr", unload=TRUE)
WA_comb_OL_B <- WA_comb_B[,c('feature','level','estimate','std.error','p')]
WA_comb_OL_B <- rename(WA_comb_OL_B, Variable = feature)
WA_comb_OL_B <- rename(WA_comb_OL_B, Level = level)
WA_comb_OL_B <- rename(WA_comb_OL_B, Estimate = estimate)
WA_comb_OL_B <- rename(WA_comb_OL_B, SE = std.error)
BA_comb_OL_B <- BA_comb_B[,c('feature','level','estimate','std.error','p')]
BA_comb_OL_B <- rename(BA_comb_OL_B, Variable = feature)
BA_comb_OL_B <- rename(BA_comb_OL_B, Level = level)
BA_comb_OL_B <- rename(BA_comb_OL_B, Estimate = estimate)
BA_comb_OL_B <- rename(BA_comb_OL_B, SE = std.error)
HA_comb_OL_B <- HA_comb_B[,c('feature','level','estimate','std.error','p')]
HA_comb_OL_B <- rename(HA_comb_OL_B, Variable = feature)
HA_comb_OL_B <- rename(HA_comb_OL_B, Level = level)
HA_comb_OL_B <- rename(HA_comb_OL_B, Estimate = estimate)
HA_comb_OL_B <- rename(HA_comb_OL_B, SE = std.error)
AA_comb_OL_B <- AA_comb_B[,c('feature','level','estimate','std.error','p')]
AA_comb_OL_B <- rename(AA_comb_OL_B, Variable = feature)
AA_comb_OL_B <- rename(AA_comb_OL_B, Level = level)
AA_comb_OL_B <- rename(AA_comb_OL_B, Estimate = estimate)
AA_comb_OL_B <- rename(AA_comb_OL_B, SE = std.error)
MA_comb_OL_B <- MA_comb_B[,c('feature','level','estimate','std.error','p')]
MA_comb_OL_B <- rename(MA_comb_OL_B, Variable = feature)
MA_comb_OL_B <- rename(MA_comb_OL_B, Level = level)
MA_comb_OL_B <- rename(MA_comb_OL_B, Estimate = estimate)
MA_comb_OL_B <- rename(MA_comb_OL_B, SE = std.error)

WA_comb_OL_B$Level <- as.character(WA_comb_OL_B$Level)
for(i in 1:nrow(WA_comb_OL_B)){
  if(isTRUE(grepl("Wh",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "White"
  }
  else if(isTRUE(grepl("Bl",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "Black/African American"
  }
  else if(isTRUE(grepl("As",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "Asian/Asian American"
  }
  else if(isTRUE(grepl("Hisp",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "Hispanic/Latino(a/x)"
  }
  else if(isTRUE(grepl("MENA",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "Middle Eastern or North African"
  }
  else if(isTRUE(grepl("Don't",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "No"
  }
  else if(isTRUE(grepl("Speak",as.character(WA_comb_OL_B$Level[i])))){
    WA_comb_OL_B$Level[i] <- "Yes"
  }
  else{
    next
  }
}
i = 0
WA_comb_OL_B$Variable <- as.character(WA_comb_OL_B$Variable)
for(i in 1:nrow(WA_comb_OL_B)){
  if(isTRUE(WA_comb_OL_B$Variable[i]=="gender")){
    WA_comb_OL_B$Variable[i] <- "Image gender"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="ethnicity")){
    WA_comb_OL_B$Variable[i] <- "Image race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="skin_tone")){
    WA_comb_OL_B$Variable[i] <- "Skin color"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="birthparents_ethnicID")){
    WA_comb_OL_B$Variable[i] <- "Parents' background"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="age")){
    WA_comb_OL_B$Variable[i] <- "Age (years)"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="selfID_ethnic")){
    WA_comb_OL_B$Variable[i] <- "Self-identification"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="occup_status")){
    WA_comb_OL_B$Variable[i] <- "Parents' occupational status"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="english")){
    WA_comb_OL_B$Variable[i] <- "Speaks English at home"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="resrace")){
    WA_comb_OL_B$Variable[i] <- "Race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="educ")){
    WA_comb_OL_B$Variable[i] <- "Educational attainment"
  }
  else if(isTRUE(WA_comb_OL_B$Variable[i]=="party")){
    WA_comb_OL_B$Variable[i] <- "Party identification"
  }
  else{
    next
  }
}

gen_models_B <- list(BA_comb_OL_B, HA_comb_OL_B, AA_comb_OL_B, MA_comb_OL_B)

for(x in 1:length(gen_models_B)){
  gen_models_B[[x]]$Level <- as.character(gen_models_B[[x]]$Level)
  for(i in 1:nrow(gen_models_B[[x]])){
    if(isTRUE(grepl("Wh",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models_B[[x]]$Level[i]))){
      gen_models_B[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models_B[[x]]$Variable <- as.character(gen_models_B[[x]]$Variable)
  for(j in 1:nrow(gen_models_B[[x]])){
    if(isTRUE(gen_models_B[[x]]$Variable[j]=="gender")){
      gen_models_B[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="ethnicity")){
      gen_models_B[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="skin_tone")){
      gen_models_B[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models_B[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="age")){
      gen_models_B[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models_B[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="occup_status")){
      gen_models_B[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="english")){
      gen_models_B[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="resrace")){
      gen_models_B[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="educ")){
      gen_models_B[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models_B[[x]]$Variable[j]=="party")){
      gen_models_B[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
BA_comb_OL_B <- gen_models_B[[1]]
HA_comb_OL_B <- gen_models_B[[2]]
AA_comb_OL_B <- gen_models_B[[3]]
MA_comb_OL_B <- gen_models_B[[4]]


WA_comb_OL_B$stars <- symnum(WA_comb_OL_B$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_comb_OL_B$stars[WA_comb_OL_B$stars == ""] <- NA
WA_comb_OL_B$stars[WA_comb_OL_B$stars == "?"] <- NA
WA_comb_OL_B = subset(WA_comb_OL_B, select = -c(Variable, p))
WA_comb_OL_B <- WA_comb_OL_B %>% 
  mutate(ID = row_number())
WA_comb_OL_B <- WA_comb_OL_B[c("ID", "Level", "Estimate", "stars", "SE")]
WA_comb_OL_B <- WA_comb_OL_B %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_comb_OL_B <- WA_comb_OL_B %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_comb_OL_B$stars <- symnum(BA_comb_OL_B$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_comb_OL_B$stars[BA_comb_OL_B$stars == ""] <- NA
BA_comb_OL_B$stars[BA_comb_OL_B$stars == "?"] <- NA
BA_comb_OL_B = subset(BA_comb_OL_B, select = -c(Variable, p))
BA_comb_OL_B <- BA_comb_OL_B %>% 
  mutate(ID = row_number())
BA_comb_OL_B <- BA_comb_OL_B[c("ID", "Level", "Estimate", "stars", "SE")]
BA_comb_OL_B <- BA_comb_OL_B %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_comb_OL_B <- BA_comb_OL_B %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_comb_OL_B$stars <- symnum(HA_comb_OL_B$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_comb_OL_B$stars[HA_comb_OL_B$stars == ""] <- NA
HA_comb_OL_B$stars[HA_comb_OL_B$stars == "?"] <- NA
HA_comb_OL_B = subset(HA_comb_OL_B, select = -c(Variable, p))
HA_comb_OL_B <- HA_comb_OL_B %>% 
  mutate(ID = row_number())
HA_comb_OL_B <- HA_comb_OL_B[c("ID", "Level", "Estimate", "stars", "SE")]
HA_comb_OL_B <- HA_comb_OL_B %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_comb_OL_B <- HA_comb_OL_B %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_comb_OL_B$stars <- symnum(AA_comb_OL_B$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_comb_OL_B$stars[AA_comb_OL_B$stars == ""] <- NA
AA_comb_OL_B$stars[AA_comb_OL_B$stars == "?"] <- NA
AA_comb_OL_B = subset(AA_comb_OL_B, select = -c(Variable, p))
AA_comb_OL_B <- AA_comb_OL_B %>% 
  mutate(ID = row_number())
AA_comb_OL_B <- AA_comb_OL_B[c("ID", "Level", "Estimate", "stars", "SE")]
AA_comb_OL_B <- AA_comb_OL_B %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_comb_OL_B <- AA_comb_OL_B %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_comb_OL_B$stars <- symnum(MA_comb_OL_B$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_comb_OL_B$stars[MA_comb_OL_B$stars == ""] <- NA
MA_comb_OL_B$stars[MA_comb_OL_B$stars == "?"] <- NA
MA_comb_OL_B = subset(MA_comb_OL_B, select = -c(Variable, p))
MA_comb_OL_B <- MA_comb_OL_B %>% 
  mutate(ID = row_number())
MA_comb_OL_B <- MA_comb_OL_B[c("ID", "Level", "Estimate", "stars", "SE")]
MA_comb_OL_B <- MA_comb_OL_B %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_comb_OL_B <- MA_comb_OL_B %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

library(plyr)
tablea5_cleaner2 <- join(WA_comb_OL_B,BA_comb_OL_B,by="ID")
tablea5_cleaner2 <- join(tablea5_cleaner2,HA_comb_OL_B,by="ID")
tablea5_cleaner2 <- join(tablea5_cleaner2,AA_comb_OL_B,by="ID")
tablea5_cleaner2 <- join(tablea5_cleaner2,MA_comb_OL_B,by="ID")
tablea5_cleaner3 = subset(tablea5_cleaner2, select = -c(1,5,8,11,14))
View(tablea5_cleaner3)
### rearrange row order in Excel

## Table A6
WA_comb_H <- cj(r3_Hisp, `White_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
BA_comb_H <- cj(r3_Hisp, `Black_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
HA_comb_H <- cj(r3_Hisp, `Hispanic_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
AA_comb_H <- cj(r3_Hisp, `Asian_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
MA_comb_H <- cj(r3_Hisp, `MENA_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")

detach("package:plyr", unload=TRUE)
WA_comb_OL_H <- WA_comb_H[,c('feature','level','estimate','std.error','p')]
WA_comb_OL_H <- rename(WA_comb_OL_H, Variable = feature)
WA_comb_OL_H <- rename(WA_comb_OL_H, Level = level)
WA_comb_OL_H <- rename(WA_comb_OL_H, Estimate = estimate)
WA_comb_OL_H <- rename(WA_comb_OL_H, SE = std.error)
BA_comb_OL_H <- BA_comb_H[,c('feature','level','estimate','std.error','p')]
BA_comb_OL_H <- rename(BA_comb_OL_H, Variable = feature)
BA_comb_OL_H <- rename(BA_comb_OL_H, Level = level)
BA_comb_OL_H <- rename(BA_comb_OL_H, Estimate = estimate)
BA_comb_OL_H <- rename(BA_comb_OL_H, SE = std.error)
HA_comb_OL_H <- HA_comb_H[,c('feature','level','estimate','std.error','p')]
HA_comb_OL_H <- rename(HA_comb_OL_H, Variable = feature)
HA_comb_OL_H <- rename(HA_comb_OL_H, Level = level)
HA_comb_OL_H <- rename(HA_comb_OL_H, Estimate = estimate)
HA_comb_OL_H <- rename(HA_comb_OL_H, SE = std.error)
AA_comb_OL_H <- AA_comb_H[,c('feature','level','estimate','std.error','p')]
AA_comb_OL_H <- rename(AA_comb_OL_H, Variable = feature)
AA_comb_OL_H <- rename(AA_comb_OL_H, Level = level)
AA_comb_OL_H <- rename(AA_comb_OL_H, Estimate = estimate)
AA_comb_OL_H <- rename(AA_comb_OL_H, SE = std.error)
MA_comb_OL_H <- MA_comb_H[,c('feature','level','estimate','std.error','p')]
MA_comb_OL_H <- rename(MA_comb_OL_H, Variable = feature)
MA_comb_OL_H <- rename(MA_comb_OL_H, Level = level)
MA_comb_OL_H <- rename(MA_comb_OL_H, Estimate = estimate)
MA_comb_OL_H <- rename(MA_comb_OL_H, SE = std.error)

WA_comb_OL_H$Level <- as.character(WA_comb_OL_H$Level)
for(i in 1:nrow(WA_comb_OL_H)){
  if(isTRUE(grepl("Wh",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "White"
  }
  else if(isTRUE(grepl("Bl",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "Black/African American"
  }
  else if(isTRUE(grepl("As",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "Asian/Asian American"
  }
  else if(isTRUE(grepl("Hisp",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "Hispanic/Latino(a/x)"
  }
  else if(isTRUE(grepl("MENA",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "Middle Eastern or North African"
  }
  else if(isTRUE(grepl("Don't",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "No"
  }
  else if(isTRUE(grepl("Speak",as.character(WA_comb_OL_H$Level[i])))){
    WA_comb_OL_H$Level[i] <- "Yes"
  }
  else{
    next
  }
}
i = 0
WA_comb_OL_H$Variable <- as.character(WA_comb_OL_H$Variable)
for(i in 1:nrow(WA_comb_OL_H)){
  if(isTRUE(WA_comb_OL_H$Variable[i]=="gender")){
    WA_comb_OL_H$Variable[i] <- "Image gender"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="ethnicity")){
    WA_comb_OL_H$Variable[i] <- "Image race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="skin_tone")){
    WA_comb_OL_H$Variable[i] <- "Skin color"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="birthparents_ethnicID")){
    WA_comb_OL_H$Variable[i] <- "Parents' background"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="age")){
    WA_comb_OL_H$Variable[i] <- "Age (years)"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="selfID_ethnic")){
    WA_comb_OL_H$Variable[i] <- "Self-identification"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="occup_status")){
    WA_comb_OL_H$Variable[i] <- "Parents' occupational status"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="english")){
    WA_comb_OL_H$Variable[i] <- "Speaks English at home"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="resrace")){
    WA_comb_OL_H$Variable[i] <- "Race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="educ")){
    WA_comb_OL_H$Variable[i] <- "Educational attainment"
  }
  else if(isTRUE(WA_comb_OL_H$Variable[i]=="party")){
    WA_comb_OL_H$Variable[i] <- "Party identification"
  }
  else{
    next
  }
}

gen_models_H <- list(BA_comb_OL_H, HA_comb_OL_H, AA_comb_OL_H, MA_comb_OL_H)

for(x in 1:length(gen_models_H)){
  gen_models_H[[x]]$Level <- as.character(gen_models_H[[x]]$Level)
  for(i in 1:nrow(gen_models_H[[x]])){
    if(isTRUE(grepl("Wh",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models_H[[x]]$Level[i]))){
      gen_models_H[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models_H[[x]]$Variable <- as.character(gen_models_H[[x]]$Variable)
  for(j in 1:nrow(gen_models_H[[x]])){
    if(isTRUE(gen_models_H[[x]]$Variable[j]=="gender")){
      gen_models_H[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="ethnicity")){
      gen_models_H[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="skin_tone")){
      gen_models_H[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models_H[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="age")){
      gen_models_H[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models_H[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="occup_status")){
      gen_models_H[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="english")){
      gen_models_H[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="resrace")){
      gen_models_H[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="educ")){
      gen_models_H[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models_H[[x]]$Variable[j]=="party")){
      gen_models_H[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
BA_comb_OL_H <- gen_models_H[[1]]
HA_comb_OL_H <- gen_models_H[[2]]
AA_comb_OL_H <- gen_models_H[[3]]
MA_comb_OL_H <- gen_models_H[[4]]

WA_comb_OL_H$stars <- symnum(WA_comb_OL_H$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_comb_OL_H$stars[WA_comb_OL_H$stars == ""] <- NA
WA_comb_OL_H$stars[WA_comb_OL_H$stars == "?"] <- NA
WA_comb_OL_H = subset(WA_comb_OL_H, select = -c(Variable, p))
WA_comb_OL_H <- WA_comb_OL_H %>% 
  mutate(ID = row_number())
WA_comb_OL_H <- WA_comb_OL_H[c("ID", "Level", "Estimate", "stars", "SE")]
WA_comb_OL_H <- WA_comb_OL_H %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_comb_OL_H <- WA_comb_OL_H %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_comb_OL_H$stars <- symnum(BA_comb_OL_H$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_comb_OL_H$stars[BA_comb_OL_H$stars == ""] <- NA
BA_comb_OL_H$stars[BA_comb_OL_H$stars == "?"] <- NA
BA_comb_OL_H = subset(BA_comb_OL_H, select = -c(Variable, p))
BA_comb_OL_H <- BA_comb_OL_H %>% 
  mutate(ID = row_number())
BA_comb_OL_H <- BA_comb_OL_H[c("ID", "Level", "Estimate", "stars", "SE")]
BA_comb_OL_H <- BA_comb_OL_H %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_comb_OL_H <- BA_comb_OL_H %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_comb_OL_H$stars <- symnum(HA_comb_OL_H$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_comb_OL_H$stars[HA_comb_OL_H$stars == ""] <- NA
HA_comb_OL_H$stars[HA_comb_OL_H$stars == "?"] <- NA
HA_comb_OL_H = subset(HA_comb_OL_H, select = -c(Variable, p))
HA_comb_OL_H <- HA_comb_OL_H %>% 
  mutate(ID = row_number())
HA_comb_OL_H <- HA_comb_OL_H[c("ID", "Level", "Estimate", "stars", "SE")]
HA_comb_OL_H <- HA_comb_OL_H %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_comb_OL_H <- HA_comb_OL_H %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_comb_OL_H$stars <- symnum(AA_comb_OL_H$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_comb_OL_H$stars[AA_comb_OL_H$stars == ""] <- NA
AA_comb_OL_H$stars[AA_comb_OL_H$stars == "?"] <- NA
AA_comb_OL_H = subset(AA_comb_OL_H, select = -c(Variable, p))
AA_comb_OL_H <- AA_comb_OL_H %>% 
  mutate(ID = row_number())
AA_comb_OL_H <- AA_comb_OL_H[c("ID", "Level", "Estimate", "stars", "SE")]
AA_comb_OL_H <- AA_comb_OL_H %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_comb_OL_H <- AA_comb_OL_H %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_comb_OL_H$stars <- symnum(MA_comb_OL_H$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_comb_OL_H$stars[MA_comb_OL_H$stars == ""] <- NA
MA_comb_OL_H$stars[MA_comb_OL_H$stars == "?"] <- NA
MA_comb_OL_H = subset(MA_comb_OL_H, select = -c(Variable, p))
MA_comb_OL_H <- MA_comb_OL_H %>% 
  mutate(ID = row_number())
MA_comb_OL_H <- MA_comb_OL_H[c("ID", "Level", "Estimate", "stars", "SE")]
MA_comb_OL_H <- MA_comb_OL_H %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_comb_OL_H <- MA_comb_OL_H %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

library(plyr)
tablea6_cleaner2 <- join(WA_comb_OL_H,BA_comb_OL_H,by="ID")
tablea6_cleaner2 <- join(tablea6_cleaner2,HA_comb_OL_H,by="ID")
tablea6_cleaner2 <- join(tablea6_cleaner2,AA_comb_OL_H,by="ID")
tablea6_cleaner2 <- join(tablea6_cleaner2,MA_comb_OL_H,by="ID")
tablea6_cleaner3 = subset(tablea6_cleaner2, select = -c(1,5,8,11,14))
View(tablea6_cleaner3)
### rearrange row order in Excel

## Table A7
WA_comb_A <- cj(r3_Asian, `White_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
BA_comb_A <- cj(r3_Asian, `Black_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
HA_comb_A <- cj(r3_Asian, `Hispanic_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
AA_comb_A <- cj(r3_Asian, `Asian_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")
MA_comb_A <- cj(r3_Asian, `MENA_agree` ~ `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + gender + ethnicity + `skin_tone`, id = ~ `Respondent_ID`, estimate = "amce")

detach("package:plyr", unload=TRUE)
WA_comb_OL_A <- WA_comb_A[,c('feature','level','estimate','std.error','p')]
WA_comb_OL_A <- rename(WA_comb_OL_A, Variable = feature)
WA_comb_OL_A <- rename(WA_comb_OL_A, Level = level)
WA_comb_OL_A <- rename(WA_comb_OL_A, Estimate = estimate)
WA_comb_OL_A <- rename(WA_comb_OL_A, SE = std.error)
BA_comb_OL_A <- BA_comb_A[,c('feature','level','estimate','std.error','p')]
BA_comb_OL_A <- rename(BA_comb_OL_A, Variable = feature)
BA_comb_OL_A <- rename(BA_comb_OL_A, Level = level)
BA_comb_OL_A <- rename(BA_comb_OL_A, Estimate = estimate)
BA_comb_OL_A <- rename(BA_comb_OL_A, SE = std.error)
HA_comb_OL_A <- HA_comb_A[,c('feature','level','estimate','std.error','p')]
HA_comb_OL_A <- rename(HA_comb_OL_A, Variable = feature)
HA_comb_OL_A <- rename(HA_comb_OL_A, Level = level)
HA_comb_OL_A <- rename(HA_comb_OL_A, Estimate = estimate)
HA_comb_OL_A <- rename(HA_comb_OL_A, SE = std.error)
AA_comb_OL_A <- AA_comb_A[,c('feature','level','estimate','std.error','p')]
AA_comb_OL_A <- rename(AA_comb_OL_A, Variable = feature)
AA_comb_OL_A <- rename(AA_comb_OL_A, Level = level)
AA_comb_OL_A <- rename(AA_comb_OL_A, Estimate = estimate)
AA_comb_OL_A <- rename(AA_comb_OL_A, SE = std.error)
MA_comb_OL_A <- MA_comb_A[,c('feature','level','estimate','std.error','p')]
MA_comb_OL_A <- rename(MA_comb_OL_A, Variable = feature)
MA_comb_OL_A <- rename(MA_comb_OL_A, Level = level)
MA_comb_OL_A <- rename(MA_comb_OL_A, Estimate = estimate)
MA_comb_OL_A <- rename(MA_comb_OL_A, SE = std.error)

WA_comb_OL_A$Level <- as.character(WA_comb_OL_A$Level)
for(i in 1:nrow(WA_comb_OL_A)){
  if(isTRUE(grepl("Wh",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "White"
  }
  else if(isTRUE(grepl("Bl",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "Black/African American"
  }
  else if(isTRUE(grepl("As",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "Asian/Asian American"
  }
  else if(isTRUE(grepl("Hisp",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "Hispanic/Latino(a/x)"
  }
  else if(isTRUE(grepl("MENA",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "Middle Eastern or North African"
  }
  else if(isTRUE(grepl("Don't",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "No"
  }
  else if(isTRUE(grepl("Speak",as.character(WA_comb_OL_A$Level[i])))){
    WA_comb_OL_A$Level[i] <- "Yes"
  }
  else{
    next
  }
}
i = 0
WA_comb_OL_A$Variable <- as.character(WA_comb_OL_A$Variable)
for(i in 1:nrow(WA_comb_OL_A)){
  if(isTRUE(WA_comb_OL_A$Variable[i]=="gender")){
    WA_comb_OL_A$Variable[i] <- "Image gender"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="ethnicity")){
    WA_comb_OL_A$Variable[i] <- "Image race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="skin_tone")){
    WA_comb_OL_A$Variable[i] <- "Skin color"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="birthparents_ethnicID")){
    WA_comb_OL_A$Variable[i] <- "Parents' background"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="age")){
    WA_comb_OL_A$Variable[i] <- "Age (years)"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="selfID_ethnic")){
    WA_comb_OL_A$Variable[i] <- "Self-identification"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="occup_status")){
    WA_comb_OL_A$Variable[i] <- "Parents' occupational status"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="english")){
    WA_comb_OL_A$Variable[i] <- "Speaks English at home"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="resrace")){
    WA_comb_OL_A$Variable[i] <- "Race/ethnicity"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="educ")){
    WA_comb_OL_A$Variable[i] <- "Educational attainment"
  }
  else if(isTRUE(WA_comb_OL_A$Variable[i]=="party")){
    WA_comb_OL_A$Variable[i] <- "Party identification"
  }
  else{
    next
  }
}

gen_models_A <- list(BA_comb_OL_A, HA_comb_OL_A, AA_comb_OL_A, MA_comb_OL_A)

for(x in 1:length(gen_models_A)){
  gen_models_A[[x]]$Level <- as.character(gen_models_A[[x]]$Level)
  for(i in 1:nrow(gen_models_A[[x]])){
    if(isTRUE(grepl("Wh",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models_A[[x]]$Level[i]))){
      gen_models_A[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models_A[[x]]$Variable <- as.character(gen_models_A[[x]]$Variable)
  for(j in 1:nrow(gen_models_A[[x]])){
    if(isTRUE(gen_models_A[[x]]$Variable[j]=="gender")){
      gen_models_A[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="ethnicity")){
      gen_models_A[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="skin_tone")){
      gen_models_A[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models_A[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="age")){
      gen_models_A[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models_A[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="occup_status")){
      gen_models_A[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="english")){
      gen_models_A[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="resrace")){
      gen_models_A[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="educ")){
      gen_models_A[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models_A[[x]]$Variable[j]=="party")){
      gen_models_A[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
BA_comb_OL_A <- gen_models_A[[1]]
HA_comb_OL_A <- gen_models_A[[2]]
AA_comb_OL_A <- gen_models_A[[3]]
MA_comb_OL_A <- gen_models_A[[4]]

WA_comb_OL_A$stars <- symnum(WA_comb_OL_A$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_comb_OL_A$stars[WA_comb_OL_A$stars == ""] <- NA
WA_comb_OL_A$stars[WA_comb_OL_A$stars == "?"] <- NA
WA_comb_OL_A = subset(WA_comb_OL_A, select = -c(Variable, p))
WA_comb_OL_A <- WA_comb_OL_A %>% 
  mutate(ID = row_number())
WA_comb_OL_A <- WA_comb_OL_A[c("ID", "Level", "Estimate", "stars", "SE")]
WA_comb_OL_A <- WA_comb_OL_A %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_comb_OL_A <- WA_comb_OL_A %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_comb_OL_A$stars <- symnum(BA_comb_OL_A$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_comb_OL_A$stars[BA_comb_OL_A$stars == ""] <- NA
BA_comb_OL_A$stars[BA_comb_OL_A$stars == "?"] <- NA
BA_comb_OL_A = subset(BA_comb_OL_A, select = -c(Variable, p))
BA_comb_OL_A <- BA_comb_OL_A %>% 
  mutate(ID = row_number())
BA_comb_OL_A <- BA_comb_OL_A[c("ID", "Level", "Estimate", "stars", "SE")]
BA_comb_OL_A <- BA_comb_OL_A %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_comb_OL_A <- BA_comb_OL_A %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_comb_OL_A$stars <- symnum(HA_comb_OL_A$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_comb_OL_A$stars[HA_comb_OL_A$stars == ""] <- NA
HA_comb_OL_A$stars[HA_comb_OL_A$stars == "?"] <- NA
HA_comb_OL_A = subset(HA_comb_OL_A, select = -c(Variable, p))
HA_comb_OL_A <- HA_comb_OL_A %>% 
  mutate(ID = row_number())
HA_comb_OL_A <- HA_comb_OL_A[c("ID", "Level", "Estimate", "stars", "SE")]
HA_comb_OL_A <- HA_comb_OL_A %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_comb_OL_A <- HA_comb_OL_A %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_comb_OL_A$stars <- symnum(AA_comb_OL_A$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_comb_OL_A$stars[AA_comb_OL_A$stars == ""] <- NA
AA_comb_OL_A$stars[AA_comb_OL_A$stars == "?"] <- NA
AA_comb_OL_A = subset(AA_comb_OL_A, select = -c(Variable, p))
AA_comb_OL_A <- AA_comb_OL_A %>% 
  mutate(ID = row_number())
AA_comb_OL_A <- AA_comb_OL_A[c("ID", "Level", "Estimate", "stars", "SE")]
AA_comb_OL_A <- AA_comb_OL_A %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_comb_OL_A <- AA_comb_OL_A %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_comb_OL_A$stars <- symnum(MA_comb_OL_A$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_comb_OL_A$stars[MA_comb_OL_A$stars == ""] <- NA
MA_comb_OL_A$stars[MA_comb_OL_A$stars == "?"] <- NA
MA_comb_OL_A = subset(MA_comb_OL_A, select = -c(Variable, p))
MA_comb_OL_A <- MA_comb_OL_A %>% 
  mutate(ID = row_number())
MA_comb_OL_A <- MA_comb_OL_A[c("ID", "Level", "Estimate", "stars", "SE")]
MA_comb_OL_A <- MA_comb_OL_A %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_comb_OL_A <- MA_comb_OL_A %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

library(plyr)
table7_cleaner2 <- join(WA_comb_OL_A,BA_comb_OL_A,by="ID")
table7_cleaner2 <- join(table7_cleaner2,HA_comb_OL_A,by="ID")
table7_cleaner2 <- join(table7_cleaner2,AA_comb_OL_A,by="ID")
table7_cleaner2 <- join(table7_cleaner2,MA_comb_OL_A,by="ID")
table7_cleaner3 = subset(table7_cleaner2, select = -c(1,5,8,11,14))
View(table7_cleaner3)
### rearrange row order in Excel


# Online Supplementary Tables----
## Figure S1 is a facsimile of what respondents saw in our survey and is not reproducible in R
## it is a screenshot from our survey instrument

## Table S1
WA_comb_mm <- cj(r3, `White_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "mm")

# Agreement that a profile is Black
BA_comb_mm <- cj(r3, `Black_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "mm")

# Agreement that a profile is Hisp
HA_comb_mm <- cj(r3, `Hispanic_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "mm")

# Agreement that a profile is Asian
AA_comb_mm <- cj(r3, `Asian_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "mm")

# Agreement that a profile is MENA
MA_comb_mm <- cj(r3, `MENA_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "mm")

detach("package:plyr", unload=TRUE)
WA_combM_OL <- WA_comb_mm[,c('feature','level','estimate','std.error','p')]
WA_combM_OL <- rename(WA_combM_OL, Variable = feature)
WA_combM_OL <- rename(WA_combM_OL, Level = level)
WA_combM_OL <- rename(WA_combM_OL, Estimate = estimate)
WA_combM_OL <- rename(WA_combM_OL, SE = std.error)
BA_combM_OL <- BA_comb_mm[,c('feature','level','estimate','std.error','p')]
BA_combM_OL <- rename(BA_combM_OL, Variable = feature)
BA_combM_OL <- rename(BA_combM_OL, Level = level)
BA_combM_OL <- rename(BA_combM_OL, Estimate = estimate)
BA_combM_OL <- rename(BA_combM_OL, SE = std.error)
HA_combM_OL <- HA_comb_mm[,c('feature','level','estimate','std.error','p')]
HA_combM_OL <- rename(HA_combM_OL, Variable = feature)
HA_combM_OL <- rename(HA_combM_OL, Level = level)
HA_combM_OL <- rename(HA_combM_OL, Estimate = estimate)
HA_combM_OL <- rename(HA_combM_OL, SE = std.error)
AA_combM_OL <- AA_comb_mm[,c('feature','level','estimate','std.error','p')]
AA_combM_OL <- rename(AA_combM_OL, Variable = feature)
AA_combM_OL <- rename(AA_combM_OL, Level = level)
AA_combM_OL <- rename(AA_combM_OL, Estimate = estimate)
AA_combM_OL <- rename(AA_combM_OL, SE = std.error)
MA_combM_OL <- MA_comb_mm[,c('feature','level','estimate','std.error','p')]
MA_combM_OL <- rename(MA_combM_OL, Variable = feature)
MA_combM_OL <- rename(MA_combM_OL, Level = level)
MA_combM_OL <- rename(MA_combM_OL, Estimate = estimate)
MA_combM_OL <- rename(MA_combM_OL, SE = std.error)

gen_models2 <- list(WA_combM_OL, BA_combM_OL, HA_combM_OL, AA_combM_OL, MA_combM_OL)

for(x in 1:length(gen_models2)){
  gen_models2[[x]]$Level <- as.character(gen_models2[[x]]$Level)
  for(i in 1:nrow(gen_models2[[x]])){
    if(isTRUE(grepl("Wh",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("Middle Eastern or North African",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "MENA"
    }
    else if(isTRUE(grepl("Don't",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models2[[x]]$Level[i]))){
      gen_models2[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models2[[x]]$Variable <- as.character(gen_models2[[x]]$Variable)
  for(j in 1:nrow(gen_models2[[x]])){
    if(isTRUE(gen_models2[[x]]$Variable[j]=="gender")){
      gen_models2[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="ethnicity")){
      gen_models2[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="skin_tone")){
      gen_models2[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models2[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="age")){
      gen_models2[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models2[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="occup_status")){
      gen_models2[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="english")){
      gen_models2[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="resrace")){
      gen_models2[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="educ")){
      gen_models2[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models2[[x]]$Variable[j]=="party")){
      gen_models2[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}

WA_combM_OL <- gen_models2[[1]]
BA_combM_OL <- gen_models2[[2]]
HA_combM_OL <- gen_models2[[3]]
AA_combM_OL <- gen_models2[[4]]
MA_combM_OL <- gen_models2[[5]]
WBHA_combM_OL <- cbind(WA_combM_OL, BA_combM_OL, HA_combM_OL, AA_combM_OL, MA_combM_OL)
WBHA_combM_OL <- WBHA_combM_OL[-c(5,6,7,10,11,12,15,16,17,20,21,22)]

### rearrange row order and change model names in Overleaf
WBHAcombMOL <- print(xtable(WBHA_combM_OL, caption = "Predicting agreement that profiles belong to an ethnoracial category, all respondents: Marginal means"), caption.placement = 'top', include.rownames=FALSE)

## Table S2
# Agreement that a profile is white - race
WA_race_mm <- cj(r3, `White_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)
# Agreement that a profile is Black - race
BA_race_mm <- cj(r3, `Black_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)

# Agreement that a profile is Hisp - race
HA_race_mm <- cj(r3, `Hispanic_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)

# Agreement that a profile is Asian - race
AA_race_mm <- cj(r3, `Asian_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)

# Agreement that a profile is MENA - race
MA_race_mm <- cj(r3, `MENA_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)

WA_race_Wm_OL <- WA_race_mm[1:49,c('feature','level','estimate','std.error')]
WA_race_Wm_OL <- rename(WA_race_Wm_OL, Variable = feature)
WA_race_Wm_OL <- rename(WA_race_Wm_OL, Level = level)
WA_race_Wm_OL <- rename(WA_race_Wm_OL, Est = estimate)
WA_race_Wm_OL <- rename(WA_race_Wm_OL, SE = std.error)
BA_race_Wm_OL <- BA_race_mm[1:49,c('feature','level','estimate','std.error')]
BA_race_Wm_OL <- rename(BA_race_Wm_OL, Variable = feature)
BA_race_Wm_OL <- rename(BA_race_Wm_OL, Level = level)
BA_race_Wm_OL <- rename(BA_race_Wm_OL, Est = estimate)
BA_race_Wm_OL <- rename(BA_race_Wm_OL, SE = std.error)
HA_race_Wm_OL <- HA_race_mm[1:49,c('feature','level','estimate','std.error')]
HA_race_Wm_OL <- rename(HA_race_Wm_OL, Variable = feature)
HA_race_Wm_OL <- rename(HA_race_Wm_OL, Level = level)
HA_race_Wm_OL <- rename(HA_race_Wm_OL, Est = estimate)
HA_race_Wm_OL <- rename(HA_race_Wm_OL, SE = std.error)
AA_race_Wm_OL <- AA_race_mm[1:49,c('feature','level','estimate','std.error')]
AA_race_Wm_OL <- rename(AA_race_Wm_OL, Variable = feature)
AA_race_Wm_OL <- rename(AA_race_Wm_OL, Level = level)
AA_race_Wm_OL <- rename(AA_race_Wm_OL, Est = estimate)
AA_race_Wm_OL <- rename(AA_race_Wm_OL, SE = std.error)
MA_race_Wm_OL <- MA_race_mm[1:49,c('feature','level','estimate','std.error')]
MA_race_Wm_OL <- rename(MA_race_Wm_OL, Variable = feature)
MA_race_Wm_OL <- rename(MA_race_Wm_OL, Level = level)
MA_race_Wm_OL <- rename(MA_race_Wm_OL, Est = estimate)
MA_race_Wm_OL <- rename(MA_race_Wm_OL, SE = std.error)
WA_race_Am_OL <- WA_race_mm[50:98,c('feature','level','estimate','std.error')]
WA_race_Am_OL <- rename(WA_race_Am_OL, Variable = feature)
WA_race_Am_OL <- rename(WA_race_Am_OL, Level = level)
WA_race_Am_OL <- rename(WA_race_Am_OL, Est = estimate)
WA_race_Am_OL <- rename(WA_race_Am_OL, SE = std.error)
BA_race_Am_OL <- BA_race_mm[50:98,c('feature','level','estimate','std.error')]
BA_race_Am_OL <- rename(BA_race_Am_OL, Variable = feature)
BA_race_Am_OL <- rename(BA_race_Am_OL, Level = level)
BA_race_Am_OL <- rename(BA_race_Am_OL, Est = estimate)
BA_race_Am_OL <- rename(BA_race_Am_OL, SE = std.error)
HA_race_Am_OL <- HA_race_mm[50:98,c('feature','level','estimate','std.error')]
HA_race_Am_OL <- rename(HA_race_Am_OL, Variable = feature)
HA_race_Am_OL <- rename(HA_race_Am_OL, Level = level)
HA_race_Am_OL <- rename(HA_race_Am_OL, Est = estimate)
HA_race_Am_OL <- rename(HA_race_Am_OL, SE = std.error)
AA_race_Am_OL <- AA_race_mm[50:98,c('feature','level','estimate','std.error')]
AA_race_Am_OL <- rename(AA_race_Am_OL, Variable = feature)
AA_race_Am_OL <- rename(AA_race_Am_OL, Level = level)
AA_race_Am_OL <- rename(AA_race_Am_OL, Est = estimate)
AA_race_Am_OL <- rename(AA_race_Am_OL, SE = std.error)
MA_race_Am_OL <- MA_race_mm[50:98,c('feature','level','estimate','std.error')]
MA_race_Am_OL <- rename(MA_race_Am_OL, Variable = feature)
MA_race_Am_OL <- rename(MA_race_Am_OL, Level = level)
MA_race_Am_OL <- rename(MA_race_Am_OL, Est = estimate)
MA_race_Am_OL <- rename(MA_race_Am_OL, SE = std.error)
WA_race_Bm_OL <- WA_race_mm[99:147,c('feature','level','estimate','std.error')]
WA_race_Bm_OL <- rename(WA_race_Bm_OL, Variable = feature)
WA_race_Bm_OL <- rename(WA_race_Bm_OL, Level = level)
WA_race_Bm_OL <- rename(WA_race_Bm_OL, Est = estimate)
WA_race_Bm_OL <- rename(WA_race_Bm_OL, SE = std.error)
BA_race_Bm_OL <- BA_race_mm[99:147,c('feature','level','estimate','std.error')]
BA_race_Bm_OL <- rename(BA_race_Bm_OL, Variable = feature)
BA_race_Bm_OL <- rename(BA_race_Bm_OL, Level = level)
BA_race_Bm_OL <- rename(BA_race_Bm_OL, Est = estimate)
BA_race_Bm_OL <- rename(BA_race_Bm_OL, SE = std.error)
HA_race_Bm_OL <- HA_race_mm[99:147,c('feature','level','estimate','std.error')]
HA_race_Bm_OL <- rename(HA_race_Bm_OL, Variable = feature)
HA_race_Bm_OL <- rename(HA_race_Bm_OL, Level = level)
HA_race_Bm_OL <- rename(HA_race_Bm_OL, Est = estimate)
HA_race_Bm_OL <- rename(HA_race_Bm_OL, SE = std.error)
AA_race_Bm_OL <- AA_race_mm[99:147,c('feature','level','estimate','std.error')]
AA_race_Bm_OL <- rename(AA_race_Bm_OL, Variable = feature)
AA_race_Bm_OL <- rename(AA_race_Bm_OL, Level = level)
AA_race_Bm_OL <- rename(AA_race_Bm_OL, Est = estimate)
AA_race_Bm_OL <- rename(AA_race_Bm_OL, SE = std.error)
MA_race_Bm_OL <- MA_race_mm[99:147,c('feature','level','estimate','std.error')]
MA_race_Bm_OL <- rename(MA_race_Bm_OL, Variable = feature)
MA_race_Bm_OL <- rename(MA_race_Bm_OL, Level = level)
MA_race_Bm_OL <- rename(MA_race_Bm_OL, Est = estimate)
MA_race_Bm_OL <- rename(MA_race_Bm_OL, SE = std.error)
WA_race_Hm_OL <- WA_race_mm[148:196,c('feature','level','estimate','std.error')]
WA_race_Hm_OL <- rename(WA_race_Hm_OL, Variable = feature)
WA_race_Hm_OL <- rename(WA_race_Hm_OL, Level = level)
WA_race_Hm_OL <- rename(WA_race_Hm_OL, Est = estimate)
WA_race_Hm_OL <- rename(WA_race_Hm_OL, SE = std.error)
BA_race_Hm_OL <- BA_race_mm[148:196,c('feature','level','estimate','std.error')]
BA_race_Hm_OL <- rename(BA_race_Hm_OL, Variable = feature)
BA_race_Hm_OL <- rename(BA_race_Hm_OL, Level = level)
BA_race_Hm_OL <- rename(BA_race_Hm_OL, Est = estimate)
BA_race_Hm_OL <- rename(BA_race_Hm_OL, SE = std.error)
HA_race_Hm_OL <- HA_race_mm[148:196,c('feature','level','estimate','std.error')]
HA_race_Hm_OL <- rename(HA_race_Hm_OL, Variable = feature)
HA_race_Hm_OL <- rename(HA_race_Hm_OL, Level = level)
HA_race_Hm_OL <- rename(HA_race_Hm_OL, Est = estimate)
HA_race_Hm_OL <- rename(HA_race_Hm_OL, SE = std.error)
AA_race_Hm_OL <- AA_race_mm[148:196,c('feature','level','estimate','std.error')]
AA_race_Hm_OL <- rename(AA_race_Hm_OL, Variable = feature)
AA_race_Hm_OL <- rename(AA_race_Hm_OL, Level = level)
AA_race_Hm_OL <- rename(AA_race_Hm_OL, Est = estimate)
AA_race_Hm_OL <- rename(AA_race_Hm_OL, SE = std.error)
MA_race_Hm_OL <- MA_race_mm[148:196,c('feature','level','estimate','std.error')]
MA_race_Hm_OL <- rename(MA_race_Hm_OL, Variable = feature)
MA_race_Hm_OL <- rename(MA_race_Hm_OL, Level = level)
MA_race_Hm_OL <- rename(MA_race_Hm_OL, Est = estimate)
MA_race_Hm_OL <- rename(MA_race_Hm_OL, SE = std.error)

WA_raceM_OL <- cbind(WA_race_Wm_OL, WA_race_Bm_OL, WA_race_Hm_OL, WA_race_Am_OL)
WA_raceM_OL <- WA_raceM_OL[-c(31:49),-c(5,6,9,10,13,14)]

BA_raceM_OL <- cbind(BA_race_Wm_OL, BA_race_Bm_OL, BA_race_Hm_OL, BA_race_Am_OL)
BA_raceM_OL <- BA_raceM_OL[-c(31:49),-c(5,6,9,10,13,14)]

HA_raceM_OL <- cbind(HA_race_Wm_OL, HA_race_Bm_OL, HA_race_Hm_OL, HA_race_Am_OL)
HA_raceM_OL <- HA_raceM_OL[-c(31:49),-c(5,6,9,10,13,14)]

AA_raceM_OL <- cbind(AA_race_Wm_OL, AA_race_Bm_OL, AA_race_Hm_OL, AA_race_Am_OL)
AA_raceM_OL <- AA_raceM_OL[-c(31:49),-c(5,6,9,10,13,14)]

MA_raceM_OL <- cbind(MA_race_Wm_OL, MA_race_Bm_OL, MA_race_Hm_OL, MA_race_Am_OL)
MA_raceM_OL <- MA_raceM_OL[-c(31:49),-c(5,6,9,10,13,14)]

race_models <- list(WA_raceM_OL, BA_raceM_OL, HA_raceM_OL, AA_raceM_OL, MA_raceM_OL)


for(x in 1:length(race_models)){
  race_models[[x]]$Level <- as.character(race_models[[x]]$Level)
  for(i in 1:nrow(race_models[[x]])){
    if(isTRUE(grepl("Wh",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("Middle Eastern or North African",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "MENA"
    }
    else if(isTRUE(grepl("Don't",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",race_models[[x]]$Level[i]))){
      race_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  race_models[[x]]$Variable <- as.character(race_models[[x]]$Variable)
  for(j in 1:nrow(race_models[[x]])){
    if(isTRUE(race_models[[x]]$Variable[j]=="gender")){
      race_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="ethnicity")){
      race_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="skin_tone")){
      race_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      race_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="age")){
      race_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="selfID_ethnic")){
      race_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="occup_status")){
      race_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="english")){
      race_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="resrace")){
      race_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="educ")){
      race_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="party")){
      race_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
  for(j in 1:nrow(race_models[[x]])){
    if(isTRUE(race_models[[x]]$Variable[j]=="Image gender")){
      if(j==2){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Image race/ethnicity")){
      if(j==4){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Skin color")){
      if(j==6 | j==7){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }      
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Parents' background")){
      if(j==9 | j==10 | j==11 | j==12){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      } 
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Age (years)")){
      if(j==14 | j==15 | j==16 | j==17){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      } 
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Self-identification")){
      if(j==27 | j==28 | j==29 | j==30){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Parents' occupational status")){
      if(j==19 | j==20){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Speaks English at home")){
      if(j==25){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Religion")){
      if(j==22 | j==23){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Sample")){
      if(j==32){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Gender")){
      if(j==34){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Income")){
      if(j==36 | j==37){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Nativity")){
      if(j==39){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Age")){
      if(j==41){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Educational attainment")){
      if(j==43 | j==44){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Party identification")){
      if(j==46 | j==47){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(race_models[[x]]$Variable[j]=="Condition")){
      if(j==49){
        race_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else{
      next
    } 
  }
}


WA_raceM_OL <- race_models[[1]]
BA_raceM_OL <- race_models[[2]]
HA_raceM_OL <- race_models[[3]]
AA_raceM_OL <- race_models[[4]]
MA_raceM_OL <- race_models[[5]]

mm_White_resp_OL <- cbind(WA_raceM_OL[c(1:4)], BA_raceM_OL[c(3:4)], HA_raceM_OL[c(3:4)], AA_raceM_OL[c(3:4)], MA_raceM_OL[c(3:4)])

## Table S3
### run the code for Table S2 first, then run the line below
mm_Black_resp_OL <- cbind(WA_raceM_OL[c(1,2,5,6)], BA_raceM_OL[c(5:6)], HA_raceM_OL[c(5:6)], AA_raceM_OL[c(5:6)], MA_raceM_OL[c(5:6)])

## Table S4
### run the code for Table S2 first, then run the line below
mm_Hisp_resp_OL <- cbind(WA_raceM_OL[c(1,2,7,8)], BA_raceM_OL[c(7:8)], HA_raceM_OL[c(7:8)], AA_raceM_OL[c(7:8)], MA_raceM_OL[c(7:8)])

## Table S5
### run the code for Table S2 first, then run the line below
mm_Asian_resp_OL <- cbind(WA_raceM_OL[c(1,2,9,10)], BA_raceM_OL[c(9:10)], HA_raceM_OL[c(9:10)], AA_raceM_OL[c(9:10)], MA_raceM_OL[c(9:10)])

## Table S6
# Agreement that a profile is white
WA_samp <- cj(r3, `White_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ Sample)

# Agreement that a profile is Black
BA_samp <- cj(r3, `Black_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ Sample)

# Agreement that a profile is Hisp
HA_samp <- cj(r3, `Hispanic_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ Sample)

# Agreement that a profile is Asian
AA_samp <- cj(r3, `Asian_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ Sample)

# Agreement that a profile is MENA
MA_samp <- cj(r3, `MENA_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ Sample)

WA_samp_S_OL <- WA_samp[1:51,c('feature','level','estimate','std.error')]
WA_samp_S_OL <- rename(WA_samp_S_OL, Variable = feature)
WA_samp_S_OL <- rename(WA_samp_S_OL, Level = level)
WA_samp_S_OL <- rename(WA_samp_S_OL, Est = estimate)
WA_samp_S_OL <- rename(WA_samp_S_OL, SE = std.error)
BA_samp_S_OL <- BA_samp[1:51,c('feature','level','estimate','std.error')]
BA_samp_S_OL <- rename(BA_samp_S_OL, Variable = feature)
BA_samp_S_OL <- rename(BA_samp_S_OL, Level = level)
BA_samp_S_OL <- rename(BA_samp_S_OL, Estimate = estimate)
BA_samp_S_OL <- rename(BA_samp_S_OL, SE = std.error)
HA_samp_S_OL <- HA_samp[1:51,c('feature','level','estimate','std.error')]
HA_samp_S_OL <- rename(HA_samp_S_OL, Variable = feature)
HA_samp_S_OL <- rename(HA_samp_S_OL, Level = level)
HA_samp_S_OL <- rename(HA_samp_S_OL, Estimate = estimate)
HA_samp_S_OL <- rename(HA_samp_S_OL, SE = std.error)
AA_samp_S_OL <- AA_samp[1:51,c('feature','level','estimate','std.error')]
AA_samp_S_OL <- rename(AA_samp_S_OL, Variable = feature)
AA_samp_S_OL <- rename(AA_samp_S_OL, Level = level)
AA_samp_S_OL <- rename(AA_samp_S_OL, Estimate = estimate)
AA_samp_S_OL <- rename(AA_samp_S_OL, SE = std.error)
MA_samp_S_OL <- MA_samp[1:51,c('feature','level','estimate','std.error')]
MA_samp_S_OL <- rename(MA_samp_S_OL, Variable = feature)
MA_samp_S_OL <- rename(MA_samp_S_OL, Level = level)
MA_samp_S_OL <- rename(MA_samp_S_OL, Estimate = estimate)
MA_samp_S_OL <- rename(MA_samp_S_OL, SE = std.error)
WA_samp_D_OL <- WA_samp[52:102,c('feature','level','estimate','std.error')]
WA_samp_D_OL <- rename(WA_samp_D_OL, Variable = feature)
WA_samp_D_OL <- rename(WA_samp_D_OL, Level = level)
WA_samp_D_OL <- rename(WA_samp_D_OL, Estimate = estimate)
WA_samp_D_OL <- rename(WA_samp_D_OL, SE = std.error)
BA_samp_D_OL <- BA_samp[52:102,c('feature','level','estimate','std.error')]
BA_samp_D_OL <- rename(BA_samp_D_OL, Variable = feature)
BA_samp_D_OL <- rename(BA_samp_D_OL, Level = level)
BA_samp_D_OL <- rename(BA_samp_D_OL, Estimate = estimate)
BA_samp_D_OL <- rename(BA_samp_D_OL, SE = std.error)
HA_samp_D_OL <- HA_samp[52:102,c('feature','level','estimate','std.error')]
HA_samp_D_OL <- rename(HA_samp_D_OL, Variable = feature)
HA_samp_D_OL <- rename(HA_samp_D_OL, Level = level)
HA_samp_D_OL <- rename(HA_samp_D_OL, Estimate = estimate)
HA_samp_D_OL <- rename(HA_samp_D_OL, SE = std.error)
AA_samp_D_OL <- AA_samp[52:102,c('feature','level','estimate','std.error')]
AA_samp_D_OL <- rename(AA_samp_D_OL, Variable = feature)
AA_samp_D_OL <- rename(AA_samp_D_OL, Level = level)
AA_samp_D_OL <- rename(AA_samp_D_OL, Estimate = estimate)
AA_samp_D_OL <- rename(AA_samp_D_OL, SE = std.error)
MA_samp_D_OL <- MA_samp[52:102,c('feature','level','estimate','std.error')]
MA_samp_D_OL <- rename(MA_samp_D_OL, Variable = feature)
MA_samp_D_OL <- rename(MA_samp_D_OL, Level = level)
MA_samp_D_OL <- rename(MA_samp_D_OL, Estimate = estimate)
MA_samp_D_OL <- rename(MA_samp_D_OL, SE = std.error)

WBHA_samp_S_OL <- cbind(WA_samp_S_OL, BA_samp_S_OL, HA_samp_S_OL)
WBHA_samp_S_OL <- WBHA_samp_S_OL[-c(31:51),-c(5,6,9,10)]

AMPA_samp_S_OL <- cbind(AA_samp_S_OL, MA_samp_S_OL)
AMPA_samp_S_OL <- AMPA_samp_S_OL[-c(31:51),-c(5,6,9,10)]

WBHA_samp_D_OL <- cbind(WA_samp_D_OL, BA_samp_D_OL, HA_samp_D_OL)
WBHA_samp_D_OL <- WBHA_samp_D_OL[-c(31:51),-c(5,6,9,10)]

AMPA_samp_D_OL <- cbind(AA_samp_D_OL, MA_samp_D_OL)
AMPA_samp_D_OL <- AMPA_samp_D_OL[-c(31:51),-c(5,6,9,10)]

sample_models <- list(WBHA_samp_S_OL, AMPA_samp_S_OL, WBHA_samp_D_OL, AMPA_samp_D_OL)

for(x in 1:length(sample_models)){
  sample_models[[x]]$Level <- as.character(sample_models[[x]]$Level)
  for(i in 1:nrow(sample_models[[x]])){
    if(isTRUE(grepl("Wh",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("Middle Eastern or North African",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "MENA"
    }
    else if(isTRUE(grepl("Don't",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",sample_models[[x]]$Level[i]))){
      sample_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  sample_models[[x]]$Variable <- as.character(sample_models[[x]]$Variable)
  for(j in 1:nrow(sample_models[[x]])){
    if(isTRUE(sample_models[[x]]$Variable[j]=="gender")){
      sample_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="ethnicity")){
      sample_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="skin_tone")){
      sample_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      sample_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="age")){
      sample_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="selfID_ethnic")){
      sample_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="occup_status")){
      sample_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="english")){
      sample_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="resrace")){
      sample_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="educ")){
      sample_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="party")){
      sample_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
  for(j in 1:nrow(sample_models[[x]])){
    if(isTRUE(sample_models[[x]]$Variable[j]=="Image gender")){
      if(j==2){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Image race/ethnicity")){
      if(j==4){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Skin color")){
      if(j==6 | j==7){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }      
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Parents' background")){
      if(j==9 | j==10 | j==11 | j==12){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      } 
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Age (years)")){
      if(j==14 | j==15 | j==16 | j==17){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      } 
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Self-identification")){
      if(j==27 | j==28 | j==29 | j==30){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Parents' occupational status")){
      if(j==19 | j==20){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Speaks English at home")){
      if(j==25){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Religion")){
      if(j==22 | j==23){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Gender")){
      if(j==32){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Race/ethnicity")){
      if(j==34 | j==35 | j==36){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Income")){
      if(j==38 | j==39){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Nativity")){
      if(j==41){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Age")){
      if(j==43){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Educational attainment")){
      if(j==45 | j==46){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Party identification")){
      if(j==48 | j==49){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else if(isTRUE(sample_models[[x]]$Variable[j]=="Condition")){
      if(j==51){
        sample_models[[x]]$Variable[j] <- ""
      }
      else{
        next
      }
    }
    else{
      next
    } 
  }
}

WBHA_samp_S_OL <- sample_models[[1]]
AMPA_samp_S_OL <- sample_models[[2]]
WBHA_samp_D_OL <- sample_models[[3]]
AMPA_samp_D_OL <- sample_models[[4]]

### added stars in Overleaf
sample_dynata_table <- cbind(WBHA_samp_D_OL, AMPA_samp_D_OL[c(3:6)])

## Table S7
### run the code for Table S6 before running the following line:
sample_student_table <- cbind(WBHA_samp_S_OL, AMPA_samp_S_OL[c(3:6)])
### added stars in Overleaf

## Table S8
ancW_selfW_light <- subset(r3, ancestryWhite == 1 & selfIDWhite == 1 & skin_tone == "Light")
freq(ancW_selfW_light$White_agree_prct)
ancW_selfB_light <- subset(r3, ancestryWhite == 1 & selfIDBlack == 1 & skin_tone == "Light")
freq(ancW_selfB_light$White_agree_prct)
freq(ancW_selfB_light$Black_agree_prct)
ancW_selfH_light <- subset(r3, ancestryWhite == 1 & selfIDHispanic== 1 & skin_tone == "Light")
freq(ancW_selfH_light$White_agree_prct)
freq(ancW_selfH_light$Hispanic_agree_prct)
ancW_selfA_light <- subset(r3, ancestryWhite == 1 & selfIDAsian == 1 & skin_tone == "Light")
freq(ancW_selfA_light$White_agree_prct)
freq(ancW_selfA_light$Asian_agree_prct)
ancW_selfM_light <- subset(r3, ancestryWhite == 1 & selfIDMENA == 1 & skin_tone == "Light")
freq(ancW_selfM_light$White_agree_prct)
freq(ancW_selfM_light$MENA_agree_prct)

ancB_selfW_light <- subset(r3, ancestryBlack == 1 & selfIDWhite == 1 & skin_tone == "Light")
freq(ancB_selfW_light$Black_agree_prct)
freq(ancB_selfW_light$White_agree_prct)
ancB_selfB_light <- subset(r3, ancestryBlack == 1 & selfIDBlack == 1 & skin_tone == "Light")
freq(ancB_selfB_light$Black_agree_prct)
ancB_selfH_light <- subset(r3, ancestryBlack == 1 & selfIDHispanic== 1 & skin_tone == "Light")
freq(ancB_selfH_light$Black_agree_prct)
freq(ancB_selfH_light$Hispanic_agree_prct)
ancB_selfA_light <- subset(r3, ancestryBlack == 1 & selfIDAsian == 1 & skin_tone == "Light")
freq(ancB_selfA_light$Black_agree_prct)
freq(ancB_selfA_light$Asian_agree_prct)
ancB_selfM_light <- subset(r3, ancestryBlack == 1 & selfIDMENA == 1 & skin_tone == "Light")
freq(ancB_selfM_light$Black_agree_prct)
freq(ancB_selfM_light$MENA_agree_prct)

ancH_selfW_light <- subset(r3, ancestryHispanic == 1 & selfIDWhite == 1 & skin_tone == "Light")
freq(ancH_selfW_light$Hispanic_agree_prct)
freq(ancH_selfW_light$White_agree_prct)
ancH_selfB_light <- subset(r3, ancestryHispanic == 1 & selfIDBlack == 1 & skin_tone == "Light")
freq(ancH_selfB_light$Hispanic_agree_prct)
freq(ancH_selfB_light$Black_agree_prct)
ancH_selfH_light <- subset(r3, ancestryHispanic == 1 & selfIDHispanic== 1 & skin_tone == "Light")
freq(ancH_selfH_light$Hispanic_agree_prct)
ancH_selfA_light <- subset(r3, ancestryHispanic == 1 & selfIDAsian == 1 & skin_tone == "Light")
freq(ancH_selfA_light$Hispanic_agree_prct)
freq(ancH_selfA_light$Asian_agree_prct)
ancH_selfM_light <- subset(r3, ancestryHispanic == 1 & selfIDMENA == 1 & skin_tone == "Light")
freq(ancH_selfM_light$Hispanic_agree_prct)
freq(ancH_selfM_light$MENA_agree_prct)

ancA_selfW_light <- subset(r3, ancestryAsian == 1 & selfIDWhite == 1 & skin_tone == "Light")
freq(ancA_selfW_light$Asian_agree_prct)
freq(ancA_selfW_light$White_agree_prct)
ancA_selfB_light <- subset(r3, ancestryAsian == 1 & selfIDBlack == 1 & skin_tone == "Light")
freq(ancA_selfB_light$Asian_agree_prct)
freq(ancA_selfB_light$Black_agree_prct)
ancA_selfH_light <- subset(r3, ancestryAsian == 1 & selfIDHispanic== 1 & skin_tone == "Light")
freq(ancA_selfH_light$Asian_agree_prct)
freq(ancA_selfH_light$Hispanic_agree_prct)
ancA_selfA_light <- subset(r3, ancestryAsian == 1 & selfIDAsian == 1 & skin_tone == "Light")
freq(ancA_selfA_light$Asian_agree_prct)
ancA_selfM_light <- subset(r3, ancestryAsian == 1 & selfIDMENA == 1 & skin_tone == "Light")
freq(ancA_selfM_light$Asian_agree_prct)
freq(ancA_selfM_light$MENA_agree_prct)

ancM_selfW_light <- subset(r3, ancestryMENA == 1 & selfIDWhite == 1 & skin_tone == "Light")
freq(ancM_selfW_light$MENA_agree_prct)
freq(ancM_selfW_light$White_agree_prct)
ancM_selfB_light <- subset(r3, ancestryMENA == 1 & selfIDBlack == 1 & skin_tone == "Light")
freq(ancM_selfB_light$MENA_agree_prct)
freq(ancM_selfB_light$Black_agree_prct)
ancM_selfH_light <- subset(r3, ancestryMENA == 1 & selfIDHispanic== 1 & skin_tone == "Light")
freq(ancM_selfH_light$MENA_agree_prct)
freq(ancM_selfH_light$Hispanic_agree_prct)
ancM_selfA_light <- subset(r3, ancestryMENA == 1 & selfIDAsian == 1 & skin_tone == "Light")
freq(ancM_selfA_light$MENA_agree_prct)
freq(ancM_selfA_light$Asian_agree_prct)
ancM_selfM_light <- subset(r3, ancestryMENA == 1 & selfIDMENA == 1 & skin_tone == "Light")
freq(ancM_selfM_light$MENA_agree_prct)

## Table S9
ancW_selfW_Medium <- subset(r3, ancestryWhite == 1 & selfIDWhite == 1 & skin_tone == "Medium ")
freq(ancW_selfW_Medium$White_agree_prct)
ancW_selfB_Medium <- subset(r3, ancestryWhite == 1 & selfIDBlack == 1 & skin_tone == "Medium ")
freq(ancW_selfB_Medium$White_agree_prct)
freq(ancW_selfB_Medium$Black_agree_prct)
ancW_selfH_Medium <- subset(r3, ancestryWhite == 1 & selfIDHispanic== 1 & skin_tone == "Medium ")
freq(ancW_selfH_Medium$White_agree_prct)
freq(ancW_selfH_Medium$Hispanic_agree_prct)
ancW_selfA_Medium <- subset(r3, ancestryWhite == 1 & selfIDAsian == 1 & skin_tone == "Medium ")
freq(ancW_selfA_Medium$White_agree_prct)
freq(ancW_selfA_Medium$Asian_agree_prct)
ancW_selfM_Medium <- subset(r3, ancestryWhite == 1 & selfIDMENA == 1 & skin_tone == "Medium ")
freq(ancW_selfM_Medium$White_agree_prct)
freq(ancW_selfM_Medium$MENA_agree_prct)

ancB_selfW_Medium <- subset(r3, ancestryBlack == 1 & selfIDWhite == 1 & skin_tone == "Medium ")
freq(ancB_selfW_Medium$Black_agree_prct)
freq(ancB_selfW_Medium$White_agree_prct)
ancB_selfB_Medium <- subset(r3, ancestryBlack == 1 & selfIDBlack == 1 & skin_tone == "Medium ")
freq(ancB_selfB_Medium$Black_agree_prct)
ancB_selfH_Medium <- subset(r3, ancestryBlack == 1 & selfIDHispanic== 1 & skin_tone == "Medium ")
freq(ancB_selfH_Medium$Black_agree_prct)
freq(ancB_selfH_Medium$Hispanic_agree_prct)
ancB_selfA_Medium <- subset(r3, ancestryBlack == 1 & selfIDAsian == 1 & skin_tone == "Medium ")
freq(ancB_selfA_Medium$Black_agree_prct)
freq(ancB_selfA_Medium$Asian_agree_prct)
ancB_selfM_Medium <- subset(r3, ancestryBlack == 1 & selfIDMENA == 1 & skin_tone == "Medium ")
freq(ancB_selfM_Medium$Black_agree_prct)
freq(ancB_selfM_Medium$MENA_agree_prct)

ancH_selfW_Medium <- subset(r3, ancestryHispanic == 1 & selfIDWhite == 1 & skin_tone == "Medium ")
freq(ancH_selfW_Medium$Hispanic_agree_prct)
freq(ancH_selfW_Medium$White_agree_prct)
ancH_selfB_Medium <- subset(r3, ancestryHispanic == 1 & selfIDBlack == 1 & skin_tone == "Medium ")
freq(ancH_selfB_Medium$Hispanic_agree_prct)
freq(ancH_selfB_Medium$Black_agree_prct)
ancH_selfH_Medium <- subset(r3, ancestryHispanic == 1 & selfIDHispanic== 1 & skin_tone == "Medium ")
freq(ancH_selfH_Medium$Hispanic_agree_prct)
ancH_selfA_Medium <- subset(r3, ancestryHispanic == 1 & selfIDAsian == 1 & skin_tone == "Medium ")
freq(ancH_selfA_Medium$Hispanic_agree_prct)
freq(ancH_selfA_Medium$Asian_agree_prct)
ancH_selfM_Medium <- subset(r3, ancestryHispanic == 1 & selfIDMENA == 1 & skin_tone == "Medium ")
freq(ancH_selfM_Medium$Hispanic_agree_prct)
freq(ancH_selfM_Medium$MENA_agree_prct)

ancA_selfW_Medium <- subset(r3, ancestryAsian == 1 & selfIDWhite == 1 & skin_tone == "Medium ")
freq(ancA_selfW_Medium$Asian_agree_prct)
freq(ancA_selfW_Medium$White_agree_prct)
ancA_selfB_Medium <- subset(r3, ancestryAsian == 1 & selfIDBlack == 1 & skin_tone == "Medium ")
freq(ancA_selfB_Medium$Asian_agree_prct)
freq(ancA_selfB_Medium$Black_agree_prct)
ancA_selfH_Medium <- subset(r3, ancestryAsian == 1 & selfIDHispanic== 1 & skin_tone == "Medium ")
freq(ancA_selfH_Medium$Asian_agree_prct)
freq(ancA_selfH_Medium$Hispanic_agree_prct)
ancA_selfA_Medium <- subset(r3, ancestryAsian == 1 & selfIDAsian == 1 & skin_tone == "Medium ")
freq(ancA_selfA_Medium$Asian_agree_prct)
ancA_selfM_Medium <- subset(r3, ancestryAsian == 1 & selfIDMENA == 1 & skin_tone == "Medium ")
freq(ancA_selfM_Medium$Asian_agree_prct)
freq(ancA_selfM_Medium$MENA_agree_prct)

ancM_selfW_Medium <- subset(r3, ancestryMENA == 1 & selfIDWhite == 1 & skin_tone == "Medium ")
freq(ancM_selfW_Medium$MENA_agree_prct)
freq(ancM_selfW_Medium$White_agree_prct)
ancM_selfB_Medium <- subset(r3, ancestryMENA == 1 & selfIDBlack == 1 & skin_tone == "Medium ")
freq(ancM_selfB_Medium$MENA_agree_prct)
freq(ancM_selfB_Medium$Black_agree_prct)
ancM_selfH_Medium <- subset(r3, ancestryMENA == 1 & selfIDHispanic== 1 & skin_tone == "Medium ")
freq(ancM_selfH_Medium$MENA_agree_prct)
freq(ancM_selfH_Medium$Hispanic_agree_prct)
ancM_selfA_Medium <- subset(r3, ancestryMENA == 1 & selfIDAsian == 1 & skin_tone == "Medium ")
freq(ancM_selfA_Medium$MENA_agree_prct)
freq(ancM_selfA_Medium$Asian_agree_prct)
ancM_selfM_Medium <- subset(r3, ancestryMENA == 1 & selfIDMENA == 1 & skin_tone == "Medium ")
freq(ancM_selfM_Medium$MENA_agree_prct)

## Table S10
ancW_selfW_dark <- subset(r3, ancestryWhite == 1 & selfIDWhite == 1 & skin_tone == "Dark")
freq(ancW_selfW_dark$White_agree_prct)
ancW_selfB_dark <- subset(r3, ancestryWhite == 1 & selfIDBlack == 1 & skin_tone == "Dark")
freq(ancW_selfB_dark$White_agree_prct)
freq(ancW_selfB_dark$Black_agree_prct)
ancW_selfH_dark <- subset(r3, ancestryWhite == 1 & selfIDHispanic== 1 & skin_tone == "Dark")
freq(ancW_selfH_dark$White_agree_prct)
freq(ancW_selfH_dark$Hispanic_agree_prct)
ancW_selfA_dark <- subset(r3, ancestryWhite == 1 & selfIDAsian == 1 & skin_tone == "Dark")
freq(ancW_selfA_dark$White_agree_prct)
freq(ancW_selfA_dark$Asian_agree_prct)
ancW_selfM_dark <- subset(r3, ancestryWhite == 1 & selfIDMENA == 1 & skin_tone == "Dark")
freq(ancW_selfM_dark$White_agree_prct)
freq(ancW_selfM_dark$MENA_agree_prct)

ancB_selfW_dark <- subset(r3, ancestryBlack == 1 & selfIDWhite == 1 & skin_tone == "Dark")
freq(ancB_selfW_dark$Black_agree_prct)
freq(ancB_selfW_dark$White_agree_prct)
ancB_selfB_dark <- subset(r3, ancestryBlack == 1 & selfIDBlack == 1 & skin_tone == "Dark")
freq(ancB_selfB_dark$Black_agree_prct)
ancB_selfH_dark <- subset(r3, ancestryBlack == 1 & selfIDHispanic== 1 & skin_tone == "Dark")
freq(ancB_selfH_dark$Black_agree_prct)
freq(ancB_selfH_dark$Hispanic_agree_prct)
ancB_selfA_dark <- subset(r3, ancestryBlack == 1 & selfIDAsian == 1 & skin_tone == "Dark")
freq(ancB_selfA_dark$Black_agree_prct)
freq(ancB_selfA_dark$Asian_agree_prct)
ancB_selfM_dark <- subset(r3, ancestryBlack == 1 & selfIDMENA == 1 & skin_tone == "Dark")
freq(ancB_selfM_dark$Black_agree_prct)
freq(ancB_selfM_dark$MENA_agree_prct)

ancH_selfW_dark <- subset(r3, ancestryHispanic == 1 & selfIDWhite == 1 & skin_tone == "Dark")
freq(ancH_selfW_dark$Hispanic_agree_prct)
freq(ancH_selfW_dark$White_agree_prct)
ancH_selfB_dark <- subset(r3, ancestryHispanic == 1 & selfIDBlack == 1 & skin_tone == "Dark")
freq(ancH_selfB_dark$Hispanic_agree_prct)
freq(ancH_selfB_dark$Black_agree_prct)
ancH_selfH_dark <- subset(r3, ancestryHispanic == 1 & selfIDHispanic== 1 & skin_tone == "Dark")
freq(ancH_selfH_dark$Hispanic_agree_prct)
ancH_selfA_dark <- subset(r3, ancestryHispanic == 1 & selfIDAsian == 1 & skin_tone == "Dark")
freq(ancH_selfA_dark$Hispanic_agree_prct)
freq(ancH_selfA_dark$Asian_agree_prct)
ancH_selfM_dark <- subset(r3, ancestryHispanic == 1 & selfIDMENA == 1 & skin_tone == "Dark")
freq(ancH_selfM_dark$Hispanic_agree_prct)
freq(ancH_selfM_dark$MENA_agree_prct)

ancA_selfW_dark <- subset(r3, ancestryAsian == 1 & selfIDWhite == 1 & skin_tone == "Dark")
freq(ancA_selfW_dark$Asian_agree_prct)
freq(ancA_selfW_dark$White_agree_prct)
ancA_selfB_dark <- subset(r3, ancestryAsian == 1 & selfIDBlack == 1 & skin_tone == "Dark")
freq(ancA_selfB_dark$Asian_agree_prct)
freq(ancA_selfB_dark$Black_agree_prct)
ancA_selfH_dark <- subset(r3, ancestryAsian == 1 & selfIDHispanic== 1 & skin_tone == "Dark")
freq(ancA_selfH_dark$Asian_agree_prct)
freq(ancA_selfH_dark$Hispanic_agree_prct)
ancA_selfA_dark <- subset(r3, ancestryAsian == 1 & selfIDAsian == 1 & skin_tone == "Dark")
freq(ancA_selfA_dark$Asian_agree_prct)
ancA_selfM_dark <- subset(r3, ancestryAsian == 1 & selfIDMENA == 1 & skin_tone == "Dark")
freq(ancA_selfM_dark$Asian_agree_prct)
freq(ancA_selfM_dark$MENA_agree_prct)

ancM_selfW_dark <- subset(r3, ancestryMENA == 1 & selfIDWhite == 1 & skin_tone == "Dark")
freq(ancM_selfW_dark$MENA_agree_prct)
freq(ancM_selfW_dark$White_agree_prct)
ancM_selfB_dark <- subset(r3, ancestryMENA == 1 & selfIDBlack == 1 & skin_tone == "Dark")
freq(ancM_selfB_dark$MENA_agree_prct)
freq(ancM_selfB_dark$Black_agree_prct)
ancM_selfH_dark <- subset(r3, ancestryMENA == 1 & selfIDHispanic== 1 & skin_tone == "Dark")
freq(ancM_selfH_dark$MENA_agree_prct)
freq(ancM_selfH_dark$Hispanic_agree_prct)
ancM_selfA_dark <- subset(r3, ancestryMENA == 1 & selfIDAsian == 1 & skin_tone == "Dark")
freq(ancM_selfA_dark$MENA_agree_prct)
freq(ancM_selfA_dark$Asian_agree_prct)
ancM_selfM_dark <- subset(r3, ancestryMENA == 1 & selfIDMENA == 1 & skin_tone == "Dark")
freq(ancM_selfM_dark$MENA_agree_prct)

## Table S11
# Agreement that a profile is white
WA_combI <- cj(r3, `White_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                 `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                 `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# Agreement that a profile is Black
BA_combI <- cj(r3, `Black_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                 `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                 `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# Agreement that a profile is Hisp
HA_combI <- cj(r3, `Hispanic_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                 `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                 `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# Agreement that a profile is Asian
AA_combI <- cj(r3, `Asian_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                 `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                 `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# Agreement that a profile is MENA
MA_combI <- cj(r3, `MENA_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                 `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                 `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

# go through and subset/create tables with labels + stars
WA_combI_OL <- WA_combI[,c('feature','level','estimate','std.error','p')]
WA_combI_OL <- dplyr::rename(WA_combI_OL, Variable = feature)
WA_combI_OL <- dplyr::rename(WA_combI_OL, Level = level)
WA_combI_OL <- dplyr::rename(WA_combI_OL, Estimate = estimate)
WA_combI_OL <- dplyr::rename(WA_combI_OL, SE = std.error)
BA_combI_OL <- BA_combI[,c('feature','level','estimate','std.error','p')]
BA_combI_OL <- dplyr::rename(BA_combI_OL, Variable = feature)
BA_combI_OL <- dplyr::rename(BA_combI_OL, Level = level)
BA_combI_OL <- dplyr::rename(BA_combI_OL, Estimate = estimate)
BA_combI_OL <- dplyr::rename(BA_combI_OL, SE = std.error)
HA_combI_OL <- HA_combI[,c('feature','level','estimate','std.error','p')]
HA_combI_OL <- dplyr::rename(HA_combI_OL, Variable = feature)
HA_combI_OL <- dplyr::rename(HA_combI_OL, Level = level)
HA_combI_OL <- dplyr::rename(HA_combI_OL, Estimate = estimate)
HA_combI_OL <- dplyr::rename(HA_combI_OL, SE = std.error)
AA_combI_OL <- AA_combI[,c('feature','level','estimate','std.error','p')]
AA_combI_OL <- dplyr::rename(AA_combI_OL, Variable = feature)
AA_combI_OL <- dplyr::rename(AA_combI_OL, Level = level)
AA_combI_OL <- dplyr::rename(AA_combI_OL, Estimate = estimate)
AA_combI_OL <- dplyr::rename(AA_combI_OL, SE = std.error)
MA_combI_OL <- MA_combI[,c('feature','level','estimate','std.error','p')]
MA_combI_OL <- dplyr::rename(MA_combI_OL, Variable = feature)
MA_combI_OL <- dplyr::rename(MA_combI_OL, Level = level)
MA_combI_OL <- dplyr::rename(MA_combI_OL, Estimate = estimate)
MA_combI_OL <- dplyr::rename(MA_combI_OL, SE = std.error)

gen_models <- list(WA_combI_OL, BA_combI_OL, HA_combI_OL, AA_combI_OL, MA_combI_OL)

for(x in 1:length(gen_models)){
  gen_models[[x]]$Level <- as.character(gen_models[[x]]$Level)
  for(i in 1:nrow(gen_models[[x]])){
    if(isTRUE(grepl("Wh",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models[[x]]$Variable <- as.character(gen_models[[x]]$Variable)
  for(j in 1:nrow(gen_models[[x]])){
    if(isTRUE(gen_models[[x]]$Variable[j]=="gender")){
      gen_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="ethnicity")){
      gen_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="skin_tone")){
      gen_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="age")){
      gen_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="occup_status")){
      gen_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="english")){
      gen_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="resrace")){
      gen_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="educ")){
      gen_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="party")){
      gen_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
WA_combI_OL <- gen_models[[1]]
BA_combI_OL <- gen_models[[2]]
HA_combI_OL <- gen_models[[3]]
AA_combI_OL <- gen_models[[4]]
MA_combI_OL <- gen_models[[5]]

WA_combI_OL$stars <- symnum(WA_combI_OL$p, 
                            symbols   = c("***","**","*","+", ""),
                            cutpoints = c(0, .001,.01,.05, .1, 1),
                            corr      = FALSE
)
WA_combI_OL$stars[WA_combI_OL$stars == ""] <- NA
WA_combI_OL$stars[WA_combI_OL$stars == "?"] <- NA
WA_combI_OL = subset(WA_combI_OL, select = -c(Variable, p))
WA_combI_OL <- WA_combI_OL %>% 
  dplyr::mutate(ID = row_number())
WA_combI_OL <- WA_combI_OL[c("ID", "Level", "Estimate", "stars", "SE")]
WA_combI_OL <- WA_combI_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_combI_OL <- WA_combI_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)


BA_combI_OL$stars <- symnum(BA_combI_OL$p, 
                            symbols   = c("***","**","*","+", ""),
                            cutpoints = c(0, .001,.01,.05, .1, 1),
                            corr      = FALSE
)
BA_combI_OL$stars[BA_combI_OL$stars == ""] <- NA
BA_combI_OL$stars[BA_combI_OL$stars == "?"] <- NA
BA_combI_OL = subset(BA_combI_OL, select = -c(Variable, p))
BA_combI_OL <- BA_combI_OL %>% 
  dplyr::mutate(ID = row_number())
BA_combI_OL <- BA_combI_OL[c("ID", "Level", "Estimate", "stars", "SE")]
BA_combI_OL <- BA_combI_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_combI_OL <- BA_combI_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_combI_OL$stars <- symnum(HA_combI_OL$p, 
                            symbols   = c("***","**","*","+", ""),
                            cutpoints = c(0, .001,.01,.05, .1, 1),
                            corr      = FALSE
)
HA_combI_OL$stars[HA_combI_OL$stars == ""] <- NA
HA_combI_OL$stars[HA_combI_OL$stars == "?"] <- NA
HA_combI_OL = subset(HA_combI_OL, select = -c(Variable, p))
HA_combI_OL <- HA_combI_OL %>% 
  dplyr::mutate(ID = row_number())
HA_combI_OL <- HA_combI_OL[c("ID", "Level", "Estimate", "stars", "SE")]
HA_combI_OL <- HA_combI_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_combI_OL <- HA_combI_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_combI_OL$stars <- symnum(AA_combI_OL$p, 
                            symbols   = c("***","**","*","+", ""),
                            cutpoints = c(0, .001,.01,.05, .1, 1),
                            corr      = FALSE
)
AA_combI_OL$stars[AA_combI_OL$stars == ""] <- NA
AA_combI_OL$stars[AA_combI_OL$stars == "?"] <- NA
AA_combI_OL = subset(AA_combI_OL, select = -c(Variable, p))
AA_combI_OL <- AA_combI_OL %>% 
  dplyr::mutate(ID = row_number())
AA_combI_OL <- AA_combI_OL[c("ID", "Level", "Estimate", "stars", "SE")]
AA_combI_OL <- AA_combI_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_combI_OL <- AA_combI_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_combI_OL$stars <- symnum(MA_combI_OL$p, 
                            symbols   = c("***","**","*","+", ""),
                            cutpoints = c(0, .001,.01,.05, .1, 1),
                            corr      = FALSE
)
MA_combI_OL$stars[MA_combI_OL$stars == ""] <- NA
MA_combI_OL$stars[MA_combI_OL$stars == "?"] <- NA
MA_combI_OL = subset(MA_combI_OL, select = -c(Variable, p))
MA_combI_OL <- MA_combI_OL %>% 
  dplyr::mutate(ID = row_number())
MA_combI_OL <- MA_combI_OL[c("ID", "Level", "Estimate", "stars", "SE")]
MA_combI_OL <- MA_combI_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_combI_OL <- MA_combI_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)


library(plyr)
table_image2 <- join(WA_combI_OL,BA_combI_OL,by="ID")
table_image2 <- join(table_image2,HA_combI_OL,by="ID")
table_image2 <- join(table_image2,AA_combI_OL,by="ID")
table_image2 <- join(table_image2,MA_combI_OL,by="ID")
table_image3 = subset(table_image2, select = -c(1,5,8,11,14))
View(table_image3)

## Tables S12-S15: code for models
WA_raceIW <- cj(r3_White, `White_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

BA_raceIW <- cj(r3_White, `Black_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

HA_raceIW <- cj(r3_White, `Hispanic_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

AA_raceIW <- cj(r3_White, `Asian_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

MA_raceIW <- cj(r3_White, `MENA_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")


WA_raceIB <- cj(r3_Black, `White_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

BA_raceIB <- cj(r3_Black, `Black_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

HA_raceIB <- cj(r3_Black, `Hispanic_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

AA_raceIB <- cj(r3_Black, `Asian_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

MA_raceIB <- cj(r3_Black, `MENA_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")


WA_raceIH <- cj(r3_Hisp, `White_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

BA_raceIH <- cj(r3_Hisp, `Black_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

HA_raceIH <- cj(r3_Hisp, `Hispanic_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

AA_raceIH <- cj(r3_Hisp, `Asian_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

MA_raceIH <- cj(r3_Hisp, `MENA_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")


WA_raceIA <- cj(r3_Asian, `White_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

BA_raceIA <- cj(r3_Asian, `Black_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

HA_raceIA <- cj(r3_Asian, `Hispanic_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

AA_raceIA <- cj(r3_Asian, `Asian_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

MA_raceIA <- cj(r3_Asian, `MENA_agree` ~ Latina1 + Latina3 + Latino1 + Latino2 + Latino3 + WhiteWoman1 + WhiteWoman2 + WhiteMan1 + WhiteMan2 + 
                  `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

## Table B2
# White respondents
WA_raceIW_OL <- WA_raceIW[,c('feature','level','estimate','std.error','p')]
WA_raceIW_OL <- dplyr::rename(WA_raceIW_OL, Variable = feature)
WA_raceIW_OL <- dplyr::rename(WA_raceIW_OL, Level = level)
WA_raceIW_OL <- dplyr::rename(WA_raceIW_OL, Estimate = estimate)
WA_raceIW_OL <- dplyr::rename(WA_raceIW_OL, SE = std.error)
BA_raceIW_OL <- BA_raceIW[,c('feature','level','estimate','std.error','p')]
BA_raceIW_OL <- dplyr::rename(BA_raceIW_OL, Variable = feature)
BA_raceIW_OL <- dplyr::rename(BA_raceIW_OL, Level = level)
BA_raceIW_OL <- dplyr::rename(BA_raceIW_OL, Estimate = estimate)
BA_raceIW_OL <- dplyr::rename(BA_raceIW_OL, SE = std.error)
HA_raceIW_OL <- HA_raceIW[,c('feature','level','estimate','std.error','p')]
HA_raceIW_OL <- dplyr::rename(HA_raceIW_OL, Variable = feature)
HA_raceIW_OL <- dplyr::rename(HA_raceIW_OL, Level = level)
HA_raceIW_OL <- dplyr::rename(HA_raceIW_OL, Estimate = estimate)
HA_raceIW_OL <- dplyr::rename(HA_raceIW_OL, SE = std.error)
AA_raceIW_OL <- AA_raceIW[,c('feature','level','estimate','std.error','p')]
AA_raceIW_OL <- dplyr::rename(AA_raceIW_OL, Variable = feature)
AA_raceIW_OL <- dplyr::rename(AA_raceIW_OL, Level = level)
AA_raceIW_OL <- dplyr::rename(AA_raceIW_OL, Estimate = estimate)
AA_raceIW_OL <- dplyr::rename(AA_raceIW_OL, SE = std.error)
MA_raceIW_OL <- MA_raceIW[,c('feature','level','estimate','std.error','p')]
MA_raceIW_OL <- dplyr::rename(MA_raceIW_OL, Variable = feature)
MA_raceIW_OL <- dplyr::rename(MA_raceIW_OL, Level = level)
MA_raceIW_OL <- dplyr::rename(MA_raceIW_OL, Estimate = estimate)
MA_raceIW_OL <- dplyr::rename(MA_raceIW_OL, SE = std.error)

gen_models <- list(WA_raceIW_OL, BA_raceIW_OL, HA_raceIW_OL, AA_raceIW_OL, MA_raceIW_OL)

for(x in 1:length(gen_models)){
  gen_models[[x]]$Level <- as.character(gen_models[[x]]$Level)
  for(i in 1:nrow(gen_models[[x]])){
    if(isTRUE(grepl("Wh",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models[[x]]$Variable <- as.character(gen_models[[x]]$Variable)
  for(j in 1:nrow(gen_models[[x]])){
    if(isTRUE(gen_models[[x]]$Variable[j]=="gender")){
      gen_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="ethnicity")){
      gen_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="skin_tone")){
      gen_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="age")){
      gen_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="occup_status")){
      gen_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="english")){
      gen_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="resrace")){
      gen_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="educ")){
      gen_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="party")){
      gen_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
WA_raceIW_OL <- gen_models[[1]]
BA_raceIW_OL <- gen_models[[2]]
HA_raceIW_OL <- gen_models[[3]]
AA_raceIW_OL <- gen_models[[4]]
MA_raceIW_OL <- gen_models[[5]]

WA_raceIW_OL$stars <- symnum(WA_raceIW_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_raceIW_OL$stars[WA_raceIW_OL$stars == ""] <- NA
WA_raceIW_OL$stars[WA_raceIW_OL$stars == "?"] <- NA
WA_raceIW_OL = subset(WA_raceIW_OL, select = -c(Variable, p))
WA_raceIW_OL <- WA_raceIW_OL %>% 
  dplyr::mutate(ID = row_number())
WA_raceIW_OL <- WA_raceIW_OL[c("ID", "Level", "Estimate", "stars", "SE")]
WA_raceIW_OL <- WA_raceIW_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_raceIW_OL <- WA_raceIW_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)


BA_raceIW_OL$stars <- symnum(BA_raceIW_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_raceIW_OL$stars[BA_raceIW_OL$stars == ""] <- NA
BA_raceIW_OL$stars[BA_raceIW_OL$stars == "?"] <- NA
BA_raceIW_OL = subset(BA_raceIW_OL, select = -c(Variable, p))
BA_raceIW_OL <- BA_raceIW_OL %>% 
  dplyr::mutate(ID = row_number())
BA_raceIW_OL <- BA_raceIW_OL[c("ID", "Level", "Estimate", "stars", "SE")]
BA_raceIW_OL <- BA_raceIW_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_raceIW_OL <- BA_raceIW_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_raceIW_OL$stars <- symnum(HA_raceIW_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_raceIW_OL$stars[HA_raceIW_OL$stars == ""] <- NA
HA_raceIW_OL$stars[HA_raceIW_OL$stars == "?"] <- NA
HA_raceIW_OL = subset(HA_raceIW_OL, select = -c(Variable, p))
HA_raceIW_OL <- HA_raceIW_OL %>% 
  dplyr::mutate(ID = row_number())
HA_raceIW_OL <- HA_raceIW_OL[c("ID", "Level", "Estimate", "stars", "SE")]
HA_raceIW_OL <- HA_raceIW_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_raceIW_OL <- HA_raceIW_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_raceIW_OL$stars <- symnum(AA_raceIW_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_raceIW_OL$stars[AA_raceIW_OL$stars == ""] <- NA
AA_raceIW_OL$stars[AA_raceIW_OL$stars == "?"] <- NA
AA_raceIW_OL = subset(AA_raceIW_OL, select = -c(Variable, p))
AA_raceIW_OL <- AA_raceIW_OL %>% 
  dplyr::mutate(ID = row_number())
AA_raceIW_OL <- AA_raceIW_OL[c("ID", "Level", "Estimate", "stars", "SE")]
AA_raceIW_OL <- AA_raceIW_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_raceIW_OL <- AA_raceIW_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_raceIW_OL$stars <- symnum(MA_raceIW_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_raceIW_OL$stars[MA_raceIW_OL$stars == ""] <- NA
MA_raceIW_OL$stars[MA_raceIW_OL$stars == "?"] <- NA
MA_raceIW_OL = subset(MA_raceIW_OL, select = -c(Variable, p))
MA_raceIW_OL <- MA_raceIW_OL %>% 
  dplyr::mutate(ID = row_number())
MA_raceIW_OL <- MA_raceIW_OL[c("ID", "Level", "Estimate", "stars", "SE")]
MA_raceIW_OL <- MA_raceIW_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_raceIW_OL <- MA_raceIW_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)


library(plyr)
table_image2W <- join(WA_raceIW_OL,BA_raceIW_OL,by="ID")
table_image2W <- join(table_image2W,HA_raceIW_OL,by="ID")
table_image2W <- join(table_image2W,AA_raceIW_OL,by="ID")
table_image2W <- join(table_image2W,MA_raceIW_OL,by="ID")
table_image3W = subset(table_image2W, select = -c(1,5,8,11,14))
View(table_image3W)


## Table S13
# Black respondents
WA_raceIB_OL <- WA_raceIB[,c('feature','level','estimate','std.error','p')]
WA_raceIB_OL <- dplyr::rename(WA_raceIB_OL, Variable = feature)
WA_raceIB_OL <- dplyr::rename(WA_raceIB_OL, Level = level)
WA_raceIB_OL <- dplyr::rename(WA_raceIB_OL, Estimate = estimate)
WA_raceIB_OL <- dplyr::rename(WA_raceIB_OL, SE = std.error)
BA_raceIB_OL <- BA_raceIB[,c('feature','level','estimate','std.error','p')]
BA_raceIB_OL <- dplyr::rename(BA_raceIB_OL, Variable = feature)
BA_raceIB_OL <- dplyr::rename(BA_raceIB_OL, Level = level)
BA_raceIB_OL <- dplyr::rename(BA_raceIB_OL, Estimate = estimate)
BA_raceIB_OL <- dplyr::rename(BA_raceIB_OL, SE = std.error)
HA_raceIB_OL <- HA_raceIB[,c('feature','level','estimate','std.error','p')]
HA_raceIB_OL <- dplyr::rename(HA_raceIB_OL, Variable = feature)
HA_raceIB_OL <- dplyr::rename(HA_raceIB_OL, Level = level)
HA_raceIB_OL <- dplyr::rename(HA_raceIB_OL, Estimate = estimate)
HA_raceIB_OL <- dplyr::rename(HA_raceIB_OL, SE = std.error)
AA_raceIB_OL <- AA_raceIB[,c('feature','level','estimate','std.error','p')]
AA_raceIB_OL <- dplyr::rename(AA_raceIB_OL, Variable = feature)
AA_raceIB_OL <- dplyr::rename(AA_raceIB_OL, Level = level)
AA_raceIB_OL <- dplyr::rename(AA_raceIB_OL, Estimate = estimate)
AA_raceIB_OL <- dplyr::rename(AA_raceIB_OL, SE = std.error)
MA_raceIB_OL <- MA_raceIB[,c('feature','level','estimate','std.error','p')]
MA_raceIB_OL <- dplyr::rename(MA_raceIB_OL, Variable = feature)
MA_raceIB_OL <- dplyr::rename(MA_raceIB_OL, Level = level)
MA_raceIB_OL <- dplyr::rename(MA_raceIB_OL, Estimate = estimate)
MA_raceIB_OL <- dplyr::rename(MA_raceIB_OL, SE = std.error)

gen_models <- list(WA_raceIB_OL, BA_raceIB_OL, HA_raceIB_OL, AA_raceIB_OL, MA_raceIB_OL)

for(x in 1:length(gen_models)){
  gen_models[[x]]$Level <- as.character(gen_models[[x]]$Level)
  for(i in 1:nrow(gen_models[[x]])){
    if(isTRUE(grepl("Wh",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models[[x]]$Variable <- as.character(gen_models[[x]]$Variable)
  for(j in 1:nrow(gen_models[[x]])){
    if(isTRUE(gen_models[[x]]$Variable[j]=="gender")){
      gen_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="ethnicity")){
      gen_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="skin_tone")){
      gen_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="age")){
      gen_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="occup_status")){
      gen_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="english")){
      gen_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="resrace")){
      gen_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="educ")){
      gen_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="party")){
      gen_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
WA_raceIB_OL <- gen_models[[1]]
BA_raceIB_OL <- gen_models[[2]]
HA_raceIB_OL <- gen_models[[3]]
AA_raceIB_OL <- gen_models[[4]]
MA_raceIB_OL <- gen_models[[5]]

WA_raceIB_OL$stars <- symnum(WA_raceIB_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_raceIB_OL$stars[WA_raceIB_OL$stars == ""] <- NA
WA_raceIB_OL$stars[WA_raceIB_OL$stars == "?"] <- NA
WA_raceIB_OL = subset(WA_raceIB_OL, select = -c(Variable, p))
WA_raceIB_OL <- WA_raceIB_OL %>% 
  dplyr::mutate(ID = row_number())
WA_raceIB_OL <- WA_raceIB_OL[c("ID", "Level", "Estimate", "stars", "SE")]
WA_raceIB_OL <- WA_raceIB_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_raceIB_OL <- WA_raceIB_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)


BA_raceIB_OL$stars <- symnum(BA_raceIB_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_raceIB_OL$stars[BA_raceIB_OL$stars == ""] <- NA
BA_raceIB_OL$stars[BA_raceIB_OL$stars == "?"] <- NA
BA_raceIB_OL = subset(BA_raceIB_OL, select = -c(Variable, p))
BA_raceIB_OL <- BA_raceIB_OL %>% 
  dplyr::mutate(ID = row_number())
BA_raceIB_OL <- BA_raceIB_OL[c("ID", "Level", "Estimate", "stars", "SE")]
BA_raceIB_OL <- BA_raceIB_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_raceIB_OL <- BA_raceIB_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_raceIB_OL$stars <- symnum(HA_raceIB_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_raceIB_OL$stars[HA_raceIB_OL$stars == ""] <- NA
HA_raceIB_OL$stars[HA_raceIB_OL$stars == "?"] <- NA
HA_raceIB_OL = subset(HA_raceIB_OL, select = -c(Variable, p))
HA_raceIB_OL <- HA_raceIB_OL %>% 
  dplyr::mutate(ID = row_number())
HA_raceIB_OL <- HA_raceIB_OL[c("ID", "Level", "Estimate", "stars", "SE")]
HA_raceIB_OL <- HA_raceIB_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_raceIB_OL <- HA_raceIB_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_raceIB_OL$stars <- symnum(AA_raceIB_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_raceIB_OL$stars[AA_raceIB_OL$stars == ""] <- NA
AA_raceIB_OL$stars[AA_raceIB_OL$stars == "?"] <- NA
AA_raceIB_OL = subset(AA_raceIB_OL, select = -c(Variable, p))
AA_raceIB_OL <- AA_raceIB_OL %>% 
  dplyr::mutate(ID = row_number())
AA_raceIB_OL <- AA_raceIB_OL[c("ID", "Level", "Estimate", "stars", "SE")]
AA_raceIB_OL <- AA_raceIB_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_raceIB_OL <- AA_raceIB_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_raceIB_OL$stars <- symnum(MA_raceIB_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_raceIB_OL$stars[MA_raceIB_OL$stars == ""] <- NA
MA_raceIB_OL$stars[MA_raceIB_OL$stars == "?"] <- NA
MA_raceIB_OL = subset(MA_raceIB_OL, select = -c(Variable, p))
MA_raceIB_OL <- MA_raceIB_OL %>% 
  dplyr::mutate(ID = row_number())
MA_raceIB_OL <- MA_raceIB_OL[c("ID", "Level", "Estimate", "stars", "SE")]
MA_raceIB_OL <- MA_raceIB_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_raceIB_OL <- MA_raceIB_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)


library(plyr)
table_image2B <- join(WA_raceIB_OL,BA_raceIB_OL,by="ID")
table_image2B <- join(table_image2B,HA_raceIB_OL,by="ID")
table_image2B <- join(table_image2B,AA_raceIB_OL,by="ID")
table_image2B <- join(table_image2B,MA_raceIB_OL,by="ID")
table_image3B = subset(table_image2B, select = -c(1,5,8,11,14))
View(table_image3B)

## Table S14
# Hispanic respondents
WA_raceIH_OL <- WA_raceIH[,c('feature','level','estimate','std.error','p')]
WA_raceIH_OL <- dplyr::rename(WA_raceIH_OL, Variable = feature)
WA_raceIH_OL <- dplyr::rename(WA_raceIH_OL, Level = level)
WA_raceIH_OL <- dplyr::rename(WA_raceIH_OL, Estimate = estimate)
WA_raceIH_OL <- dplyr::rename(WA_raceIH_OL, SE = std.error)
BA_raceIH_OL <- BA_raceIH[,c('feature','level','estimate','std.error','p')]
BA_raceIH_OL <- dplyr::rename(BA_raceIH_OL, Variable = feature)
BA_raceIH_OL <- dplyr::rename(BA_raceIH_OL, Level = level)
BA_raceIH_OL <- dplyr::rename(BA_raceIH_OL, Estimate = estimate)
BA_raceIH_OL <- dplyr::rename(BA_raceIH_OL, SE = std.error)
HA_raceIH_OL <- HA_raceIH[,c('feature','level','estimate','std.error','p')]
HA_raceIH_OL <- dplyr::rename(HA_raceIH_OL, Variable = feature)
HA_raceIH_OL <- dplyr::rename(HA_raceIH_OL, Level = level)
HA_raceIH_OL <- dplyr::rename(HA_raceIH_OL, Estimate = estimate)
HA_raceIH_OL <- dplyr::rename(HA_raceIH_OL, SE = std.error)
AA_raceIH_OL <- AA_raceIH[,c('feature','level','estimate','std.error','p')]
AA_raceIH_OL <- dplyr::rename(AA_raceIH_OL, Variable = feature)
AA_raceIH_OL <- dplyr::rename(AA_raceIH_OL, Level = level)
AA_raceIH_OL <- dplyr::rename(AA_raceIH_OL, Estimate = estimate)
AA_raceIH_OL <- dplyr::rename(AA_raceIH_OL, SE = std.error)
MA_raceIH_OL <- MA_raceIH[,c('feature','level','estimate','std.error','p')]
MA_raceIH_OL <- dplyr::rename(MA_raceIH_OL, Variable = feature)
MA_raceIH_OL <- dplyr::rename(MA_raceIH_OL, Level = level)
MA_raceIH_OL <- dplyr::rename(MA_raceIH_OL, Estimate = estimate)
MA_raceIH_OL <- dplyr::rename(MA_raceIH_OL, SE = std.error)

gen_models <- list(WA_raceIH_OL, BA_raceIH_OL, HA_raceIH_OL, AA_raceIH_OL, MA_raceIH_OL)

for(x in 1:length(gen_models)){
  gen_models[[x]]$Level <- as.character(gen_models[[x]]$Level)
  for(i in 1:nrow(gen_models[[x]])){
    if(isTRUE(grepl("Wh",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models[[x]]$Variable <- as.character(gen_models[[x]]$Variable)
  for(j in 1:nrow(gen_models[[x]])){
    if(isTRUE(gen_models[[x]]$Variable[j]=="gender")){
      gen_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="ethnicity")){
      gen_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="skin_tone")){
      gen_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="age")){
      gen_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="occup_status")){
      gen_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="english")){
      gen_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="resrace")){
      gen_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="educ")){
      gen_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="party")){
      gen_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
WA_raceIH_OL <- gen_models[[1]]
BA_raceIH_OL <- gen_models[[2]]
HA_raceIH_OL <- gen_models[[3]]
AA_raceIH_OL <- gen_models[[4]]
MA_raceIH_OL <- gen_models[[5]]

WA_raceIH_OL$stars <- symnum(WA_raceIH_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_raceIH_OL$stars[WA_raceIH_OL$stars == ""] <- NA
WA_raceIH_OL$stars[WA_raceIH_OL$stars == "?"] <- NA
WA_raceIH_OL = subset(WA_raceIH_OL, select = -c(Variable, p))
WA_raceIH_OL <- WA_raceIH_OL %>% 
  dplyr::mutate(ID = row_number())
WA_raceIH_OL <- WA_raceIH_OL[c("ID", "Level", "Estimate", "stars", "SE")]
WA_raceIH_OL <- WA_raceIH_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_raceIH_OL <- WA_raceIH_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_raceIH_OL$stars <- symnum(BA_raceIH_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_raceIH_OL$stars[BA_raceIH_OL$stars == ""] <- NA
BA_raceIH_OL$stars[BA_raceIH_OL$stars == "?"] <- NA
BA_raceIH_OL = subset(BA_raceIH_OL, select = -c(Variable, p))
BA_raceIH_OL <- BA_raceIH_OL %>% 
  dplyr::mutate(ID = row_number())
BA_raceIH_OL <- BA_raceIH_OL[c("ID", "Level", "Estimate", "stars", "SE")]
BA_raceIH_OL <- BA_raceIH_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_raceIH_OL <- BA_raceIH_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_raceIH_OL$stars <- symnum(HA_raceIH_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_raceIH_OL$stars[HA_raceIH_OL$stars == ""] <- NA
HA_raceIH_OL$stars[HA_raceIH_OL$stars == "?"] <- NA
HA_raceIH_OL = subset(HA_raceIH_OL, select = -c(Variable, p))
HA_raceIH_OL <- HA_raceIH_OL %>% 
  dplyr::mutate(ID = row_number())
HA_raceIH_OL <- HA_raceIH_OL[c("ID", "Level", "Estimate", "stars", "SE")]
HA_raceIH_OL <- HA_raceIH_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_raceIH_OL <- HA_raceIH_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_raceIH_OL$stars <- symnum(AA_raceIH_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_raceIH_OL$stars[AA_raceIH_OL$stars == ""] <- NA
AA_raceIH_OL$stars[AA_raceIH_OL$stars == "?"] <- NA
AA_raceIH_OL = subset(AA_raceIH_OL, select = -c(Variable, p))
AA_raceIH_OL <- AA_raceIH_OL %>% 
  dplyr::mutate(ID = row_number())
AA_raceIH_OL <- AA_raceIH_OL[c("ID", "Level", "Estimate", "stars", "SE")]
AA_raceIH_OL <- AA_raceIH_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_raceIH_OL <- AA_raceIH_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_raceIH_OL$stars <- symnum(MA_raceIH_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_raceIH_OL$stars[MA_raceIH_OL$stars == ""] <- NA
MA_raceIH_OL$stars[MA_raceIH_OL$stars == "?"] <- NA
MA_raceIH_OL = subset(MA_raceIH_OL, select = -c(Variable, p))
MA_raceIH_OL <- MA_raceIH_OL %>% 
  dplyr::mutate(ID = row_number())
MA_raceIH_OL <- MA_raceIH_OL[c("ID", "Level", "Estimate", "stars", "SE")]
MA_raceIH_OL <- MA_raceIH_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_raceIH_OL <- MA_raceIH_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

table_image2H <- join(WA_raceIH_OL,BA_raceIH_OL,by="ID")
table_image2H <- join(table_image2H,HA_raceIH_OL,by="ID")
table_image2H <- join(table_image2H,AA_raceIH_OL,by="ID")
table_image2H <- join(table_image2H,MA_raceIH_OL,by="ID")
table_image3H = subset(table_image2H, select = -c(1,5,8,11,14))
View(table_image3H)

## Table S15
# Asian respondents
WA_raceIA_OL <- WA_raceIA[,c('feature','level','estimate','std.error','p')]
WA_raceIA_OL <- dplyr::rename(WA_raceIA_OL, Variable = feature)
WA_raceIA_OL <- dplyr::rename(WA_raceIA_OL, Level = level)
WA_raceIA_OL <- dplyr::rename(WA_raceIA_OL, Estimate = estimate)
WA_raceIA_OL <- dplyr::rename(WA_raceIA_OL, SE = std.error)
BA_raceIA_OL <- BA_raceIA[,c('feature','level','estimate','std.error','p')]
BA_raceIA_OL <- dplyr::rename(BA_raceIA_OL, Variable = feature)
BA_raceIA_OL <- dplyr::rename(BA_raceIA_OL, Level = level)
BA_raceIA_OL <- dplyr::rename(BA_raceIA_OL, Estimate = estimate)
BA_raceIA_OL <- dplyr::rename(BA_raceIA_OL, SE = std.error)
HA_raceIA_OL <- HA_raceIA[,c('feature','level','estimate','std.error','p')]
HA_raceIA_OL <- dplyr::rename(HA_raceIA_OL, Variable = feature)
HA_raceIA_OL <- dplyr::rename(HA_raceIA_OL, Level = level)
HA_raceIA_OL <- dplyr::rename(HA_raceIA_OL, Estimate = estimate)
HA_raceIA_OL <- dplyr::rename(HA_raceIA_OL, SE = std.error)
AA_raceIA_OL <- AA_raceIA[,c('feature','level','estimate','std.error','p')]
AA_raceIA_OL <- dplyr::rename(AA_raceIA_OL, Variable = feature)
AA_raceIA_OL <- dplyr::rename(AA_raceIA_OL, Level = level)
AA_raceIA_OL <- dplyr::rename(AA_raceIA_OL, Estimate = estimate)
AA_raceIA_OL <- dplyr::rename(AA_raceIA_OL, SE = std.error)
MA_raceIA_OL <- MA_raceIA[,c('feature','level','estimate','std.error','p')]
MA_raceIA_OL <- dplyr::rename(MA_raceIA_OL, Variable = feature)
MA_raceIA_OL <- dplyr::rename(MA_raceIA_OL, Level = level)
MA_raceIA_OL <- dplyr::rename(MA_raceIA_OL, Estimate = estimate)
MA_raceIA_OL <- dplyr::rename(MA_raceIA_OL, SE = std.error)

gen_models <- list(WA_raceIA_OL, BA_raceIA_OL, HA_raceIA_OL, AA_raceIA_OL, MA_raceIA_OL)

for(x in 1:length(gen_models)){
  gen_models[[x]]$Level <- as.character(gen_models[[x]]$Level)
  for(i in 1:nrow(gen_models[[x]])){
    if(isTRUE(grepl("Wh",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "White"
    }
    else if(isTRUE(grepl("Bl",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Black/African American"
    }
    else if(isTRUE(grepl("As",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Asian/Asian American"
    }
    else if(isTRUE(grepl("Hisp",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Hispanic/Latino(a/x)"
    }
    else if(isTRUE(grepl("MENA",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Middle Eastern or North African"
    }
    else if(isTRUE(grepl("Don't",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "No"
    }
    else if(isTRUE(grepl("Speak",gen_models[[x]]$Level[i]))){
      gen_models[[x]]$Level[i] <- "Yes"
    }
    else{
      next
    }
  }
  gen_models[[x]]$Variable <- as.character(gen_models[[x]]$Variable)
  for(j in 1:nrow(gen_models[[x]])){
    if(isTRUE(gen_models[[x]]$Variable[j]=="gender")){
      gen_models[[x]]$Variable[j] <- "Image gender"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="ethnicity")){
      gen_models[[x]]$Variable[j] <- "Image race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="skin_tone")){
      gen_models[[x]]$Variable[j] <- "Skin color"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="birthparents_ethnicID")){
      gen_models[[x]]$Variable[j] <- "Parents' background"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="age")){
      gen_models[[x]]$Variable[j] <- "Age (years)"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="selfID_ethnic")){
      gen_models[[x]]$Variable[j] <- "Self-identification"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="occup_status")){
      gen_models[[x]]$Variable[j] <- "Parents' occupational status"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="english")){
      gen_models[[x]]$Variable[j] <- "Speaks English at home"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="resrace")){
      gen_models[[x]]$Variable[j] <- "Race/ethnicity"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="educ")){
      gen_models[[x]]$Variable[j] <- "Educational attainment"
    }
    else if(isTRUE(gen_models[[x]]$Variable[j]=="party")){
      gen_models[[x]]$Variable[j] <- "Party identification"
    }
    else{
      next
    }
  }
}
WA_raceIA_OL <- gen_models[[1]]
BA_raceIA_OL <- gen_models[[2]]
HA_raceIA_OL <- gen_models[[3]]
AA_raceIA_OL <- gen_models[[4]]
MA_raceIA_OL <- gen_models[[5]]

WA_raceIA_OL$stars <- symnum(WA_raceIA_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
WA_raceIA_OL$stars[WA_raceIA_OL$stars == ""] <- NA
WA_raceIA_OL$stars[WA_raceIA_OL$stars == "?"] <- NA
WA_raceIA_OL = subset(WA_raceIA_OL, select = -c(Variable, p))
WA_raceIA_OL <- WA_raceIA_OL %>% 
  dplyr::mutate(ID = row_number())
WA_raceIA_OL <- WA_raceIA_OL[c("ID", "Level", "Estimate", "stars", "SE")]
WA_raceIA_OL <- WA_raceIA_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
WA_raceIA_OL <- WA_raceIA_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

BA_raceIA_OL$stars <- symnum(BA_raceIA_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
BA_raceIA_OL$stars[BA_raceIA_OL$stars == ""] <- NA
BA_raceIA_OL$stars[BA_raceIA_OL$stars == "?"] <- NA
BA_raceIA_OL = subset(BA_raceIA_OL, select = -c(Variable, p))
BA_raceIA_OL <- BA_raceIA_OL %>% 
  dplyr::mutate(ID = row_number())
BA_raceIA_OL <- BA_raceIA_OL[c("ID", "Level", "Estimate", "stars", "SE")]
BA_raceIA_OL <- BA_raceIA_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
BA_raceIA_OL <- BA_raceIA_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

HA_raceIA_OL$stars <- symnum(HA_raceIA_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
HA_raceIA_OL$stars[HA_raceIA_OL$stars == ""] <- NA
HA_raceIA_OL$stars[HA_raceIA_OL$stars == "?"] <- NA
HA_raceIA_OL = subset(HA_raceIA_OL, select = -c(Variable, p))
HA_raceIA_OL <- HA_raceIA_OL %>% 
  dplyr::mutate(ID = row_number())
HA_raceIA_OL <- HA_raceIA_OL[c("ID", "Level", "Estimate", "stars", "SE")]
HA_raceIA_OL <- HA_raceIA_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
HA_raceIA_OL <- HA_raceIA_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

AA_raceIA_OL$stars <- symnum(AA_raceIA_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
AA_raceIA_OL$stars[AA_raceIA_OL$stars == ""] <- NA
AA_raceIA_OL$stars[AA_raceIA_OL$stars == "?"] <- NA
AA_raceIA_OL = subset(AA_raceIA_OL, select = -c(Variable, p))
AA_raceIA_OL <- AA_raceIA_OL %>% 
  dplyr::mutate(ID = row_number())
AA_raceIA_OL <- AA_raceIA_OL[c("ID", "Level", "Estimate", "stars", "SE")]
AA_raceIA_OL <- AA_raceIA_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
AA_raceIA_OL <- AA_raceIA_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

MA_raceIA_OL$stars <- symnum(MA_raceIA_OL$p, 
                             symbols   = c("***","**","*","+", ""),
                             cutpoints = c(0, .001,.01,.05, .1, 1),
                             corr      = FALSE
)
MA_raceIA_OL$stars[MA_raceIA_OL$stars == ""] <- NA
MA_raceIA_OL$stars[MA_raceIA_OL$stars == "?"] <- NA
MA_raceIA_OL = subset(MA_raceIA_OL, select = -c(Variable, p))
MA_raceIA_OL <- MA_raceIA_OL %>% 
  dplyr::mutate(ID = row_number())
MA_raceIA_OL <- MA_raceIA_OL[c("ID", "Level", "Estimate", "stars", "SE")]
MA_raceIA_OL <- MA_raceIA_OL %>% 
  mutate_if(is.numeric, round, digits = 2)
MA_raceIA_OL <- MA_raceIA_OL %>% unite("Est", Estimate:stars, sep = "", remove = TRUE, na.rm = TRUE)

table_image2A <- join(WA_raceIA_OL,BA_raceIA_OL,by="ID")
table_image2A <- join(table_image2A,HA_raceIA_OL,by="ID")
table_image2A <- join(table_image2A,AA_raceIA_OL,by="ID")
table_image2A <- join(table_image2A,MA_raceIA_OL,by="ID")
table_image3A = subset(table_image2A, select = -c(1,5,8,11,14))
View(table_image2A)


## Table S16
r3$selfID_conflicts[r3$skin_tone == "Light" & r3$birthparents_ethnicID == "White (parents)"] <- "White and Light"
#r3$white_light[r3$skin_tone != "Light" | r3$birthparents_ethnicID != "White (parents)"] <- 0
r3$selfID_conflicts[r3$skin_tone == "Dark" & r3$birthparents_ethnicID == "White (parents)"] <- "White and Dark"
#r3$white_dark[r3$skin_tone != "Dark" | r3$birthparents_ethnicID != "White (parents)"] <- 0
r3$selfID_conflicts[r3$skin_tone == "Light" & r3$birthparents_ethnicID == "Black/African-American"] <- "Black and Light"
#r3$black_light[r3$skin_tone != "Light" | r3$birthparents_ethnicID != "Black/African-American"] <- 0
r3$selfID_conflicts[r3$skin_tone == "Dark" & r3$birthparents_ethnicID == "Black/African-American"] <- "Black and Dark"
#r3$black_dark[r3$skin_tone != "Dark" | r3$birthparents_ethnicID != "Black/African-American"] <- 0
r3$selfID_conflicts <- factor(r3$selfID_conflicts, levels = c("White and Light","White and Dark","Black and Light","Black and Dark"))

table_s16 <- r3 %>%
  filter(!is.na(selfID_conflicts)) %>%
  dplyr::group_by(selfID_conflicts) %>%
  dplyr::summarize(mean_White = mean(White_agree_prct, na.rm = T) * 100,
                   mean_Black = mean(Black_agree_prct, na.rm = T) * 100,
                   mean_Hispanic = mean(Hispanic_agree_prct, na.rm = T) * 100,
                   mean_Asian = mean(Asian_agree_prct, na.rm = T) * 100,
                   mean_MENA = mean(MENA_agree_prct, na.rm = T) * 100)




# All Figures----
## Figure 1
# for the White_agree model,
# Black/African American is the reference category for birth parents' ethnic ID
# and profile self-ID. "Dark" is the reference category for skin tone
WA_comb_1 <- cj(r3, `White_agree` ~ gender + ethnicity + `skin_tone_rev` + `birthparents_ethnicID_white` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic_white` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# create BA_comb, HA_comb, AA_comb, and MA_comb found in the code for Table A3

# row bind models
WA_comb_1$identifier <- "White agree"
BA_comb$identifier <- "Black agree"
HA_comb$identifier <- "Hispanic agree"
AA_comb$identifier <- "Asian agree"
MA_comb$identifier <- "MENA agree"

fig1_data <- bind_rows(WA_comb_1, BA_comb, HA_comb, AA_comb, MA_comb, .id = "identifier")
fig1_data2 <- fig1_data[c(7,9,27,58,61,79,109,113,131,160,162,180,211,216,234),]
fig1_data2$feature[fig1_data2$feature == "birthparents_ethnicID_white"] <- "birthparents_ethnicID"
fig1_data2$feature[fig1_data2$feature == "selfID_ethnic_white"] <- "selfID_ethnic"
fig1_data2$feature[fig1_data2$feature == "skin_tone_rev"] <- "skin_tone"

fig1_data2$outcome[fig1_data2$outcome == "White_agree"] <- "White"
fig1_data2$outcome[fig1_data2$outcome == "Black_agree"] <- "Black"
fig1_data2$outcome[fig1_data2$outcome == "Hispanic_agree"] <- "Latino"
fig1_data2$outcome[fig1_data2$outcome == "Asian_agree"] <- "Asian"
fig1_data2$outcome[fig1_data2$outcome == "MENA_agree"] <- "MENA"

fig1_data2 <- fig1_data2 %>%
  mutate(outcome =  factor(outcome, levels = c("MENA","Asian","Latino","Black","White"))) %>%
  arrange(outcome)
fig1_data2$feature <- factor(fig1_data2$feature, levels = c("selfID_ethnic","birthparents_ethnicID","skin_tone"))

pF1 <- plot(fig1_data2, group = "outcome", lwd=5.0)
pF1 + xlab("AMCEs") + ggtitle("Racial classification, all respondents") + theme(plot.title = element_text(size=30), axis.title.x = element_text(size=18), axis.text.x=element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.0,0.5,1.0,1.5,2.0), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("White","Black","Latino","Asian","MENA"), values=c("green","black","purple","firebrick","blue")) + theme(legend.position = "none")

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")
grid.brackets(488, 175, 488, 258, lwd=2, col="black")
grid.brackets(666, 465,  666, 375, lwd=2, col="black")
grid.brackets(562, 585,  562, 665, lwd=2, col="black")
grid.text("Skin color*", x=unit(532, 'native'), y=unit(223, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Ancestry", x=unit(325, 'native'), y=unit(425, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Self-ID", x=unit(608, 'native'), y=unit(630, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("White", x=unit(382, 'native'), y=unit(185, 'native'), hjust = 0, vjust=0, gp=gpar(col="blue",fontsize=16))
grid.text("Black", x=unit(385, 'native'), y=unit(204, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Latino", x=unit(377, 'native'), y=unit(224, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("MENA", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))


## Figure 2

# WHITE RESPONDENTS

# for the White_agree model,
# Black/African American is the reference category for birth parents' ethnic ID
# and profile self-ID. "Dark" is the reference category for skin tone
WA_comb_W2 <- cj(r3_White, `White_agree` ~ gender + ethnicity + `skin_tone_rev` + `birthparents_ethnicID_white` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic_white` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# create BA_comb_W, HA_comb_W, AA_comb_W, and MA_comb_W found in the code for Table A4


# row bind models
WA_comb_W2$identifier <- "White agree"
BA_comb_W$identifier <- "Black agree"
HA_comb_W$identifier <- "Hispanic agree"
AA_comb_W$identifier <- "Asian agree"
MA_comb_W$identifier <- "MENA agree"

fig2_dataW <- bind_rows(WA_comb_W2, BA_comb_W, HA_comb_W, AA_comb_W, MA_comb_W, .id = "identifier")
fig2_dataW2 <- fig2_dataW[c(7,9,27,50,68,94,98,116,141,143,161,188,193,211,235),]
fig2_dataW2$feature[fig2_dataW2$feature == "birthparents_ethnicID_white"] <- "birthparents_ethnicID"
fig2_dataW2$feature[fig2_dataW2$feature == "selfID_ethnic_white"] <- "selfID_ethnic"
fig2_dataW2$feature[fig2_dataW2$feature == "skin_tone_rev"] <- "skin_tone"

fig2_dataW2$outcome[fig2_dataW2$outcome == "White_agree"] <- "White"
fig2_dataW2$outcome[fig2_dataW2$outcome == "Black_agree"] <- "Black"
fig2_dataW2$outcome[fig2_dataW2$outcome == "Hispanic_agree"] <- "Latino"
fig2_dataW2$outcome[fig2_dataW2$outcome == "Asian_agree"] <- "Asian"
fig2_dataW2$outcome[fig2_dataW2$outcome == "MENA_agree"] <- "MENA"

fig2_dataW2 <- fig2_dataW2 %>%
  mutate(outcome =  factor(outcome, levels = c("MENA","Asian","Latino","Black","White"))) %>%
  arrange(outcome)
fig2_dataW2$feature <- factor(fig2_dataW2$feature, levels = c("selfID_ethnic","birthparents_ethnicID","skin_tone"))

pF2W <- plot(fig2_dataW2, group = "outcome", lwd=5.0)
pF2W + xlab("AMCEs") + ggtitle("Racial classification, White respondents") + theme(plot.title = element_text(size=30), axis.title.x = element_text(size=18), axis.text.x=element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.0,0.5,1.0,1.5,2.0), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("White","Black","Latino","Asian","MENA"), values=c("green","black","purple","firebrick","blue")) + theme(legend.position = "none")

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")
grid.brackets(488, 175, 488, 258, lwd=2, col="black")
grid.brackets(666, 465,  666, 375, lwd=2, col="black")
grid.brackets(562, 585,  562, 665, lwd=2, col="black")
grid.text("Skin color*", x=unit(532, 'native'), y=unit(223, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Ancestry", x=unit(325, 'native'), y=unit(425, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Self-ID", x=unit(608, 'native'), y=unit(630, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("White", x=unit(382, 'native'), y=unit(185, 'native'), hjust = 0, vjust=0, gp=gpar(col="blue",fontsize=16))
grid.text("Black", x=unit(385, 'native'), y=unit(204, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Latino", x=unit(377, 'native'), y=unit(224, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("MENA", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

# BLACK RESPONDENTS

# for the White_agree model,
# Black/African American is the reference category for birth parents' ethnic ID
# and profile self-ID. "Dark" is the reference category for skin tone
WA_comb_B2 <- cj(r3_Black, `White_agree` ~ gender + ethnicity + `skin_tone_rev` + `birthparents_ethnicID_white` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic_white` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# create BA_comb_B, HA_comb_B, AA_comb_B, and MA_comb_B found in the code for Table A5

# row bind models
WA_comb_B2$identifier <- "White agree"
BA_comb_B$identifier <- "Black agree"
HA_comb_B$identifier <- "Hispanic agree"
AA_comb_B$identifier <- "Asian agree"
MA_comb_B$identifier <- "MENA agree"

fig2_dataB <- bind_rows(WA_comb_B2, BA_comb_B, HA_comb_B, AA_comb_B, MA_comb_B, .id = "identifier")
fig2_dataB2 <- fig2_dataB[c(7,9,27,50,68,94,98,116,141,143,161,188,193,211,235),]
fig2_dataB2$feature[fig2_dataB2$feature == "birthparents_ethnicID_white"] <- "birthparents_ethnicID"
fig2_dataB2$feature[fig2_dataB2$feature == "selfID_ethnic_white"] <- "selfID_ethnic"
fig2_dataB2$feature[fig2_dataB2$feature == "skin_tone_rev"] <- "skin_tone"

fig2_dataB2$outcome[fig2_dataB2$outcome == "White_agree"] <- "White"
fig2_dataB2$outcome[fig2_dataB2$outcome == "Black_agree"] <- "Black"
fig2_dataB2$outcome[fig2_dataB2$outcome == "Hispanic_agree"] <- "Latino"
fig2_dataB2$outcome[fig2_dataB2$outcome == "Asian_agree"] <- "Asian"
fig2_dataB2$outcome[fig2_dataB2$outcome == "MENA_agree"] <- "MENA"

fig2_dataB2 <- fig2_dataB2 %>%
  mutate(outcome =  factor(outcome, levels = c("MENA","Asian","Latino","Black","White"))) %>%
  arrange(outcome)
fig2_dataB2$feature <- factor(fig2_dataB2$feature, levels = c("selfID_ethnic","birthparents_ethnicID","skin_tone"))

pF2B <- plot(fig2_dataB2, group = "outcome", lwd=5.0)
pF2B + xlab("AMCEs") + ggtitle("Racial classification, Black respondents") + theme(plot.title = element_text(size=30), axis.title.x = element_text(size=18), axis.text.x=element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.0,0.5,1.0,1.5,2.0), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("White","Black","Latino","Asian","MENA"), values=c("green","black","purple","firebrick","blue")) + theme(legend.position = "none")

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")
grid.brackets(488, 175, 488, 258, lwd=2, col="black")
grid.brackets(666, 465,  666, 375, lwd=2, col="black")
grid.brackets(562, 585,  562, 665, lwd=2, col="black")
grid.text("Skin color*", x=unit(532, 'native'), y=unit(223, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Ancestry", x=unit(325, 'native'), y=unit(425, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Self-ID", x=unit(608, 'native'), y=unit(630, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("White", x=unit(382, 'native'), y=unit(185, 'native'), hjust = 0, vjust=0, gp=gpar(col="blue",fontsize=16))
grid.text("Black", x=unit(385, 'native'), y=unit(204, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Latino", x=unit(377, 'native'), y=unit(224, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("MENA", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

# HISPANIC RESPONDENTS

# for the White_agree model,
# Black/African American is the reference category for birth parents' ethnic ID
# and profile self-ID. "Dark" is the reference category for skin tone
WA_comb_H2 <- cj(r3_Hisp, `White_agree` ~ gender + ethnicity + `skin_tone_rev` + `birthparents_ethnicID_white` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic_white` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# create BA_comb_H, HA_comb_H, AA_comb_H, MA_comb_H found in the code for Table A6

# row bind models
WA_comb_H2$identifier <- "White agree"
BA_comb_H$identifier <- "Black agree"
HA_comb_H$identifier <- "Hispanic agree"
AA_comb_H$identifier <- "Asian agree"
MA_comb_H$identifier <- "MENA agree"

fig2_dataH <- bind_rows(WA_comb_H2, BA_comb_H, HA_comb_H, AA_comb_H, MA_comb_H, .id = "identifier")
fig2_dataH2 <- fig2_dataH[c(7,9,27,50,68,94,98,116,141,143,161,188,193,211,235),]
fig2_dataH2$feature[fig2_dataH2$feature == "birthparents_ethnicID_white"] <- "birthparents_ethnicID"
fig2_dataH2$feature[fig2_dataH2$feature == "selfID_ethnic_white"] <- "selfID_ethnic"
fig2_dataH2$feature[fig2_dataH2$feature == "skin_tone_rev"] <- "skin_tone"

fig2_dataH2$outcome[fig2_dataH2$outcome == "White_agree"] <- "White"
fig2_dataH2$outcome[fig2_dataH2$outcome == "Black_agree"] <- "Black"
fig2_dataH2$outcome[fig2_dataH2$outcome == "Hispanic_agree"] <- "Latino"
fig2_dataH2$outcome[fig2_dataH2$outcome == "Asian_agree"] <- "Asian"
fig2_dataH2$outcome[fig2_dataH2$outcome == "MENA_agree"] <- "MENA"

fig2_dataH2 <- fig2_dataH2 %>%
  mutate(outcome =  factor(outcome, levels = c("MENA","Asian","Latino","Black","White"))) %>%
  arrange(outcome)
fig2_dataH2$feature <- factor(fig2_dataH2$feature, levels = c("selfID_ethnic","birthparents_ethnicID","skin_tone"))

pF2H <- plot(fig2_dataH2, group = "outcome", lwd=5.0)
pF2H + xlab("AMCEs") + ggtitle("Racial classification, Hispanic respondents") + theme(plot.title = element_text(size=30), axis.title.x = element_text(size=18), axis.text.x=element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.0,0.5,1.0,1.5,2.0), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("White","Black","Latino","Asian","MENA"), values=c("green","black","purple","firebrick","blue")) + theme(legend.position = "none")

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")
grid.brackets(488, 175, 488, 258, lwd=2, col="black")
grid.brackets(666, 465,  666, 375, lwd=2, col="black")
grid.brackets(562, 585,  562, 665, lwd=2, col="black")
grid.text("Skin color*", x=unit(532, 'native'), y=unit(223, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Ancestry", x=unit(325, 'native'), y=unit(425, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Self-ID", x=unit(608, 'native'), y=unit(630, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("White", x=unit(382, 'native'), y=unit(185, 'native'), hjust = 0, vjust=0, gp=gpar(col="blue",fontsize=16))
grid.text("Black", x=unit(385, 'native'), y=unit(204, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Latino", x=unit(377, 'native'), y=unit(224, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("MENA", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

# ASIAN RESPONDENTS

# for the White_agree model,
# Black/African American is the reference category for birth parents' ethnic ID
# and profile self-ID. "Dark" is the reference category for skin tone
WA_comb_A2 <- cj(r3_Asian, `White_agree` ~ gender + ethnicity + `skin_tone_rev` + `birthparents_ethnicID_white` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic_white` + `Sample` + Gender + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")
# create BA_comb_A, HA_comb_A, AA_comb_A, MA_comb_A found in the code for Table A7

# row bind models
WA_comb_A2$identifier <- "White agree"
BA_comb_A$identifier <- "Black agree"
HA_comb_A$identifier <- "Hispanic agree"
AA_comb_A$identifier <- "Asian agree"
MA_comb_A$identifier <- "MENA agree"

fig2_dataA <- bind_rows(WA_comb_A2,BA_comb_A, HA_comb_A, AA_comb_A, MA_comb_A, .id = "identifier")
fig2_dataA2 <- fig2_dataA[c(7,9,27,50,68,94,98,116,141,143,161,188,193,211,235),]
fig2_dataA2$feature[fig2_dataA2$feature == "birthparents_ethnicID_white"] <- "birthparents_ethnicID"
fig2_dataA2$feature[fig2_dataA2$feature == "selfID_ethnic_white"] <- "selfID_ethnic"
fig2_dataA2$feature[fig2_dataA2$feature == "skin_tone_rev"] <- "skin_tone"

fig2_dataA2$outcome[fig2_dataA2$outcome == "White_agree"] <- "White"
fig2_dataA2$outcome[fig2_dataA2$outcome == "Black_agree"] <- "Black"
fig2_dataA2$outcome[fig2_dataA2$outcome == "Hispanic_agree"] <- "Latino"
fig2_dataA2$outcome[fig2_dataA2$outcome == "Asian_agree"] <- "Asian"
fig2_dataA2$outcome[fig2_dataA2$outcome == "MENA_agree"] <- "MENA"

fig2_dataA2 <- fig2_dataA2 %>%
  mutate(outcome =  factor(outcome, levels = c("MENA","Asian","Latino","Black","White"))) %>%
  arrange(outcome)
fig2_dataA2$feature <- factor(fig2_dataA2$feature, levels = c("selfID_ethnic","birthparents_ethnicID","skin_tone"))

pF2A <- plot(fig2_dataA2, group = "outcome", lwd=5.0)
pF2A + xlab("AMCEs") + ggtitle("Racial classification, Asian respondents") + theme(plot.title = element_text(size=30), axis.title.x = element_text(size=18), axis.text.x=element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.0,0.5,1.0,1.5,2.0), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("White","Black","Latino","Asian","MENA"), values=c("green","black","purple","firebrick","blue")) + theme(legend.position = "none")

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")
grid.brackets(488, 175, 488, 258, lwd=2, col="black")
grid.brackets(666, 465,  666, 375, lwd=2, col="black")
grid.brackets(562, 585,  562, 665, lwd=2, col="black")
grid.text("Skin color*", x=unit(532, 'native'), y=unit(223, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Ancestry", x=unit(325, 'native'), y=unit(425, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Self-ID", x=unit(608, 'native'), y=unit(630, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("White", x=unit(382, 'native'), y=unit(185, 'native'), hjust = 0, vjust=0, gp=gpar(col="blue",fontsize=16))
grid.text("Black", x=unit(385, 'native'), y=unit(204, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Latino", x=unit(377, 'native'), y=unit(224, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("MENA", x=unit(408, 'native'), y=unit(244, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

# combine all four of the above figures to make Figure 2
# combined in Adobe Express and added sub-figure labels "A", "B", "C", and "D".
