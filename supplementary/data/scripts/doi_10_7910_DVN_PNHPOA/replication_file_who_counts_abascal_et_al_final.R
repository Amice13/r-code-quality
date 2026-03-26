################
# The following replication file corresponds to "Who counts as a "person of color"? 
# The role of ancestry, phenotype, self-identification, and other factors" 
# by Maria Abascal, Amada Armenta, and W. M. Halm
# The code was created by William Halm using R 4.4.2 on September 23, 2025.

# This code first loads in and cleans the data set "CleanedPooledREPS_04252023_WH.csv"
# that has been provided for replication.
# The code then creates the tables and figures for the main text
# before creating the tables and figure for the appendix.
# Please note that the figures create labels that can appear quite different 
# depending on the dimensions of your computer screen.
################

# load required packages
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
library(car)
library(ngramr)


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
r3$age <- factor(r3$age, levels = c("17","18","19","20","21"))
r3$occup_status <- factor(r3$occup_status, levels = c("Low","Medium","High"))
r3$Religion <- factor(r3$religion, levels = c("Christian","Muslim","Not available"))
r3$english <- factor(r3$english, levels = c("Don't speak English at home","Speak English at home"))
r3$selfID_ethnic <- factor(r3$selfID_ethnic, levels = c("White (self)","Asian","Black","Hispanic","MENA"))
r3$selfID_ethnic_white <- factor(r3$selfID_ethnic, levels = c("Black","White (self)","Asian","Hispanic","MENA"))

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

## subsets
# four respondent race subsets
r3_White <- subset(r3, resrace=="Wht")
r3_Black <- subset(r3, resrace=="Blk")
r3_Hisp <- subset(r3, resrace=="Hisp")
r3_Asian <- subset(r3, resrace=="Asn")

# Main Text Tables and Figures----
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

r3_stu <- r3_stu %>%
  mutate(resrace = case_when(
    Q3_race_ethnic == 1 ~ "Asian or Asian American",
    Q3_race_ethnic == 2 ~ "Black or African American",
    Q3_race_ethnic == 3 ~ "Hispanic or Latino", 
    Q3_race_ethnic == 4 ~ "Middle Eastern",
    Q3_race_ethnic == 5 ~ "Native American",
    Q3_race_ethnic == 6 ~ "White",
    Q3_race_ethnic == 7 ~ "Another racial or ethnic group",
    TRUE ~ NA_character_  # for any other values
  ))
freq(r3_stu$resrace)

r3_stu <- r3_stu %>%
  mutate(resgender = case_when(
    Q5_gender == 1 ~ "Female",
    Q5_gender == 2 ~ "Male",
    Q5_gender == 3 ~ "Other",
    TRUE ~ NA_character_  # for any other values
  ))
freq(r3_stu$resgender)

r3_stu <- r3_stu %>%
  mutate(reseduc = case_when(
    Q26_level_edu == 1 ~ "Less than high school",
    Q26_level_edu == 2 ~ "High school",
    Q26_level_edu == 3 ~ "Some college",
    Q26_level_edu == 4 ~ "Bachelor's degree or higher",
    TRUE ~ NA_character_  # for any other values
  ))
freq(r3_stu$reseduc)

r3_stu <- r3_stu %>%
  mutate(resparty = case_when(
    Q19_partyID == 1 ~ "Dem",
    Q19_partyID == 2 ~ "Rep",
    Q19_partyID == 3 ~ "Ind",
    Q19_partyID == 4 ~ "Some other party",
    TRUE ~ NA_character_  # for any other values
  ))
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

dyn_unique <- dyn_unique %>%
  mutate(resrace = case_when(
    Q3_race_ethnic == 1 ~ "Asian or Asian American",
    Q3_race_ethnic == 2 ~ "Black or African American",
    Q3_race_ethnic == 3 ~ "Hispanic or Latino", 
    Q3_race_ethnic == 4 ~ "Middle Eastern",
    Q3_race_ethnic == 5 ~ "Native American",
    Q3_race_ethnic == 6 ~ "White",
    Q3_race_ethnic == 7 ~ "Another racial or ethnic group",
    TRUE ~ NA_character_  # for any other values
  ))
freq(dyn_unique$resrace)

dyn_unique <- dyn_unique %>%
  mutate(resgender = case_when(
    Q5_gender == 1 ~ "Female",
    Q5_gender == 2 ~ "Male",
    Q5_gender == 3 ~ "Other",
    TRUE ~ NA_character_  # for any other values
  ))
freq(dyn_unique$resgender)

dyn_unique <- dyn_unique %>%
  mutate(reseduc = case_when(
    Q26_level_edu == 1 ~ "Less than high school",
    Q26_level_edu == 2 ~ "High school",
    Q26_level_edu == 3 ~ "Some college",
    Q26_level_edu == 4 ~ "Bachelor's degree or higher",
    TRUE ~ NA_character_  # for any other values
  ))
freq(dyn_unique$reseduc)

dyn_unique <- dyn_unique %>%
  mutate(resparty = case_when(
    Q19_partyID == 1 ~ "Dem",
    Q19_partyID == 2 ~ "Rep",
    Q19_partyID == 3 ~ "Ind",
    Q19_partyID == 4 ~ "Some other party",
    TRUE ~ NA_character_  # for any other values
  ))
freq(dyn_unique$resparty)

### Both samples' stats + table
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
PA_comb <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "amce")

PA_comb_OL <- PA_comb[,c('feature','level','estimate','std.error','p')]
PA_comb_OL <- dplyr::rename(PA_comb_OL, Variable = feature)
PA_comb_OL <- dplyr::rename(PA_comb_OL, Level = level)
PA_comb_OL <- dplyr::rename(PA_comb_OL, Estimate = estimate)
PA_comb_OL <- dplyr::rename(PA_comb_OL, SE = std.error)

PAcombOL <- print(xtable(PA_comb_OL, caption = "AMCEs for agreement a profile is a person of color, pooled samples. N=23,834"), caption.placement = 'top', include.rownames=FALSE)
# rows reordered, labels cleaned in Overleaf

# Fit stats for the model
PA_comb_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, data = r3)
summary(PA_comb_lm)
# F stat = 129.6; R-squared = 0.1733; Adj = 0.1719

## Table 4
PA_comb_mm <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + resrace + Income + Nativity + `Age` + educ + party, id = ~ `Respondent_ID`, estimate = "mm")

PA_comb_mm_OL <- PA_comb_mm[,c('feature','level','estimate','std.error','p')]
PA_comb_mm_OL <- dplyr::rename(PA_comb_mm_OL, Variable = feature)
PA_comb_mm_OL <- dplyr::rename(PA_comb_mm_OL, Level = level)
PA_comb_mm_OL <- dplyr::rename(PA_comb_mm_OL, Estimate = estimate)
PA_comb_mm_OL <- dplyr::rename(PA_comb_mm_OL, SE = std.error)

PAcombmmOL <- print(xtable(PA_comb_mm_OL, caption = "AMCEs for agreement a profile is a person of color, pooled samples. N=23,834"), caption.placement = 'top', include.rownames=FALSE)
# rows reordered, labels cleaned in Overleaf

# Figure 1
PA_race <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ resrace)
PA_race_white <- PA_race[c(5:12,26:30),]
PA_race_Black <- PA_race[c(103:110,124:128),]
PA_race_Hisp <- PA_race[c(152:159,173:177),]
PA_race_Asian <- PA_race[c(54:61,75:79),]

# figure for White respondents
levels(PA_race_white$feature)[levels(PA_race_white$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_white$feature)[levels(PA_race_white$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_white$feature)[levels(PA_race_white$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_white2 <- PA_race_white %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

pPAW2 <- plot(PA_race_white2, group = "feature", lwd=5.0)
pPAW2
pPAW2 + xlab("AMCEs") + ggtitle("Agreement profile is a person of color, White respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(340, 'native'), y=unit(140, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(600, 'native'), y=unit(390, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(420, 'native'), y=unit(620, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(144, 'native'), y=unit(106, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(200, 'native'), y=unit(149, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(330, 'native'), y=unit(189, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(136, 'native'), y=unit(266, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(575, 'native'), y=unit(307, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(435, 'native'), y=unit(346, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(400, 'native'), y=unit(386, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(432, 'native'), y=unit(424, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(135, 'native'), y=unit(503, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(347, 'native'), y=unit(542, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(280, 'native'), y=unit(583, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(289, 'native'), y=unit(621, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(290, 'native'), y=unit(660, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))

# figure for Black respondents
levels(PA_race_Black$feature)[levels(PA_race_Black$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_Black$feature)[levels(PA_race_Black$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_Black$feature)[levels(PA_race_Black$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_Black2 <- PA_race_Black %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

pPAB2 <- plot(PA_race_Black2, group = "feature", lwd=5.0)
pPAB2
pPAB2 + xlab("AMCEs") + ggtitle("Agreement profile is a person of color, Black respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(340, 'native'), y=unit(140, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(602, 'native'), y=unit(390, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(421, 'native'), y=unit(620, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(144, 'native'), y=unit(106, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(166, 'native'), y=unit(149, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(290, 'native'), y=unit(189, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(136, 'native'), y=unit(266, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(552, 'native'), y=unit(307, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(433, 'native'), y=unit(346, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(385, 'native'), y=unit(386, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(450, 'native'), y=unit(424, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(135, 'native'), y=unit(503, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(300, 'native'), y=unit(542, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(250, 'native'), y=unit(583, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(175, 'native'), y=unit(621, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(225, 'native'), y=unit(660, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))

# figure for Hispanic and Latino respondents
levels(PA_race_Hisp$feature)[levels(PA_race_Hisp$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_Hisp$feature)[levels(PA_race_Hisp$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_Hisp$feature)[levels(PA_race_Hisp$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_Hisp2 <- PA_race_Hisp %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

pPAH2 <- plot(PA_race_Hisp2, group = "feature", lwd=5.0)
pPAH2
pPAH2 + xlab("AMCEs") + ggtitle("Agreement profile is a person of color, Hispanic respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(340, 'native'), y=unit(140, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(602, 'native'), y=unit(390, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(421, 'native'), y=unit(620, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(144, 'native'), y=unit(106, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(220, 'native'), y=unit(149, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(383, 'native'), y=unit(189, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(136, 'native'), y=unit(266, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(525, 'native'), y=unit(307, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(455, 'native'), y=unit(346, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(365, 'native'), y=unit(386, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(447, 'native'), y=unit(424, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(135, 'native'), y=unit(503, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(325, 'native'), y=unit(542, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(300, 'native'), y=unit(583, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(270, 'native'), y=unit(621, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(332, 'native'), y=unit(660, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))

# figure for Asian respondents
levels(PA_race_Asian$feature)[levels(PA_race_Asian$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_Asian$feature)[levels(PA_race_Asian$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_Asian$feature)[levels(PA_race_Asian$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_Asian2 <- PA_race_Asian %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

pAAH2 <- plot(PA_race_Asian2, group = "feature", lwd=5.0)
pAAH2
pAAH2 + xlab("AMCEs") + ggtitle("Agreement profile is a person of color, Asian respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(-0.25,0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00), limits=c(-0.25, 2.0)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(340, 'native'), y=unit(140, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(602, 'native'), y=unit(390, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(421, 'native'), y=unit(620, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(144, 'native'), y=unit(106, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(225, 'native'), y=unit(149, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(355, 'native'), y=unit(189, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(136, 'native'), y=unit(266, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(535, 'native'), y=unit(307, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(480, 'native'), y=unit(346, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(480, 'native'), y=unit(386, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(495, 'native'), y=unit(424, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(135, 'native'), y=unit(503, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(280, 'native'), y=unit(542, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(265, 'native'), y=unit(583, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(285, 'native'), y=unit(621, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(288, 'native'), y=unit(660, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))


## Figure 2
PA_race_mm <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)
PA_race_mm_white <- PA_race_mm[c(5:12,26:30),]
PA_race_mm_Black <- PA_race_mm[c(103:110,124:128),]
PA_race_mm_Hisp <- PA_race_mm[c(152:159,173:177),]
PA_race_mm_Asian <- PA_race_mm[c(54:61,75:79),]

# for White respondents
PA_race_mm_white2 <- PA_race_mm_white %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

# for vertical line
r3_W <- subset(r3, resrace=="Wht")
summary(r3_W$POC_agree)

pPAW <- plot(PA_race_mm_white2, group = "feature", lwd=5.0)
pPAW
pPAW + xlab("Marginal Means") + ggtitle("Agreement profile is a person of color, White respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.y=element_text(size=12), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(2.25,2.50,2.75,3.00,3.25,3.50,3.75,4.00), limits=c(2.25, 4.00)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") +
  geom_vline(xintercept = 3.23, linetype="dashed", 
             color = "gray", linewidth=1.5) + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(375, 'native'), y=unit(250, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(285, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(400, 'native'), y=unit(850, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(655, 'native'), y=unit(160, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(807, 'native'), y=unit(217, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(1047, 'native'), y=unit(275, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(312, 'native'), y=unit(388, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(1183, 'native'), y=unit(445, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(900, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(860, 'native'), y=unit(555, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(927, 'native'), y=unit(612, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(582, 'native'), y=unit(725, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(995, 'native'), y=unit(780, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(850, 'native'), y=unit(837, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(875, 'native'), y=unit(895, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(868, 'native'), y=unit(950, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))

# for Black respondents
levels(PA_race_mm_Black$feature)[levels(PA_race_mm_Black$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_mm_Black$feature)[levels(PA_race_mm_Black$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_mm_Black$feature)[levels(PA_race_mm_Black$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_mm_Black2 <- PA_race_mm_Black %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

# vertical line
r3_B <- subset(r3, resrace=="Blk")
summary(r3_B$POC_agree)

pPAB <- plot(PA_race_mm_Black2, group = "feature", lwd=5.0)
pPAB
pPAB + xlab("Marginal Means") + ggtitle("Agreement profile is a person of color, Black respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.y=element_text(size=12), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(2.25,2.50,2.75,3.00,3.25,3.50,3.75,4.00), limits=c(2.25, 4.00)) +
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") +
  geom_vline(xintercept = 3.06, linetype="dashed", 
             color = "gray", linewidth=1.5) + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(375, 'native'), y=unit(250, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(285, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(400, 'native'), y=unit(850, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(575, 'native'), y=unit(163, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(628, 'native'), y=unit(219, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(910, 'native'), y=unit(275, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(190, 'native'), y=unit(385, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(1025, 'native'), y=unit(443, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(782, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(703, 'native'), y=unit(557, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(818, 'native'), y=unit(612, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(532, 'native'), y=unit(725, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(840, 'native'), y=unit(783, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(770, 'native'), y=unit(840, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(613, 'native'), y=unit(895, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(730, 'native'), y=unit(952, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))

# for Hispanic and Latino respondents
levels(PA_race_mm_Hisp$feature)[levels(PA_race_mm_Hisp$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_mm_Hisp$feature)[levels(PA_race_mm_Hisp$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_mm_Hisp$feature)[levels(PA_race_mm_Hisp$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_mm_Hisp2 <- PA_race_mm_Hisp %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

# vertical line
r3_H <- subset(r3, resrace=="Hisp")
summary(r3_H$POC_agree)

pPAH <- plot(PA_race_mm_Hisp2, group = "feature", lwd=5.0)
pPAH
pPAH + xlab("Marginal Means") + ggtitle("Agreement profile is a person of color, Hispanic respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.y=element_text(size=12), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(2.25,2.50,2.75,3.00,3.25,3.50,3.75,4.00), limits=c(2.25, 4.00))+
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") +
  geom_vline(xintercept = 3.45, linetype="dashed", 
             color = "gray", linewidth=1.5) + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(375, 'native'), y=unit(250, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(285, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(400, 'native'), y=unit(850, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(779, 'native'), y=unit(162, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(969, 'native'), y=unit(218, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(1293, 'native'), y=unit(275, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(495, 'native'), y=unit(388, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(1295, 'native'), y=unit(443, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(1147, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(967, 'native'), y=unit(557, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(1150, 'native'), y=unit(614, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(737, 'native'), y=unit(725, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(1125, 'native'), y=unit(780, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(1050, 'native'), y=unit(840, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(993, 'native'), y=unit(895, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(1125, 'native'), y=unit(950, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))

# for Asian respondents
levels(PA_race_mm_Asian$feature)[levels(PA_race_mm_Asian$feature)=="skin_tone"] <- "Skin Color"
levels(PA_race_mm_Asian$feature)[levels(PA_race_mm_Asian$feature)=="birthparents_ethnicID"] <- "Parents' Background" 
levels(PA_race_mm_Asian$feature)[levels(PA_race_mm_Asian$feature)=="selfID_ethnic"] <- "Self-ID"

PA_race_mm_Asian2 <- PA_race_mm_Asian %>%
  mutate(level =  factor(level, levels = c("MENA", "Asian", "Hispanic", "Black", "White (self)", "Middle Eastern or North African", "Asian/Asian-American", "Hispanic/Latino(a/x)", "Black/African-American", "White (parents)", "Dark", "Medium ", "Light"))) %>%
  arrange(level)

# vertical line
r3_A <- subset(r3, resrace=="Asn")
summary(r3_A$POC_agree)

pPAA <- plot(PA_race_mm_Asian2, group = "feature", lwd=5.0)
pPAA
pPAA + xlab("Marginal Means") + ggtitle("Agreement profile is a person of color, Asian respondents") + theme(plot.title = element_text(size=28), axis.title.x = element_text(size=18), axis.text.y=element_text(size=12), axis.text.x = element_text(size=16)) + scale_x_continuous(breaks=c(2.25,2.50,2.75,3.00,3.25,3.50,3.75,4.00), limits=c(2.25, 4.00))+
  scale_y_discrete(labels = NULL, breaks = NULL) + scale_color_manual(labels=c("Skin Color","Parents' Background","Self-ID"), values=c("black","purple","firebrick")) + theme(legend.position = "none") +
  geom_vline(xintercept = 3.57, linetype="dashed", 
             color = "gray", linewidth=1.5) + geom_point(size=2)

### the placement of the following labels will vary widely from computer to computer
### grid.locator can help with refining the location of labels/brackets
# grid.locator(unit="native")

grid.text("Skin Color", x=unit(375, 'native'), y=unit(250, 'native'), hjust = 0, vjust=0, gp=gpar(fontsize=18))
grid.text("Parents' Background", x=unit(285, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=18))
grid.text("Self-ID", x=unit(400, 'native'), y=unit(850, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=18))

grid.text("Light", x=unit(895, 'native'), y=unit(160, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Medium", x=unit(1088, 'native'), y=unit(218, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))
grid.text("Dark", x=unit(1353, 'native'), y=unit(275, 'native'), hjust = 0, vjust=0, gp=gpar(col="black",fontsize=16))

grid.text("White", x=unit(550, 'native'), y=unit(388, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Black", x=unit(1322, 'native'), y=unit(443, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Hispanic", x=unit(1210, 'native'), y=unit(500, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("Asian", x=unit(1218, 'native'), y=unit(555, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))
grid.text("MENA", x=unit(1215, 'native'), y=unit(612, 'native'), hjust = 0, vjust=0, gp=gpar(col="purple",fontsize=16))

grid.text("White", x=unit(865, 'native'), y=unit(725, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Black", x=unit(1152, 'native'), y=unit(782, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Hispanic", x=unit(1168, 'native'), y=unit(840, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("Asian", x=unit(1173, 'native'), y=unit(895, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))
grid.text("MENA", x=unit(1180, 'native'), y=unit(950, 'native'), hjust = 0, vjust=0, gp=gpar(col="firebrick",fontsize=16))


# Appendix Tables and Figure----

## Table A1
PA_race <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ resrace)

PA_race_W_OL <- PA_race[1:49,c('feature','level','estimate','std.error')]
PA_race_W_OL <- dplyr::rename(PA_race_W_OL, Variable = feature)
PA_race_W_OL <- dplyr::rename(PA_race_W_OL, Level = level)
PA_race_W_OL <- dplyr::rename(PA_race_W_OL, Estimate = estimate)
PA_race_W_OL <- dplyr::rename(PA_race_W_OL, SE = std.error)
PA_race_A_OL <- PA_race[50:98,c('feature','level','estimate','std.error')]
PA_race_A_OL <- dplyr::rename(PA_race_A_OL, Variable = feature)
PA_race_A_OL <- dplyr::rename(PA_race_A_OL, Level = level)
PA_race_A_OL <- dplyr::rename(PA_race_A_OL, Estimate = estimate)
PA_race_A_OL <- dplyr::rename(PA_race_A_OL, SE = std.error)
PA_race_B_OL <- PA_race[99:147,c('feature','level','estimate','std.error')]
PA_race_B_OL <- dplyr::rename(PA_race_B_OL, Variable = feature)
PA_race_B_OL <- dplyr::rename(PA_race_B_OL, Level = level)
PA_race_B_OL <- dplyr::rename(PA_race_B_OL, Estimate = estimate)
PA_race_B_OL <- dplyr::rename(PA_race_B_OL, SE = std.error)
PA_race_H_OL <- PA_race[148:196,c('feature','level','estimate','std.error')]
PA_race_H_OL <- dplyr::rename(PA_race_H_OL, Variable = feature)
PA_race_H_OL <- dplyr::rename(PA_race_H_OL, Level = level)
PA_race_H_OL <- dplyr::rename(PA_race_H_OL, Estimate = estimate)
PA_race_H_OL <- dplyr::rename(PA_race_H_OL, SE = std.error)

PA_race_OL <- cbind(PA_race_W_OL, PA_race_B_OL, PA_race_H_OL, PA_race_A_OL)
PA_race_OL <- PA_race_OL[,-c(5,6,9,10,13,14)]

PAraceOL <- print(xtable(PA_race_OL, caption = "AMCEs for agreement a profile is a person of color, by respondent race"), caption.placement = 'top', include.rownames=FALSE)
# rows reordered, labels cleaned in Overleaf

# Fit statistics for each respondent race model
PA_Wht_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, data = r3_White)
summary(PA_Wht_lm)
PA_Blk_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, data = r3_Black)
summary(PA_Blk_lm)
PA_Hisp_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, data = r3_Hisp)
summary(PA_Hisp_lm)
PA_Asn_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, data = r3_Asian)
summary(PA_Asn_lm)


# Table A2
PA_race_mm <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                   `selfID_ethnic` + `Sample` + Gender + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `resrace`)

PA_race_Wm_OL <- PA_race_mm[1:49,c('feature','level','estimate','std.error')]
PA_race_Wm_OL <- dplyr::rename(PA_race_Wm_OL, Variable = feature)
PA_race_Wm_OL <- dplyr::rename(PA_race_Wm_OL, Level = level)
PA_race_Wm_OL <- dplyr::rename(PA_race_Wm_OL, Est = estimate)
PA_race_Wm_OL <- dplyr::rename(PA_race_Wm_OL, SE = std.error)
PA_race_Am_OL <- PA_race_mm[50:98,c('feature','level','estimate','std.error')]
PA_race_Am_OL <- dplyr::rename(PA_race_Am_OL, Variable = feature)
PA_race_Am_OL <- dplyr::rename(PA_race_Am_OL, Level = level)
PA_race_Am_OL <- dplyr::rename(PA_race_Am_OL, Est = estimate)
PA_race_Am_OL <- dplyr::rename(PA_race_Am_OL, SE = std.error)
PA_race_Bm_OL <- PA_race_mm[99:147,c('feature','level','estimate','std.error')]
PA_race_Bm_OL <- dplyr::rename(PA_race_Bm_OL, Variable = feature)
PA_race_Bm_OL <- dplyr::rename(PA_race_Bm_OL, Level = level)
PA_race_Bm_OL <- dplyr::rename(PA_race_Bm_OL, Est = estimate)
PA_race_Bm_OL <- dplyr::rename(PA_race_Bm_OL, SE = std.error)
PA_race_Hm_OL <- PA_race_mm[148:196,c('feature','level','estimate','std.error')]
PA_race_Hm_OL <- dplyr::rename(PA_race_Hm_OL, Variable = feature)
PA_race_Hm_OL <- dplyr::rename(PA_race_Hm_OL, Level = level)
PA_race_Hm_OL <- dplyr::rename(PA_race_Hm_OL, Est = estimate)
PA_race_Hm_OL <- dplyr::rename(PA_race_Hm_OL, SE = std.error)

PA_raceM_OL <- cbind(PA_race_Wm_OL, PA_race_Bm_OL, PA_race_Hm_OL, PA_race_Am_OL)
PA_raceM_OL <- PA_raceM_OL[-c(5,6,9,10,13,14)]

PAracemOL <- print(xtable(PA_raceM_OL, caption = "Marginal means for agreement a profile is a person of color, by respondent race"), caption.placement = 'top', include.rownames=FALSE)
# rows reordered, labels cleaned in Overleaf

# Table A3
PA_samp <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "amce", by = ~ Sample)

PA_samp_S_OL <- PA_samp[1:51,c('feature','level','estimate','std.error','p')]
PA_samp_S_OL <- dplyr::rename(PA_samp_S_OL, Variable = feature)
PA_samp_S_OL <- dplyr::rename(PA_samp_S_OL, Level = level)
PA_samp_S_OL <- dplyr::rename(PA_samp_S_OL, Estimate = estimate)
PA_samp_S_OL <- dplyr::rename(PA_samp_S_OL, SE = std.error)
PA_samp_D_OL <- PA_samp[52:102,c('feature','level','estimate','std.error','p')]
PA_samp_D_OL <- dplyr::rename(PA_samp_D_OL, Variable = feature)
PA_samp_D_OL <- dplyr::rename(PA_samp_D_OL, Level = level)
PA_samp_D_OL <- dplyr::rename(PA_samp_D_OL, Estimate = estimate)
PA_samp_D_OL <- dplyr::rename(PA_samp_D_OL, SE = std.error)

PA_samp_OL <- cbind(PA_samp_D_OL, PA_samp_S_OL)
PA_samp_OL <- PA_samp_OL[,-c(6,7)]

PAsampOL <- print(xtable(PA_samp_OL, caption = "AMCEs for agreement a profile is a person of color, by sample"), caption.placement = 'top', include.rownames=FALSE)
# rows reordered, labels cleaned in Overleaf

# Fit statistics for each respondent sample model
PA_Dyn_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, data = r3_dyn)
summary(PA_Dyn_lm)
PA_Stu_lm <- lm(`POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                  `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, data = r3_stu)
summary(PA_Stu_lm)

# Table A4
PA_Sample_mm <- cj(r3, `POC_agree` ~ gender + ethnicity + `skin_tone` + `birthparents_ethnicID` + age + `occup_status` + Religion + english + 
                     `selfID_ethnic` + Gender + resrace + Income + Nativity + `Age` + educ + party + Condition, id = ~ `Respondent_ID`, estimate = "mm", by = ~ `Sample`)

PA_samp_Sm_OL <- PA_Sample_mm[1:51,c('feature','level','estimate','std.error','p')]
PA_samp_Sm_OL <- dplyr::rename(PA_samp_Sm_OL, Variable = feature)
PA_samp_Sm_OL <- dplyr::rename(PA_samp_Sm_OL, Level = level)
PA_samp_Sm_OL <- dplyr::rename(PA_samp_Sm_OL, Estimate = estimate)
PA_samp_Sm_OL <- dplyr::rename(PA_samp_Sm_OL, SE = std.error)

PA_samp_Dm_OL <- PA_Sample_mm[52:102,c('feature','level','estimate','std.error','p')]
PA_samp_Dm_OL <- dplyr::rename(PA_samp_Dm_OL, Variable = feature)
PA_samp_Dm_OL <- dplyr::rename(PA_samp_Dm_OL, Level = level)
PA_samp_Dm_OL <- dplyr::rename(PA_samp_Dm_OL, Estimate = estimate)
PA_samp_Dm_OL <- dplyr::rename(PA_samp_Dm_OL, SE = std.error)

PA_sampM_OL <- cbind(PA_samp_Dm_OL, PA_samp_Sm_OL)
PA_sampM_OL <- PA_sampM_OL[,-c(6,7)]

PAsampMOL <- print(xtable(PA_sampM_OL, caption = "Marginal means for agreement a profile is a person of color, by sample"), caption.placement = 'top', include.rownames=FALSE)
# rows reordered, labels cleaned in Overleaf

# Figure A1
n2 <- ggram(
  c("people of color","person of color"),
  ignore_case = TRUE,
  geom = "line",
  geom_options = list(linewidth = 1.25),
  lab = NA,
  google_theme = FALSE
)
n2

n2 + xlab("Year") + ylab("Frequency") + ggtitle('Google Books Ngram Plot for "PoC"') + theme(plot.title = element_text(size=30), axis.title.x = element_text(size=14), axis.text.x = element_text(size=12), axis.title.y=element_text(size=14), axis.text.y=element_text(size=12))+
  scale_color_manual(values=c('darkorchid4',"steelblue"))
