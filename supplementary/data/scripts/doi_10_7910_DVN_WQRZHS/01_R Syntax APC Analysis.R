### Syntax Liberal Notions of Democracy in Germany
### APC-Analysis for different aspects of democratic support


#Get all packages with the version used for this analysis
renv::restore()

####Reading the data and loading the packages#####


if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(tidyverse, haven, mgcv, itsadug, lme4, sjPlot, gridExtra, grid, rlist, texreg, tidymv, psych, GPArotation, rtf, mgcViz, gratia, here)

SYNTAX_DIR <- here::here()

setwd(SYNTAX_DIR)

dat <- haven::read_dta(paste0("data_supportdemocracy_ger.dta"))


####Data Manipulation####

datComplete <- dat %>% 
  mutate(idea_democracy = round(idea_democracy, 2),
         libdem_opposition = round(libdem_opposition, 2),
         libdem_freeopinion = round(libdem_freeopinion, 2),
         libdem_partyopportunity = round(libdem_partyopportunity, 2),
         libdem_criticism = round(libdem_criticism, 2),
         libdem_conflict = round(libdem_conflict, 2),
         libdem_generalwill = round(libdem_generalwill, 2),
         satisfaction_democracy = round(satisfaction_democracy, 2),
         period = surveyyear,
         birthyear = period - age) %>% 
  mutate(born_adult = birthyear + 18) %>% 
  mutate(age_grp = ifelse(age >= 15 & age <20, 1, ifelse(
    age >= 20 & age < 30,  2, ifelse(
      age >= 30 & age < 40, 3, ifelse(
        age >= 40 & age < 50,  4, ifelse(
          age >= 50 & age < 60,  5, ifelse(
            age >= 60 & age < 70,  6, ifelse(
              age >= 70 & age < 80,  7,  8
            )
          )
        )
      )
    )
  )), 
  age_grp_centered = age_grp - mean(age_grp, na.rm = T),
  born_adult_centered = born_adult - mean(born_adult, na.rm = T),
  age_grp1 = ifelse(age >= 15 & age < 30, 1,0),     ##Age groups for GAM model
  age_grp2 = ifelse(age >= 30 & age < 60, 1, 0),
  period.f = as.factor(period))

datComplete <- datComplete %>% 
  filter(age >= 16 & age <=95) %>% 
  mutate(age_cohort_five = findInterval(born_adult, c(seq(1910, 2020, by = 5))))  ##cohort groups for rHAPC

#Create subfolders for output
dir.create("plots", showWarnings = FALSE)
dir.create("plots/Appendix", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)

###APC-Analysis Diffuse Support 
source("02_R_Syntax_diffuse_support_revision.R")

