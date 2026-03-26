# Alexander F. Gazmararian
# agazmararian@gmail.com

library(here)
library(kableExtra)
library(modelsummary)
library(tidyverse)

#Load data
g <- readRDS(here("Data", "inter", "survey", "lucid", "202401_survey.rds"))

#Sample description----
##Get population data----
source(here("Code", "survey_jan24", "census_population.R"))

## Create sample description table ----
# Load shared statistical functions
source(here("Code", "fun", "desc_stat_fun.R"))
#Create summary statistics
sum_tab <- g %>%
  filter(!is.na(vulnindex)) %>%
  datasummary(
    data = .,
    formula = (`Age: 18-24` = age18to24) + (`Age: 25-34` = age25to34) + (`Age: 35 to 44` = age35to44) + (`Age: 45 to 64` = age45to64) + (`Age: over 65` = age65over) +
      (Female = female) + (White = white) + (Black = black) + (`AAPI` = aapi) + (Hispanic = hispanic) + 
      (`High school or less` = eduhsless) + (`Some college` = edusomecol) + (`BA or higher` = eduba) +
      (`Income Q1` = incomeq1) + (`Income Q2` = incomeq2) + (`Income Q3` = incomeq3) + (`Income Q4` = incomeq4) + (`Income Q5` = incomeq5) +
      (`Employed` = employ_bin) + (Student = student) + (Retired = retired) +
      (Rural = rural_bin) + Northeast + Midwest + South + West + 
      (Democrat = dem) + (Republican = rep) ~ Mean + N + Missing,
    output = "data.frame"
  )
#create population values
sum_tab$Pop. <- c(
  age$Freq,
  male$Freq[1],
  race$Freq[4],
  race$Freq[2],
  race$Freq[1],
  .168,#hispanic
  #https://www.census.gov/newsroom/blogs/random-samplings/2023/05/racial-ethnic-diversity-adults-children.html#:~:text=The%20Hispanic%20or%20Latino%20group,of%20the%20under%2D18%20population.
  edu$Freq[2],
  edu$Freq[1],
  edu$Freq[3],
  income_dist$Freq,
  .601, #employed full-time
  #December 2023 seasonally adjusted
  #Employment-population ratio
  #https://www.bls.gov/news.release/empsit.t01.htm
  NA,
  NA,
  .14, #https://www.ers.usda.gov/webdocs/publications/102576/eib-230.pdf
  .17,
  .21,
  .38,
  .24,
  NA,
  NA
)
#make the population column the third column in the table
sum_tab <- sum_tab[,c(1,2,ncol(sum_tab),3:(ncol(sum_tab)-1))]
#rename the mean variable
sum_tab <- sum_tab %>% rename(Sample = Mean, `NA` = Missing)
sum_tab$Pop. <- round(sum_tab$Pop., 2)
#Save table output
kbl(sum_tab, booktabs = TRUE, format = "latex", linesep = "") %>%
  cat(., file = here("Output", "tables", "tab_A1_lucid_sample.txt"))
#Check start and end date of the survey for the table notes
min(as.Date(g$EndDate))
max(as.Date(g$EndDate))
message("Created Table A1. Sample description.")
