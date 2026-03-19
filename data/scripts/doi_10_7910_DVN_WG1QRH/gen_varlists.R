#### purpose: generating variable list #### 

#### installing key packages #### 

list.of.packages = 
  c('readstata13', 'haven', 'tidyverse', 'dplyr', 'estimatr', 'texreg',
    'gridExtra', 'ggthemes', 'wCorr', 'questionr', 'xtable', 'sf', 
    'TAM', 'purrr', 'kable', 'kableExtra', 'wCorr', 'psych',
    'psychTools')
new.packages =  list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#### libraries #### 

suppressPackageStartupMessages(
  
  {
    
    library(readstata13)    
    library(haven)
    library(tidyverse)
    library(dplyr)
    library(estimatr)
    library(texreg)
    library(gridExtra)
    library(ggthemes)
    library(wCorr)
    library(questionr)
    library(xtable)
    library(sf)
    library(TAM)
    library(purrr)
    library(kable)
    library(kableExtra)
    library(wCorr)
    library(psych)
    library(psychTools)
    
  }
  
)

#### cmps '16 --- loading datasets #### 

load(file = "cmps_lat.RData")
load(file = "cmps_wht.RData")
load(file = "cmps_blk.RData")

#### cmps '20 --- loading datasets #### 

load(file = "cmps20w_clean.RData")
load(file = "cmps20b_clean.RData")
load(file = "cmps20l_clean.RData")
# mean(cmps20w$blm_ft)
# mean(cmps20b$blm_ft)
# mean(cmps20l$blm_ft)

# quick fix for missingness 

cmps20l$cath = ifelse(is.na(cmps20l$cath), 0, cmps20l$cath)

# another quick fix on dtp

cmps20l$dtps2 = (cmps20l$dtp + cmps20l$dtp2 + cmps20l$dtp3) / 3
cmps20w$dtps2 = (cmps20w$dtp + cmps20w$dtp2 + cmps20w$dtp3) / 3
cmps20b$dtps2 = (cmps20b$dtp + cmps20b$dtp2 + cmps20b$dtp3) / 3

#### more fixes #### 


# 2016 fixes --- recode so that outcomes = 1) oppose BLM and 2) belief BLM ineffective

cmps16$supp_blm  = 1 - cmps16$supp_blm 
cmps16$blm_eff = 1 - cmps16$blm_eff 

cmps16b$supp_blm  = 1 - cmps16b$supp_blm 
cmps16b$blm_eff = 1 - cmps16b$blm_eff

cmps16w$supp_blm  = 1 - cmps16w$supp_blm 

# 2020 fixes --- recode so 1 = anti-black 

# cmps20l$blm_supports = 1 - cmps20l$blm_supports
# cmps20l$blm_eff = 1 - cmps20l$blm_eff
# cmps20l$white_hood_rank_diff = cmps20l$white_hood_rank - cmps20l$black_hood_rank
# cmps20l$dtp = 1 - cmps20l$dtp
# cmps20l$dtp3 = 1 - cmps20l$dtp3
# cmps20l$dtps = (cmps20l$dtp + cmps20l$dtp3) / 2
# cmps20l$polint = abs(cmps20l$Q29 - 4)
# cmps20l$lmhhi = log(cmps20l$mhhi + 1)
# cmps20l$lmhhi_cty = log(cmps20l$mhhi_cty + 1)
# cmps20l$dep_rate = (cmps20l$total / ((cmps20l$pfb_cty / 100) * exp(cmps20l$ltpop_cty))) * 1000
cmps16$threat = cmps16$worry
cmps16$ltpop = log(cmps16$tpop + 1)
cmps16$lmhhi = log(cmps16$mhhi + 1)

#### identifying key covars #### 

data.frame(
  survey_name = colnames(cmps16)[607:length(colnames(cmps16))],
  name = c("Deportation Threat", "Support BLM Scale", "Republican", "Democrat", "Independnet",
  "Support Apologizing 4 Slavery", "Political Discrimination", "Perceived Discrimination (Black People)",
  "Perceived Discrimination (Latinos)", "Racial Profiling", "Ideology (Missing)", "Ideology", "Know Undocumented",
  "BLM Effective", "Black/Latino Commonality", "Latino Identity Centrality", "Spanish Interview", "Foreign-born",
  "Citizen", "US-Born", "English Interview", "Parents US-Born", "Parents Foreign-Born", "Second Generation",
  "Third Generation", "Acculturation", "Acculturation 2", "Mexican", "Dominican", "Cuban",
  "Puerto Rican", "Salvadorean", "Experienced DIscrimination", "Threat (Police)", "American Centrality",
  "Skin Color", "Support Citizen Pathway", "Support Undocumented Leaving", "Support Decreasing Border SPpppending",
  "Open Immigration Policy Index", "Ban Gay Marriage", "Support Climate Change Policy", "Support Taxing Rich",
  "Support Voter ID", "Support Obamacare", "Liberalism Index", "Black Street Race", "Support Immigrant Removal",
  "Black Self-Categorization", "Afro-Latino",
  "Black Latino", "Woman", "Minority Linked Fate (No Response)", "Minority Linked Fate",
  "Political Interest", "Catholic", "Married", "Unemployed", "Homeowner", "Income (Refused)",
  "Income", "Education", "Linked Fate (Latino)", "Black Neighbors", "Black Neighbors (Missing)",
  "Aware of BLM", "Economy Getting Worse", "Protest Behavior", "Racism is a Problem", "Work With Black People",
  "Lean Repulbican", "Lean Democrat", "Pure Independent", "Republicans (With Leaners)", "Democrats (With Leaners)",
  "Married w/ Black Spouse", "Married w/ White Spouse", "Political Competition", "Census Region (West)",
  "Census Region (North Central)",  "Census Region (South)",  "Census Region (Northeast)",
  "Church Attendance (Missing)", "% Black (Church)", "Commonality With Black People", "BLM Effectie 2",
  "Support BLM 2", "Oppose LGBT", "Geometry", "Missing County", "Total Population (County)",
  'Population Density (County)', "% Latino (County)", "% Black (County)", "% College (County)",
  "% Foreign (County)", "% Noncitizen (County)", "Median Household Income (County)", "% Unemployed (County)",
  "Total Population (Zipcode)", "Population Density (Zipcode)", "% College (Zipcode)",
  "% Foreign (Zipcode)", "% Latino (Zipcode)", "% Black (Zipcode)", "% Noncitizen (Zipcode)",
  "Median Household Income (Zipcode)", "% Unemployed (Zipcode)", "Log(Total Pop.) (Zipcode)",
  "Log(Median Household Income) (Zipcode)", "Missing Zipcode", "State", "County", "Level 3 SC Deportations",
  "Level 2 SC Deportations", "Level 1 SC Deportations", "Total Deportations", "% Level 1 Deportations",
  "% Level 1/2 Deportations", "% Level 3 Deportations", "Log(Total Deportations)", "Deportation Rate",
  '% college (black) (county)', '% college (latino) (county)', '% college (latino advantage) (county)',
  '% college (latino advantage) (missing, county)', "% poverty (latino advantage, county)", 
  "% unemployed (latino advantage, county)", "% poverty (latino advantage, county, missing)", 
  "% unemployed (latino advantage, county, missing)", "% poverty (latino advantage, zipcode, missing)",
  "% poverty (latino advantage, zipcode)", "% unemployed (latino advantage, zipcode)",
  "% college (latino, zipcode)))", "% college (latino advantage, zipcode)))",
  "% college (latino advantage, zipcode, missing)))", "% poverty (latino advantage, zipcode)",
  "% unemployed (latino advantage, zipcode)", "% poverty (latino advantage, zipcode, missing)",
  "% unemployed (latino advantage, zipcode, missing)", "missing advantage", "skin color missing",
  "age missing", "deportation threat")
) %>% 
  `colnames<-` (c("Survey Data Name", "Full Name")) %>% 
  write.csv(x = ., file = "cmps16varnames.csv")


data.frame(
  survey_name = colnames(cmps20l)[157:length(colnames(cmps20l))],
  name = c("English Interview", "Black ID", "AfroLatino", "Woman", "Age", "US Born",
           "Mexican", "Puerto Rican", "Dominican", "Salvadorean", "Cuban", "Education", "Republican",
           "Democrat", "Independent", "Resentment (Special Favors)", "Resentment (Slavery)", "Resentment (Deserve)",
           "Resentment (Work Hard)", "Strong Partisan", "Lean Republican", "Lean Democrat", "Pure Independent",
           "Party ID (7pt)", "Catholic", "Immigration (ICE)", "Immigration (Border)", "Immigration (Citizenship Pathway)",
           "Immigration (Birthright)", "Immigration (Visa)", "Immigration (No Collaboration)", "Racial Resentment", 
           "Skin Color", "Experienced Discrimination", "Immigration (Undocumented)", "BLM Therm.", "BLM Protest",
           "BLM Support 1", "BLM Support 2", "BLM Support Scale", "Know Deportee", "Know Undocumented", "Deportation Threat",
           "Deportation Threat 2", "Immigrants Latino Stereotype", "Perceived Discrimination (Latino, Refused)",
           "Perceived Discrimination (Latino)", "Perceived Discrimination (Black, Refused)",
           "Perceived Discrimination (Black)",
           "Experienced Discrimination",
           "Married", "Citizen", "Third Generation", "Income (Refused)", "Income", "Unemployed", "Homeowner",
           "Black Latino", "Generational Status", "Acculturation", "Immigration Index", "Ideology (Refused)", "Ideology",
           "Black Stereotype", "White Stereotype", "Defund the Police", "BLM Effective", "BLM Non-Protest",
           "BLM Unfair", "Political Brutality", "Defund the Police 2", "Defund the Police 3", "White Identity",
           "White Neighborhood Rank", "Latinx Neighborhood Rank", "Black Neighborhood Rank", "Black Commonality",
           "Black Stereotype Difference", "Identity Centrality", "Black Threat", "% Black Perceived", "Married (w/White Person)",
           "Married (w/Black Person)", "Linked Fate (Latino)", "American Identity", "Latino Economic Status Worse",
           "Afraid of Economy Getting Worse", "Afraid of Personal Economic Situation", "Political Competition",
           "Census Region (West)", "Census Region (North Central)", "Census Region (South)",
           "Census Region (Northeast)", "Immigrant Work Ethic", "Protestant Work Ethic", "Log(Total Population) (Zipcode)",
           "% College (Zipcode)", "% Unemployed (Zipcode)", "% Poverty (Latino)", "% Poverty (Black)",
           "% Latino (Zipcode)", "% Black (Zipcode)", "Median Household Income (Zipcode)",
           "% Poverty (Latino Advantage, Zipcode)", "Missing Zipcode", "% Foreign Born",
           "% Noncitizen", "Missing County", "% Foreign Born (County)", "% Noncitizen (County)",
           "Log(Total Population) (County)", "% Unemployed (County)", "% Latino (County)",
           "% Black (County)", "Median Household Income (County)", "Missing County",
           "State", "County", "Level 3 SC Deportations",
           "Level 2 SC Deportations", "Level 1 SC Deportations", "Total Deportations",
           "% Level 1 SC Deportations", "% Level 1/2 SC Deportations",
           "% Level 3 SC Deportations", "Total Population (County)", "Log(Total Deportations)",
           "Deportation Rate", "White Neighborhood Rank Difference 1", 
           "White Neighborhood Rank Difference 2", "Latinx Neighborhood Rank Difference",
           "Defund the Police Scale", "Political Interest", "Log(Median Household Income) (Zipcode)",
           "Log(Median Household Income) (County)", "Neighborhood Service Good",
           "Neighborhood Rating", "Perceived Asian Threat", "Perceived Jewish Threat", "Muslim Resentment",
           "Sexism Scale", "Ambivalent Sexism", "Black Threat 2", "White Threat 2", "Vote Democrat",
           "Voted", "Defund the Police Scale 2")
 ) %>% 
  `colnames<-` (c("Survey Data Name", "Full Name")) %>% 
  write.csv(x = ., file = "cmps20varnames.csv")
