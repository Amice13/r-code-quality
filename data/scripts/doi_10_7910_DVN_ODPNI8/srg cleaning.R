### gubitz survey 

### Overview 

#### In this document, I have taken the responses from the survey, and reshaped them so that each respondent has six rows--one row for each task they were given in the conjoint design

## Load Packages
library(skimr)
library(rlang)
library(cjoint)
library(car)
library(weights)
library(tidyverse)

## Load Data and initial clean up
civility_dat <- read_csv("Gubitz raw data.csv") %>% 
  rename(negparty_dem = negparty_1,
         negparty_rep = negparty_2) %>% 
  #removing those who did not consent; those who are not white; and those who are latino
  filter(consent == 1 & race == 1 & Latino == 2) %>%
  #removing variables which are not needed for analyses 
  select(-starts_with("Q"))

### Getting a sense of the variables left
names(civility_dat)


## Reshape and clean

civility_reshape <- civility_dat %>% 
  #first step is gathering all the variables that we want to use as our attributes
  gather(key = task, value = score, contains("_1"), contains("_2"), contains("_3"), contains("_4"), contains("_5"), contains("_6")) %>% 
  #we need to separate these variables out by characteristics; this can be done using `separate`, because variable were meaningfully named using underscores 
  separate(task, c("variable", "iteration", "index")) %>% 
  #creating an indicator of variables that were a part of an index
  mutate(index = ifelse(is.na(index), "", index))  %>%
  #create a variable specific column
  unite("variable_index", c("variable", "index"), sep = "") %>% 
  #spreading out the variables so that each meaningful variable (or attribute) is a column
  spread(key = variable_index, value = score) %>% 
  #creating an indicator for whether or not it was a symmetric interaction
  mutate(news = ifelse(is.na(newssym), news, newssym),
         uncivil = ifelse(is.na(uncivilsym), uncivil, uncivilsym),
         PANAS1 = ifelse(is.na(PANASsym1), PANAS1, PANASsym1),
         PANAS2 = ifelse(is.na(PANASsym2), PANAS2, PANASsym2),
         PANAS3 = ifelse(is.na(PANASsym3), PANAS3, PANASsym3),
         PANAS4 = ifelse(is.na(PANASsym4), PANAS4, PANASsym4),
         PANAS5 = ifelse(is.na(PANASsym5), PANAS5, PANASsym5),
         PANAS6 = ifelse(is.na(PANASsym6), PANAS6, PANASsym6),
         sym = ifelse(is.na(newssym), 0, 1)) %>% 
  #getting rid of the symmetric variables, because they are now captured by the indicator
  select(-PANASsym1, -PANASsym2, -PANASsym3, -PANASsym4, -PANASsym5, -PANASsym6, -uncivilsym, -newssym)

civility_reshape$Police

## Saving the CSV file for future use 
#write_csv(civility_reshape, "Gubitz reshaped.csv")

## Testing, each respondent should have 6 rows--and they do!
testing <- civility_reshape %>% filter(RESPONDENT_ID == "iP00d56b321f43cc32") 
  
#### Cleaning 20191105 

## load cleaned data and use clean names for variable names
civility_reshape <- read_csv("Gubitz reshaped.csv") %>% 
  janitor::clean_names()

## Let's take a look at which variables we have
civility_reshape %>% names()



civility_cleaned <- civility_reshape %>% 
  mutate(police = ifelse(police == "Details From Recent Townhall Meeting", 0, 1), 
         incivility = case_when(incivility == "an 'idiot'" | incivility == "a 'lunatic'" | incivility == "a 'moron'" | incivility == "an 'asshole'" | incivility == "a 'shit head'"  | incivility == "a 'bitch'"  | incivility == "a 'bastard'" ~ "insult", 
                                incivility == "going to 'get punched'" | incivility == "going to 'roughed up' by the crowd" | incivility == "going to 'get hurt by someone'" | incivility == "going to be 'dealt with'" ~ "threats", 
                                incivility == "a 'cunt'" | incivility == "a 'prick'" | incivility == "an 'N word'" | incivility == "a 'cracker'" ~ "slurs") %>% as.factor(),
         t_race = as.factor(t_race) %>% factor(levels = c("White", "Black")),
         t_gender = as.factor(t_gender) %>% factor(levels = c("Male", "Female")),
         t_party = as.factor(t_party), 
         t_job = case_when(t_job == "Representative" ~ "elite",
                           TRUE ~ "non-elite") %>% as.factor(),
         s_race = as.factor(s_race) %>% factor(levels = c("White", "Black")),
         s_gender = as.factor(s_gender) %>% factor(levels = c("Male", "Female")),
         s_party = as.factor(s_party), 
         s_job = case_when(s_job == "Representative" ~ "elite",
                           TRUE ~ "non-elite") %>% as.factor(),
         h1b = case_when(s_job == "non-elite" & t_job == "elite" ~ 1, 
                      s_job == "elite" & t_job == "elite" ~ 0) %>% as.numeric(), 
         pid = case_when(piddem == 1 ~ 1, 
                         piddem == 2 ~ 2,
                         pidlean == 1 ~ 3, 
                         pidlean == 3 ~ 4,
                         pidlean == 2 ~ 5, 
                         pidrep == 2 ~ 6,
                         pidrep == 1 ~ 7),
         pid2 = case_when(pid < 4 ~ "Democratic", 
                           pid > 4 ~ "Republican" ) %>% as.factor(), 
         s_inparty = case_when(pid2 == s_party ~ 1,
                               TRUE ~ 0), 
         t_inparty = case_when(pid2 == t_party ~ 1,
                               TRUE ~ 0),
         h2a_party = case_when(s_inparty == 0 & t_inparty == 1 ~ 1,
                               s_inparty == 1 & t_inparty == 1 ~ 0), 
         h2b_party = case_when(s_inparty == 0 & t_inparty == 1 ~ 1,
                               s_inparty == 1 & t_inparty == 0 ~ 0),          
         h2c_party = case_when(s_inparty == 1 & t_inparty == 0 ~ 1,
                               s_inparty == 1 & t_inparty == 1 ~ 0),
         h2d_party = case_when(s_inparty == 1 & t_inparty == 1 ~ 1, 
                               s_inparty == 0 & t_inparty == 0 ~ 0), 
         gender = case_when(gender == 1 ~ "Male", 
                            gender == 2 ~ "Female") %>% as.factor() %>% factor(levels = c("Male", "Female")),
         s_ingender = case_when(gender == s_gender ~ 1, 
                                TRUE ~ 0),
         t_ingender = case_when(gender == t_gender ~ 1, 
                                TRUE ~ 0), 
         h2a_gender = case_when(s_ingender == 0 & t_ingender == 1 ~ 1,
                                s_ingender == 1 & t_ingender == 1 ~ 0), 
         h2b_gender = case_when(s_ingender == 0 & t_ingender == 1 ~ 1,
                                s_ingender == 1 & t_ingender == 0 ~ 0),          
         h2c_gender = case_when(s_ingender == 1 & t_ingender == 0 ~ 1,
                                s_ingender == 1 & t_ingender == 1 ~ 0),
         h2d_gender = case_when(s_ingender == 1 & t_ingender == 1 ~ 1,
                                s_ingender == 0 & t_ingender == 0 ~ 0), 
         h2a_race = case_when(s_race == "Black" & t_race == "White" ~ 1,
                              s_race == "White" & t_race == "White" ~ 0), 
         h2b_race = case_when(s_race == "Black" & t_race == "White" ~ 1,
                              s_race == "White" & t_race == "Black" ~ 0),          
         h2c_race = case_when(s_race == "White" & t_race == "Black" ~ 1,
                              s_race == "White" & t_race == "White" ~ 0),
         h2d_race = case_when(s_race == "White" & t_race == "White" ~ 1,
                              s_race == "Black" & t_race == "Black" ~ 0),
         rr1 = 5 - rr1, 
         rr2 = 5 - rr2, 
         rr4 = 5 - rr4, 
         racial = nalevs(rr1 + rr2 + rr3 + rr4),
         sex1 = 6 - sex1, 
         sex2 = 6 - sex2, 
         sex3 = 6 - sex3, 
         sex4 = 6 - sex4, 
         sexism = nalevs(sex1 + sex2 + sex3 + sex4), 
         sdo1 = 6 - sdo1, 
         sdo2 = 6 - sdo2, 
         sdo6 = 6 - sdo6, 
         dominance = nalevs(sdo1 + sdo2 + sdo3 + sdo4 + sdo5 + sdo6 + sdo7 + sdo8), 
         sj3 = 6 - sj3, 
         sj7 = 6 - sj7, 
         justification = nalevs(sj1 + sj2 + sj3 + sj4 + sj5 + sj6 + sj7 + sj8), 
         white = nalevs(white1 + white2 + white3 + white4 + white5), 
         gid = nalevs(gid1 + gid2 + gid3), 
         psid1 = 6 - psid1, 
         psid2 = 6 - psid2, 
         psir1 = 6 - psir1, 
         psir2 = 6 - psir2,
         psid = psid1 + psid2 + psid3 + psid4,
         psir = psir1 + psir2 + psir3 + psir4, 
         psi = case_when(psid == 4 | psir ==  4 ~ 4, 
                         psid == 5 | psir ==  5 ~ 5, 
                         psid == 6 | psir ==  6 ~ 6, 
                         psid == 7 | psir ==  7 ~ 7, 
                         psid == 8 | psir ==  8 ~ 8, 
                         psid == 9 | psir ==  9 ~ 9, 
                         psid == 10 | psir == 10  ~ 10, 
                         psid == 11 | psir == 11  ~ 11, 
                         psid == 12 | psir == 12  ~ 12, 
                         psid == 13 | psir == 13  ~ 13, 
                         psid == 14 | psir == 14  ~ 14, 
                         psid == 15 | psir == 15  ~ 15, 
                         psid == 16 | psir == 16  ~ 16, 
                         psid == 17 | psir == 17  ~ 17, 
                         psid == 18 | psir == 18  ~ 18, 
                         psid == 19 | psir == 19  ~ 19, 
                         psid == 20 | psir == 20  ~ 20
                         ) %>% nalevs()
         ) 

#write_rds(civility_cleaned, "Gubitz cleaned.rds")
