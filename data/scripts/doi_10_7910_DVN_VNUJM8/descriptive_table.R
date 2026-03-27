#load packages
library(tidyverse)
library(janitor)

here::here()

#load data
df <- read_rds("data/clean_survey_values.rds")%>%
    mutate(experience=as.numeric(Experience)) %>%
#Create categorical variable for experience level
    mutate(exp_cat=ifelse(experience>=6, "Worked 5+ election cycles",
                   ifelse(experience>=3, "Worked 3-5 election cycles",
                   ifelse(experience<3, "Worked 1-2 election cycles", NA))))%>%
#Manually double check if more respondents are added 
   mutate(exclude=ifelse(is.na(Respondent)&is.na(Gender)&is.na(PersonalIdeology)&is.na(CandidateIdeology)&is.na(More_1), 1, 0))%>%
  mutate(took_conjoint = ifelse(!is.na(Conjoint1) | !is.na(Conjoint2) | !is.na(Conjoint3), 0, 1))%>%
    glimpse()


df_ex <- df %>%
  filter(exclude==1)%>%
  select(RecipientEmail)%>%
  glimpse()

#$ RecipientEmail <chr> "f2809f2f", "cb91135f", "11ba01e2", "f9173309", "0ae8cd72"

df_conjoint <- read_rds("data/clean_conjoint_values.rds")%>%
filter(RecipientEmail=="f2809f2f" | RecipientEmail== "cb91135f" | RecipientEmail== "11ba01e2" | RecipientEmail== "f9173309" | RecipientEmail== "0ae8cd72")%>%
  glimpse()

table(df_conjoint$RecipientEmail, df_conjoint$choice, useNA = "always")
#RecipientEmail=="f2809f2f" participated in conjoint but not demographic questions

df <- df %>%
  mutate(exclude=ifelse(RecipientEmail=="f2809f2f", 0, exclude))%>%
  glimpse()

table(df$exclude)

df <- df %>%
#filter out respondents who clicked into the survey but did not answer any questions  
  filter(exclude==0)%>%
glimpse()

#check code
table(df$experience, df$exp_cat)
#All who indicated "0" campaigns are candidates.
table(df$experience, df$Respondent)

#Correlation between respondent ideology and candidate ideology 
cor(as.numeric(df$CandidateIdeology), as.numeric(df$PersonalIdeology), method="pearson", use = "complete.obs")

#Correlation between scaled respondent ideology and candidate ideology 
cor(as.numeric(df$CandidateIdeology_scaled), as.numeric(df$PersonalIdeology_scaled), method="pearson", use = "complete.obs")


#Descriptive Table
party <- tabyl(df$republican_candidate)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
party

incum <- tabyl(df$Incumbent)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
incum

staff <- tabyl(df$Respondent)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
staff

office <- tabyl(df$Office)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
office

gender <- tabyl(df$Gender)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
gender

race <- tabyl(df$RespondentRace)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
race

experience <- tabyl(df$exp_cat)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
experience


volunteer <- tabyl(df$VolNumbers)%>%
  adorn_totals("row") %>%
  adorn_pct_formatting()
volunteer


#See Excel sheet for table generation