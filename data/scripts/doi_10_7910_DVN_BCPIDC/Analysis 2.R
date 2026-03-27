rm(list = ls())

################################################################################

# Packages

library('tidyverse')
library('stargazer')
library('reshape2')
library('zoo')
library('stringi')
library('peRspective')
library('orcutt')
library('sandwich')
library('scales')

################################################################################

# Load data frames

load('Data\df_French.Rdata')

load('Data\prsp_French.Rdata')

load('Data\df_Date.Rdata')

################################################################################

# We add dates to Transcripts data frame

df_French <- merge(df_French, df_Date)

# We infer the week, defined as the previous Sunday

df_French$Week <- NA

for (i in 1: dim(df_French)[1]){
  
  d <- df_French$Date[i]
  prev.days <- seq(d - 6, d, by = 'day')
  df_French$Week[i] <- as.Date(prev.days[weekdays(prev.days) == 'Sunday'])
  
}

# We format relevant variables (e.g., Party, Speaker)

df_French <- df_French %>%
  mutate(Week = as.Date(Week),
         Year = as.factor(as.numeric(format(Date, '%Y'))),
         Legislature = substr(Session, 1, 2),
         Party = stri_extract_last_words(Speaker), 
         Party = case_when(Party == 'Lib' ~ 'LPC',
                           Party %in% c('CPC', 'PCC') ~ 'CPC',
                           Party %in% c('NPD', 'NDP') ~ 'NDP',
                           Party == 'BQ' ~ 'BQ',
                           .default = NA),
         Speaker_clean = gsub('\\(.*', '', Speaker),
         Speaker_clean = gsub(paste(c('Right Hon. ', 
                                      'Hon. ', 
                                      'Mr. ', 
                                      'Ms. ', 
                                      'Mrs. ', 
                                      'M. ', 
                                      'L\'hon. ', 
                                      'Le très hon. ', 
                                      'L’hon. ', 
                                      'Mme '), 
                                    collapse = '|'), '', Speaker_clean),
         Speaker_clean = str_squish(Speaker_clean),
         Speaker_clean = gsub('’', '\'', Speaker_clean))

df_Speaker_Party <- df_French %>%
  select(Session, Meeting, Speaker_clean, Party) %>% 
  unique() %>% 
  na.omit()

df_French <- df_French %>%
  merge(df_Speaker_Party, 
        by = c('Session', 'Meeting', 'Speaker_clean'),
        all.x = TRUE) %>%
  select(-Party.x) %>%
  rename(Party = Party.y) %>%
  na.omit() %>%
  merge(prsp_French, by.x = 'ID', by.y = 'text_id')

################################################################################

# We compute summary statistics

# This vector contains the 'tags' of the five emotional attributes we study

vec_models <- c('IDENTITY_ATTACK', 
                'INSULT', 
                'PROFANITY', 
                'THREAT',
                'TOXICITY')

# Table B1

stargazer(df_French %>% filter(Party != 'BQ') %>% select(vec_models), 
          flip = TRUE,
          summary.stat = c('n', 'mean', 'sd', 'min', 'p25', 'median', 'p75', 'max'))

# Table G2

table(df_French %>% filter(Party != 'BQ') %>% select(c('Party', 'Language')))

# Table G3

stargazer(cor(df_French %>% filter(Party != 'BQ') %>% select(vec_models)))

temp <- melt(df_French %>% filter(Party != 'BQ') %>% select(vec_models)) %>%
  mutate(variable = case_when(variable == 'IDENTITY_ATTACK' ~ 'IDENTITY ATTACK',
                              .default = variable))

log10_minor_break = function (...){
  function(x) {
    minx         = floor(min(log10(x), na.rm=T))-1;
    maxx         = ceiling(max(log10(x), na.rm=T))+1;
    n_major      = maxx-minx+1;
    major_breaks = seq(minx, maxx, by=1)
    minor_breaks = 
      rep(log10(seq(1, 9, by=1)), times = n_major)+
      rep(major_breaks, each = 9)
    return(10^(minor_breaks))
  }
}

# Figure G1

melt(df_French %>% filter(Party != 'BQ') %>% select(vec_models)) %>%
  mutate(variable = case_when(variable == 'IDENTITY_ATTACK' ~ 'IDENTITY ATTACK',
                              .default = variable)) %>%
  ggplot(mapping = aes(x = value)) +
  facet_wrap(vars(variable), nrow = 3) +
  geom_histogram(binwidth = 0.01) +
  scale_y_log10(breaks = trans_breaks('log10', function(x) 10^x),
                minor_breaks = log10_minor_break()) +
  scale_x_continuous(breaks = seq(from = 0, to = 0.8, by = 0.1)) +
  theme_bw() +
  labs(y = '', x = '')

ggsave('Distribution Estimated Probability French.pdf', width = 6.5, height = 8.5)

################################################################################

# We generate series capturing the average incidence of emotional attributes in
# all speeches from a given party pronounced during a given week

df_Weekly <- df_French %>%
  group_by(Party, Week) %>%
  summarize_at(vars(all_of(vec_models)), mean) %>%
  ungroup() %>%
  arrange(Party, Week)

for (party in unique(df_Weekly$Party)) {
  
  temp <- setdiff(seq(min(df_Weekly$Week), 
                      max(df_Weekly$Week), 
                      by = 'week'),
                  df_Weekly[df_Weekly$Party == party,]$Week)
  
  df_Weekly <- rbind(df_Weekly,
                        data.frame(Party = rep(party, times = length(temp)),
                                   Week = as.Date(temp), 
                                   matrix(NA, 
                                          nrow = length(temp), 
                                          ncol = length(vec_models),
                                          dimnames = list(NULL, vec_models))))
  
}

df_Weekly <- df_Weekly %>%
  arrange(Party, Week) 

################################################################################

## Regressions

# These functions format certain variables for regression models

# Number of weeks left until the next election

Weeks_to_Election <- function(Date) {
  
  prev.days <- seq(Date - 6, Date, by='day')
  
  Week <- as.Date(prev.days[weekdays(prev.days)=='Sunday'])
  
  if (Week <= as.Date("2008-10-14")) {
    Time <- as.Date("2008-10-14") - Week
  } else if (Week <= as.Date("2011-05-02")) {
    Time <- as.Date("2011-05-02") - Week
  } else if (Week <= as.Date("2015-10-19")) {
    Time <- as.Date("2015-10-19") - Week
  } else if (Week <= as.Date("2019-10-21")) {
    Time <- as.Date("2019-10-21") - Week
  } else if (Week <= as.Date("2021-09-20")) {
    Time <- as.Date("2021-09-20") - Week
  }
  
  return(floor(as.numeric(Time)/7))
  
}

# Whether the party was in government during that week

Dummy_Gouv_Weekly <- function(x) {
  
  Week <- x[1]
  
  Party <- x[2]
  
  if (Week <= as.Date("2015-10-19")) {
    if (Party == "CPC") {
      Gouv <- 1
    } else {
      Gouv <- 0
    }
  } else {
    if (Party == "LPC") {
      Gouv <- 1
    } else {
      Gouv <- 0
    }  
  } 
  
  return(Gouv)
  
}

# When the next election will occur

Next_Election <- function(Date) {
  
  if (Date <= as.Date("2008-10-14")) {
    Next <- as.Date("2008-10-14")
  } else if (Date <= as.Date("2011-05-02")) {
    Next <- as.Date("2011-05-02")
  } else if (Date <= as.Date("2015-10-19")) {
    Next <- as.Date("2015-10-19")
  } else if (Date <= as.Date("2019-10-21")) {
    Next <- as.Date("2019-10-21")
  } else if (Date <= as.Date("2021-09-20")) {
    Next <- as.Date("2021-09-20")
  }
  
  return(as.Date(Next))
  
}

################################################################################

# We generate series capturing the average incidence of emotional attributes in
# all speeches from a given party pronounced in a given language during a given week

df_Weekly_Language <- df_French %>%
  group_by(Party, Week, Language) %>%
  summarize_at(vars(all_of(vec_models)), mean) %>%
  ungroup() %>%
  arrange(Party, Week, Language)

df_Weekly_Language <- df_Weekly_Language %>%  
  mutate(To_Next_Election = sapply(Week, Weeks_to_Election),
         Gov = apply(df_Weekly_Language[c("Week", "Party")], MARGIN = 1, FUN = Dummy_Gouv_Weekly),
         Next_Election = sapply(Week, FUN = Next_Election),
         Next_Election = as.Date(Next_Election),
         Next_Election = as.factor(Next_Election),
         Minority = ifelse(Next_Election %in% c("2008-10-14", "2011-05-02", "2021-09-20"), 
                           1, 0)) %>%
  mutate_at(vars(vec_models), function(x){log(x / (1 - x))})
  
################################################################################

Reg_Toxicity <- lm(TOXICITY ~ To_Next_Election * Minority + Gov + Party + Language, 
                   data = df_Weekly_Language %>% filter(Party != 'BQ'))

Reg_Identity <- lm(IDENTITY_ATTACK ~ To_Next_Election * Minority + Gov + Party + Language, 
                   data = df_Weekly_Language %>% filter(Party != 'BQ'))

Reg_Insult <- lm(INSULT ~ To_Next_Election * Minority + Gov + Party + Language, 
                 data = df_Weekly_Language %>% filter(Party != 'BQ'))

Reg_Profanity <- lm(PROFANITY ~ To_Next_Election * Minority + Gov + Party + Language, 
                    data = df_Weekly_Language %>% filter(Party != 'BQ'))

Reg_Threat <- lm(THREAT ~ To_Next_Election * Minority + Gov + Party + Language, 
                 data = df_Weekly_Language %>% filter(Party != 'BQ'))

Reg_Toxicity <- cochrane.orcutt(Reg_Toxicity)

Reg_Identity <- cochrane.orcutt(Reg_Identity)

Reg_Insult <- cochrane.orcutt(Reg_Insult)

Reg_Profanity <- cochrane.orcutt(Reg_Profanity)

Reg_Threat <- cochrane.orcutt(Reg_Threat)

# Table G4

stargazer(Reg_Identity, Reg_Insult, Reg_Profanity, Reg_Threat, Reg_Toxicity)

################################################################################

df_French <- df_French %>%  
  mutate(To_Next_Election = sapply(Week, Weeks_to_Election),
         Gov = apply(df_French[c("Week", "Party")], MARGIN = 1, FUN = Dummy_Gouv_Weekly),
         Next_Election = sapply(Week, FUN = Next_Election),
         Next_Election = as.Date(Next_Election),
         Next_Election = as.factor(Next_Election),
         Minority = ifelse(Next_Election %in% c("2008-10-14", "2011-05-02", "2021-09-20"), 
                           1, 0)) %>%
  mutate_at(vars(vec_models), function(x){log(x / (1 - x))})

################################################################################

Reg_Toxicity_Unit <- lm(TOXICITY ~ To_Next_Election * Minority + Gov + Party + Language, 
                        data = df_French %>% filter(Party != 'BQ'))

Reg_Identity_Unit <- lm(IDENTITY_ATTACK ~ To_Next_Election * Minority + Gov + Party + Language, 
                        data = df_French %>% filter(Party != 'BQ'))

Reg_Insult_Unit <- lm(INSULT ~ To_Next_Election * Minority + Gov + Party + Language, 
                      data = df_French %>% filter(Party != 'BQ'))

Reg_Profanity_Unit <- lm(PROFANITY ~ To_Next_Election * Minority + Gov + Party + Language, 
                         data = df_French %>% filter(Party != 'BQ'))

Reg_Threat_Unit <- lm(THREAT ~ To_Next_Election * Minority + Gov + Party + Language, 
                      data = df_French %>% filter(Party != 'BQ'))

Reg_Toxicity_Unit_se <- sqrt(diag(vcovCL(Reg_Toxicity_Unit, cluster = ~ Speaker + Session * Meeting)))

Reg_Identity_Unit_se <- sqrt(diag(vcovCL(Reg_Identity_Unit, cluster = ~ Speaker + Session * Meeting)))

Reg_Insult_Unit_se <- sqrt(diag(vcovCL(Reg_Insult_Unit, cluster = ~ Speaker + Session * Meeting)))

Reg_Profanity_Unit_se <- sqrt(diag(vcovCL(Reg_Profanity_Unit, cluster = ~ Speaker + Session * Meeting)))

Reg_Threat_Unit_se <- sqrt(diag(vcovCL(Reg_Threat_Unit, cluster = ~ Speaker + Session * Meeting)))

# Table G5

stargazer(Reg_Identity_Unit, Reg_Insult_Unit, Reg_Profanity_Unit, Reg_Threat_Unit, Reg_Toxicity_Unit,
          se = list(Reg_Identity_Unit_se, Reg_Insult_Unit_se, Reg_Profanity_Unit_se, Reg_Threat_Unit_se, Reg_Toxicity_Unit_se))

################################################################################

# Save weekly time series in data frames

df_Weekly_French <- df_Weekly

save(df_Weekly_French, file = 'df_Weekly_French.Rdata')

df_Weekly_Language_French <- df_Weekly_Language

save(df_Weekly_Language_French, file = 'df_Weekly_Language_French.Rdata')
