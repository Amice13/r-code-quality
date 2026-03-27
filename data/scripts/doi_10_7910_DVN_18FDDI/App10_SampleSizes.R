################################################################################
#### Title: Progressive Ideology and Support for Punitive Crime Policy
#### Author: Isabel Laterzo
#### Year: 2023
#### Journal: Comparative Political Studies
#### Portion of Analysis: Appendix 10 - Sample Size and Marginal Distributions
################################################################################

################################ SET UP #######################################

rm(list=ls()) #clear global environ


# uncomment below and set working directory using setwd() to directory which contains
# all data files
#setwd()


#packages
library(tidyverse)


#read in data and filter
data <- readRDS("laterzo_cps_2023c.RDS")

#fix levels label
levels(data$crime)[levels(data$crime)=="Tough on Crime"] <- "Punitive"

############################### Create Variables ################################

#safety variable
#4 insecure, 1 secure --> 1 = unsafe, 0 = safe
data$safety_di <- factor(ifelse(data$safety_neighb == 4, "1", 
                                ifelse(data$safety_neighb == 3, "1", 
                                       ifelse(data$safety_neighb == 2, "0", 
                                              ifelse(data$safety_neighb == 1, "0", NA)))))

##create data frames for prog vs. conserv on social and econ issues based on
## median value
prog_ideo <- data %>% filter(ideo >= 0) #same as not including zero
cons_ideo <- data %>% filter(ideo < 0) #same as not including zero

#sample size for each

# FIRST: Calculate probability distributions for each of the pre-treatment
# covariates

safety <- data %>%
  drop_na(safety_di, crime) %>% 
  select(id, safety_di) %>%
  unique()
safety$safety_di <- as.numeric(as.character(safety$safety_di))
mean(safety$safety_di) #probability that safety_di == 1 is 0.44

vic <- data %>%
  drop_na(vic, crime) %>% 
  select(id, vic) %>%
  unique()
vic$vic <- as.numeric(as.character(vic$vic))
mean(vic$vic) #probability that vic == 1 is 0.17

assist <- data %>%
  drop_na(assist_effect, crime) %>% 
  select(id, assist_effect) %>%
  unique()
assist$assist_effect <- as.numeric(as.character(assist$assist_effect))
mean(assist$assist_effect) #probability that sassist_effect == 1 is 0.34

gang <- data %>%
  drop_na(crime_gang, crime) %>% 
  select(id, crime_gang) %>%
  unique()
gang$crime_gang <- as.numeric(as.character(gang$crime_gang))
mean(gang$crime_gang) #probability that crime_gang == 1 is 0.82


# SECOND Calculate the sample sizes

## POOLED

cons <- cons_ideo %>%
  drop_na(crime) %>% 
  select(id) %>%
  unique()
nrow(cons) #1131


prog <- prog_ideo %>%
  drop_na(crime) %>% 
  select(id) %>%
  unique()
nrow(prog) #1204

## 1) SAFETY

#pooled progressives
safety_prog <- prog_ideo %>%
  drop_na(safety_di, crime) %>% 
  select(id) %>%
  unique()
nrow(safety_prog) #1218

#pooled conservatives
safety_cons <- cons_ideo %>%
  drop_na(safety_di, crime) %>% 
  select(id) %>%
  unique()
nrow(safety_cons) #1093

#progressive brazilians
safety_prog_brazil <- prog_ideo %>%
  filter(country == "brazil") %>%
  drop_na(safety_di, crime) %>% 
  select(id) %>%
  unique()
nrow(safety_prog_brazil) #447

#progressive argentinians
safety_prog_arg <- prog_ideo %>%
  filter(country == "argentina") %>%
  drop_na(safety_di, crime) %>% 
  select(id) %>%
  unique()
nrow(safety_prog_arg) #771


#conservative brazilians
safety_cons_brazil <- cons_ideo %>%
  filter(country == "brazil") %>%
  drop_na(safety_di, crime) %>% 
  select(id) %>%
  unique()
nrow(safety_cons_brazil) #731

#conservative argentinians
safety_cons_arg <- cons_ideo %>%
  filter(country == "argentina") %>%
  drop_na(safety_di, crime) %>% 
  select(id) %>%
  unique()
nrow(safety_cons_arg) #362


## 2) VICTIMIZATION

#pooled progressives
vic_prog <- prog_ideo %>%
  drop_na(vic, crime) %>% 
  select(id) %>%
  unique()
nrow(vic_prog) #1253

#pooled conservatives
vic_cons <- cons_ideo %>%
  drop_na(vic, crime) %>% 
  select(id) %>%
  unique()
nrow(vic_cons) #1113


#progressive brazilians
vic_prog_brazil <- prog_ideo %>%
  filter(country == "brazil") %>%
  drop_na(vic, crime) %>% 
  select(id) %>%
  unique()
nrow(vic_prog_brazil) #464


#progressive argentinians
vic_prog_arg <- prog_ideo %>%
  filter(country == "argentina") %>%
  drop_na(vic, crime) %>% 
  select(id) %>%
  unique()
nrow(vic_prog_arg) #789


#conservative brazilians
vic_cons_brazil <- cons_ideo %>%
  filter(country == "brazil") %>%
  drop_na(vic, crime) %>% 
  select(id) %>%
  unique()
nrow(vic_cons_brazil) #749

#conservative argentinians
vic_cons_arg <- cons_ideo %>%
  filter(country == "argentina") %>%
  drop_na(vic, crime) %>% 
  select(id) %>%
  unique()
nrow(vic_cons_arg) #364


## 3) SOCIAL ASSISTANCE

#pooled progressives
assist_prog <- prog_ideo %>%
  drop_na(assist_effect, crime) %>% 
  select(id) %>%
  unique()
nrow(assist_prog) #1141

#pooled conservatives
assist_cons <- cons_ideo %>%
  drop_na(assist_effect, crime) %>% 
  select(id) %>%
  unique()
nrow(assist_cons) #1014


#progressive brazilians
assist_prog_brazil <- prog_ideo %>%
  filter(country == "brazil") %>%
  drop_na(assist_effect, crime) %>% 
  select(id) %>%
  unique()
nrow(assist_prog_brazil) #411

#progressive argentinians
assist_prog_arg <- prog_ideo %>%
  filter(country == "argentina") %>%
  drop_na(assist_effect, crime) %>% 
  select(id) %>%
  unique()
nrow(assist_prog_arg) #730

#conservative brazilians
assist_cons_brazil <- cons_ideo %>%
  filter(country == "brazil") %>%
  drop_na(assist_effect, crime) %>% 
  select(id) %>%
  unique()
nrow(assist_cons_brazil) #681

#conservative argentinians
assist_cons_arg <- cons_ideo %>%
  filter(country == "argentina") %>%
  drop_na(assist_effect, crime) %>% 
  select(id) %>%
  unique()
nrow(assist_cons_arg) #333



## 4) CRIME: GANGS

#pooled progressives
gang_prog <- prog_ideo %>%
  drop_na(crime_gang, crime) %>% 
  select(id) %>%
  unique()
nrow(gang_prog) #1193

#pooled conservatives
gang_cons <- cons_ideo %>%
  drop_na(crime_gang, crime) %>% 
  select(id) %>%
  unique()
nrow(gang_cons) #1049

#progressive brazilians
gang_prog_brazil <- prog_ideo %>%
  filter(country == "brazil") %>%
  drop_na(crime_gang, crime) %>% 
  select(id) %>%
  unique()
nrow(gang_prog_brazil) #427

#progressive argentinians
gang_prog_arg <- prog_ideo %>%
  filter(country == "argentina") %>%
  drop_na(crime_gang, crime) %>% 
  select(id) %>%
  unique()
nrow(gang_prog_arg) #766

#conservative brazilians
gang_cons_brazil <- cons_ideo %>%
  filter(country == "brazil") %>%
  drop_na(crime_gang, crime) %>% 
  select(id) %>%
  unique()
nrow(gang_cons_brazil) #697

#conservative argentinians
gang_cons_arg <- cons_ideo %>%
  filter(country == "argentina") %>%
  drop_na(crime_gang, crime) %>% 
  select(id) %>%
  unique()
nrow(gang_cons_arg) #352





