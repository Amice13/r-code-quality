# A global panel dataset of dyadic dual citizenship acceptance
# Maarten Vink, Luuk van der Baaren, David Reichel
# Replication script for data preparation
# This script produces two datasets: 1 with monadic country-year data ('dc_data_monadic_') and 1 with dyad-year data ('dc_data_dyadic_')

rm(list=ls(all=TRUE))
library(tidyverse)
library(readxl)

# import GLOBALCIT v2 data and select country + dc variables
# download GLOBALCIT Citizenship Law v2.zip from https://cadmus.eui.eu/handle/1814/73190
# use file "data_v2.0_country-year.csv"
data <- read_csv("data_v2.0_country-year.csv") |>
  select(iso3, iso2, country, region, year, starts_with(c("A06b", "L01", "L05"))) 

# Monadic data preparation

#recode into new variable dual citizenship acceptance at acquisition in country (adc_bin)
#this variable will be used for destination country citizenship policies
data$adc_bin <- ifelse(data$A06b_cat == 0, 1, #No (generalised) requirement
                       ifelse(data$A06b_cat == 1 | data$A06b_cat == 2 | data$A06b_cat == 3 | data$A06b_cat == 9, 0, #Renunciation requirement or no residence-based naturalisation
                              NA))

#recode adc2_bin based on effective renunciation requirement
data$adc2_bin <- ifelse(data$A06b_cat == 0 | data$A06b_cat == 1, 1, #No (generalised) (effective) requirement
                        ifelse(data$A06b_cat == 2 | data$A06b_cat == 3 | data$A06b_cat == 9,  0, #Renunciation requirement or no residence-based naturalisation
                               NA))

#recode into new variable dual citizenship acceptance at acquisition abroad (ldc_bin)
#this variable will be used for origin country citizenship policies

#create ldc_cat 
data$ldc_cat <- ifelse(data$L05_cat == 5 | data$L05_cat == 2 | data$L05_cat == 4 | data$L05_cat == 1, 0, # Loss due to vol acquisition
                       ifelse((data$L05_cat == 6 | data$L05_cat == 3) & data$L01_bin == 1, 1, #Loss depends on mode cit acq + renunciation possible
                              ifelse((data$L05_cat == 6 | data$L05_cat == 3) & data$L01_bin == 0, 2, #Loss depends on mode cit acq + renunciation not possible
                                     ifelse(data$L05_cat == 0 & data$L01_bin == 1, 3, # No loss but renunciation possible
                                            ifelse(data$L05_cat == 0 & data$L01_bin == 0, 4, #No loss and renunciation not possible
                                                   NA))))) 

#recode into _bin
data$ldc_bin <- ifelse(data$ldc_cat != 0 , 1, #No loss of citizenship due to vol acq
                       ifelse(data$ldc_cat == 0 , 0, #Loss of citizenship due to vol acq
                              NA))

#alternative coding - only automatic loss
data$ldc2_cat <- ifelse(data$L05_cat == 2 | data$L05_cat == 1, 0, # Automatic loss due to vol acq
                        ifelse(data$L05_cat == 3 & data$L01_bin == 1, 1, #Automatic Loss depends on mode cit acq and renunciation possible
                               ifelse(data$L05_cat == 3 & data$L01_bin == 0, 2, #Automatic Loss depends on mode cit acq and renunciation not possible
                                      ifelse((data$L05_cat == 0 | data$L05_cat == 6 | data$L05_cat == 5 | data$L05_cat == 4) & data$L01_bin == 1, 3, # No automatic loss but renunciation possible
                                             ifelse((data$L05_cat == 0 | data$L05_cat == 6 | data$L05_cat == 5 | data$L05_cat == 4) & data$L01_bin == 0, 4, #No loss and renunciation not possible
                                                    NA)))))

#recode ldc_bin2 (only if automatic loss)
data$ldc2_bin <- ifelse(data$ldc2_cat == 0, 0, # Automatic loss due to vol acq
                        ifelse(data$ldc2_cat != 0, 1, # No automatic loss of citizenship due to vol acq
                               NA))

#recode overall dual citizenship variable - strictest operationalisation
data$aldc_cat <- ifelse(data$adc_bin == 0 & data$ldc_bin == 0, 0, #No dual citizenship acceptance
                        ifelse(data$adc_bin == 0 & data$ldc_bin == 1, 1, #Only for emigrants
                               ifelse(data$adc_bin == 1 & data$ldc_bin == 0, 2, #Only for immigrants
                                      ifelse(data$adc_bin == 1 & data$ldc_bin == 1, 3, #Full dual citizenship acceptance 
                                             NA)))) 

#binary version
data$aldc_bin <- ifelse(data$aldc_cat != 3, 0, #No or only partial dual citizenship acceptance
                        ifelse(data$aldc_cat == 3, 1, # Full dual citizenship acceptance
                               NA))

#recode overall dual citizenship variable - intermediate operationalisation (effective renunciation requirement)
#same as dualcit_combi in GLOBALCIT Citizenship Law Dataset
data$aldc1_cat <- ifelse(data$adc2_bin == 0 & data$ldc_bin == 0, 0, #No dual citizenship acceptance
                         ifelse(data$adc2_bin == 0 & data$ldc_bin == 1, 1, #Only for emigrants
                                ifelse(data$adc2_bin == 1 & data$ldc_bin == 0, 2, #Only for immigrants
                                       ifelse(data$adc2_bin == 1 & data$ldc_bin == 1, 3, #Full dual citizenship acceptance 
                                              NA)))) 

#binary version
data$aldc1_bin <- ifelse(data$aldc1_cat != 3, 0, #No or only partial dual citizenship acceptance
                         ifelse(data$aldc1_cat == 3 , 1, # Full dual citizenship acceptance
                                NA))

#recode overall dual citizenship variable, based on effective renunciation req and automatic loss only (aldc2)
data$aldc2_cat <- ifelse(data$adc2_bin == 0 & data$ldc2_bin == 0, 0, #No dual citizenship acceptance
                         ifelse(data$adc2_bin == 0 & data$ldc2_bin == 1, 1, #Only for emigrants
                                ifelse(data$adc2_bin == 1 & data$ldc2_bin == 0, 2, #Only for immigrants
                                       ifelse(data$adc2_bin == 1 & data$ldc2_bin == 1, 3, #Full dual citizenship acceptance 
                                              NA)))) 

#binary version
data$aldc2_bin <- ifelse(data$aldc2_cat != 3, 0, #No or only partial dual citizenship acceptance
                         ifelse(data$aldc2_cat == 3 , 1, # Full dual citizenship acceptance
                                NA))

#order data by country name and by year
data <- data[order(data$country,data$year),]
# View(data)

# Export recoded dual cit dataset
write.csv(data, file = paste0("dc_data_monadic_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(data, file = paste0("GcDDCAD_v1.0_monadic.csv"), row.names = FALSE)

# Dyadic data preparation

#make origin country citizenship policy variables: loss provisions
dc_o <- data %>%
  select(iso3_o = iso3, year, starts_with(c("L01", "L05", "ldc"))) 

#make destination country citizenship policy variables: reunciation requirement (acquisition)
dc_d <- data %>%
  select(iso3_d = iso3, year, starts_with(c("A06", "adc")))

#make dyadic data
d_sym <- merge(dc_o, dc_d, by = "year", allow.cartesian = TRUE)%>%
  mutate(dyad = paste(iso3_o,"_",iso3_d)) %>% 
  filter(iso3_o != iso3_d) # delete "equal" dyads (e.g. US-US) -> results in symmetric dyadic data set

d <- d_sym  %>% # remove rows with missing information on key dualcit variables 
  filter(!is.na(adc_bin))%>% 
  filter(!is.na(adc2_bin))%>% 
  filter(!is.na(ldc_bin))%>% 
  filter(!is.na(ldc2_bin))

#confirm no rows with missing information origin loss or destination ren req
d_check1 <- d %>%
  subset(is.na(L05_cat) | is.na(L01_cat) | is.na(A06b_cat))
nrow(d_check1) # 8565 missing in at least one

#add dyadic dual cit variable (binary)
d <- d %>%
  mutate(dc_dy_bin = ifelse(ldc_bin == 1 & adc_bin ==1, 1, 
                            ifelse(ldc_bin == 0 | adc_bin ==0, 0, NA)))

#add dyadic dual cit variable (binary1) based on more inclusive operationalization of dual citizenship acceptance (only effective renunciation req)
d <- d %>%
  mutate(dc1_dy_bin = ifelse(ldc_bin == 1 & adc2_bin ==1, 1,
                             ifelse(ldc_bin == 0 | adc2_bin ==0, 0, NA)))

#add dyadic dual cit variable (binary) based on more inclusive operationalization of dual citizenship acceptance
d <- d %>%
  mutate(dc2_dy_bin = ifelse(ldc2_bin == 1 & adc2_bin ==1, 1,
                             ifelse(ldc2_bin == 0 | adc2_bin ==0, 0, NA)))

#add dyadic dual cit variable (categorical)
d <- d %>%
  mutate(dc_dy_cat = ifelse(ldc_bin == 1 & adc_bin ==1, 3, #full dyadic acceptance
                                     ifelse(ldc_bin == 1 & adc_bin ==0, 2, #only origin country acceptance
                                            ifelse(ldc_bin == 0 & adc_bin ==1, 1, #only destination country acceptance
                                                   ifelse(ldc_bin == 0 & adc_bin ==0, 0, #no acceptance
                                                          NA)))))

#with more inclusive operationalisation (effective restrictions: ren req)
d <- d %>%
  mutate(dc1_dy_cat = ifelse(ldc_bin == 1 & adc2_bin ==1, 3, #full dyadic acceptance
                            ifelse(ldc_bin == 1 & adc2_bin ==0, 2, #only origin country acceptance
                                   ifelse(ldc_bin == 0 & adc2_bin ==1, 1, #only destination country acceptance
                                          ifelse(ldc_bin == 0 & adc2_bin ==0, 0, #no acceptance
                                                 NA)))))

#with more inclusive operationalisation (effective restrictions)
d <- d %>%
  mutate(dc2_dy_cat = ifelse(ldc2_bin == 1 & adc2_bin ==1, 3, #full dyadic acceptance
                                     ifelse(ldc2_bin == 1 & adc2_bin ==0, 2, #only origin country acceptance
                                            ifelse(ldc2_bin == 0 & adc2_bin ==1, 1, #only destination country acceptabce
                                                   ifelse(ldc2_bin == 0 & adc2_bin ==0, 0, #no acceptance
                                                          NA)))))

#Dyadic corrections

#import dyadic correction dataset
dcd <- read_xlsx("dc_dy_corr.xlsx", sheet = 'data') |>
  mutate(ldc_cat_c = as.numeric(ldc_cat_c)) |>
  mutate(ldc2_cat_c = as.numeric(ldc2_cat_c)) |>
  mutate(adc_bin_c = as.numeric(adc_bin_c)) |>
  mutate(adc2_bin_c = as.numeric(adc2_bin_c)) 
dim(dcd) #8 variables of 12898 obs
#check duplicates
dcd %>%
  distinct() %>% nrow() #12898 -> 0 duplicates
###identify duplicates
dcd[dcd %>% select(year, iso3_d, iso3_o) %>% duplicated(), ] #empty: no duplicaties


##############

d <- d %>%
  left_join(dcd, by = c("year", "iso3_o", "iso3_d"))

### check duplicates
d %>% select(year, iso3_d, iso3_o) %>% distinct() %>% nrow() #1804563 distinct rows
d[d %>% select(year, iso3_d, iso3_o) %>% duplicated(), ] #0 duplicated rows

### now apply the dyadic corrections to the dyadic dc policy measures
d <- d %>%
  mutate(ldc_cat_c = ifelse(ldc_cat_c == 0, 0,
                            ifelse(ldc_cat_c == 1 & L01_bin == 1, 1,
                                   ifelse(ldc_cat_c == 1 & L01_bin == 0, 2,#recode category loss depends on mode cit acq and no renunciation possible
                                          ifelse(ldc_cat_c == 2, 3,
                                                 ifelse(ldc_cat_c == 3, 4, NA))))))%>%
  mutate(ldc2_cat_c = ifelse(ldc2_cat_c == 0, 0,
                             ifelse(ldc2_cat_c == 1 & L01_bin == 1, 1,
                                    ifelse(ldc2_cat_c == 1 & L01_bin == 0, 2,#recode category loss depends on mode cit acq and no renunciation possible
                                           ifelse(ldc2_cat_c == 2, 3,
                                                  ifelse(ldc2_cat_c == 3, 4, NA))))))%>%
  mutate(dy_c = ifelse(is.na(treaty), 0, 1), #new variable to indicate relevance of unilateral dyadic specific rule (treaty==0) or bilateral/multilateral treaty-based rule (treaty==1)
         ldc_cat_c = ifelse(is.na(ldc_cat_c), ldc_cat, ldc_cat_c),
         ldc2_cat_c = ifelse(is.na(ldc2_cat_c), ldc2_cat, ldc2_cat_c),
         adc_bin_c = ifelse(is.na(adc_bin_c), adc_bin, adc_bin_c),
         adc2_bin_c = ifelse(is.na(adc2_bin_c), adc2_bin, adc2_bin_c))

# add ldc_bin_c based on ldc_cat_c  
d <- d %>%
  mutate(ldc_bin_c = ifelse(ldc_cat_c == 0, 0, 1))

# add ldc2_bin_c based on ldc2_cat_c  
d <- d %>%
  mutate(ldc2_bin_c = ifelse(ldc2_cat_c == 0, 0, 1))

#add corrected dyadic dual cit variable (binary)
d <- d %>%
  mutate(dc_dy_bin_c = ifelse(ldc_bin_c == 1 & adc_bin_c ==1, 1, 0))

#add corrected dyadic dual cit variable (binary) based on more inclusive operationalization of dual citizenship acceptance (effective ren req)
d <- d %>%
  mutate(dc1_dy_bin_c = ifelse(ldc_bin_c == 1 & adc2_bin_c ==1, 1,
                               ifelse(ldc_bin_c == 0 | adc2_bin_c ==0, 0, 
                                      NA)))

#add corrected dyadic dual cit variable (binary) based on more inclusive operationalization of dual citizenship acceptance
d <- d %>%
  mutate(dc2_dy_bin_c = ifelse(ldc2_bin_c == 1 & adc2_bin_c ==1, 1,
                               ifelse(ldc2_bin_c == 0 | adc2_bin_c ==0, 0, 
                               NA)))

#add corrected dyadic dual cit variable (categorical)
d <- d %>%
  mutate(dc_dy_cat_c = ifelse(ldc_bin_c == 1 & adc_bin_c ==1, 3, #full dyadic acceptance
                                       ifelse(ldc_bin_c == 1 & adc_bin_c ==0, 2, #only origin country acceptance
                                              ifelse(ldc_bin_c == 0 & adc_bin_c ==1, 1, #only destination country acceptance
                                                     ifelse(ldc_bin_c == 0 & adc_bin_c ==0, 0, #no acceptance
                                                            NA)))))

#add corrected dyadic dual cit variable (categorical) with only effective ren req
d <- d %>%
  mutate(dc1_dy_cat_c = ifelse(ldc_bin_c == 1 & adc2_bin_c ==1, 3, #full dyadic acceptance
                              ifelse(ldc_bin_c == 1 & adc2_bin_c ==0, 2, #only origin country acceptance
                                     ifelse(ldc_bin_c == 0 & adc2_bin_c ==1, 1, #only destination country acceptance
                                            ifelse(ldc_bin_c == 0 & adc2_bin_c ==0, 0, #no acceptance
                                                   NA)))))

#add corrected dyadic dual cit2 variable (categorical)
d <- d %>%
  mutate(dc2_dy_cat_c = ifelse(ldc2_bin_c == 1 & adc2_bin_c ==1, 3, #full dyadic acceptance
                                        ifelse(ldc2_bin_c == 1 & adc2_bin_c ==0, 2, #only origin country acceptance
                                               ifelse(ldc2_bin_c == 0 & adc2_bin_c ==1, 1, #only destination country acceptabce
                                                      ifelse(ldc2_bin_c == 0 & adc2_bin_c ==0, 0, #no acceptance
                                                             NA)))))

col_order <- c("year", "iso3_o",
               "L01_cat", "L01_bin", 
               "L05_cat", "L05_bin", 
               "ldc_cat", "ldc_bin", "ldc2_cat", "ldc2_bin", 
               "ldc_cat_c", "ldc2_cat_c", "ldc_bin_c", "ldc2_bin_c", 
               "iso3_d",
               "A06b_cat", "A06b_bin", 
               "adc_bin", "adc2_bin", "adc_bin_c", "adc2_bin_c",
               "dyad",
               "dc_dy_bin", "dc1_dy_bin", "dc2_dy_bin", "dc_dy_cat","dc1_dy_cat", "dc2_dy_cat",
               "treaty", "dy_c",  
               "dc_dy_bin_c","dc1_dy_bin_c",  "dc2_dy_bin_c", "dc_dy_cat_c","dc1_dy_cat_c", "dc2_dy_cat_c")
d <- d[, col_order]

d <- d[order(d$year,d$iso3_o),]

#save final dyadic dataset
write.csv(d, file = paste0("GcDDCAD_v1.0_dyadic.csv"), row.names = FALSE)

