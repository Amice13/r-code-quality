#############################################################################
## Data creation code for                                                  ##
## Global Economic Integration and Nativist Politics in Emerging Economies ##
## Benjamin Helms                                                          ##
## May 9, 2022                                                             ##
## American Journal of Political Science                                   ##
#############################################################################

## This file documents code to create all analysis datasets used in the 
## main text and Supplemental Information.

## In the Codebook, I document all data sources. 
## Raw data files required to run this script are available upon request.

## Operating system: MacOS Mojave version 10.14.6 (x86_64-apple-darwin17.0 64-bit)
## Statistical software: R version 4.0.5

## Enter the working directory containing raw datasets here:
setwd("")

## The following packages must be installed to run the script:
## install.packages("readstata13") ## Read Stata 13 .dta files (for one dataset)
## install.packages("haven") ## Read Stata .dta files
## install.packages("readxl") ## Read Microsoft Excel files.
## install.packages("fastDummies") ## Efficiently create indicator variables
## install.packages("tidyverse") ## Data manipulation tools

## Load necessary packages.
library(readstata13)
library(haven)
library(readxl)
library(fastDummies)
library(tidyverse)

##################################################################
## Analysis Dataset 1: District panel of rioting and covariates ##
##################################################################

## The following commands create Analysis Dataset 1:
## District panel of rioting and covariates.
## This is the primary analysis dataset.

## While constructing Analysis Dataset 1, I save four other analysis datasets (noted in code):
## Analysis Dataset 2: District panel of total migration
## Analysis Dataset 3: District panel of male migration
## Analysis Dataset 4: District panel of female migration
## Analysis Dataset 5: District panel of textile concentration and investment, including Rajasthan

## Load file for 1999.
crime_99 <- read_csv("crime_1999.csv")

## Create year identifier.
crime_99 <- mutate(crime_99, year=1999)

## Select and rename variables.
crime_99 <- select(crime_99,
                   state_name=`States / UT`,
                   district_name=District,
                   year,
                   riots=Riots,
                   murders=Murder)

## Arrange data by state, district, and year.
crime_99 <- arrange(crime_99,
                    state_name,
                    district_name,
                    year)

## Remove observations of total crimes for states and all India.
crime_99 <- filter(crime_99, ! district_name %in% c("Total",
                                                    "Total (All-India)"))

## Load file for 2000.
crime_00 <- read_csv("crime_2000.csv")

## Create year identifier.
crime_00 <- mutate(crime_00, year=2000)

## Select and rename variables.
crime_00 <- select(crime_00,
                   state_name=States,
                   district_name=District,
                   year,
                   riots=Riots,
                   murders=Murder)

## Arrange data by state, district, and year.
crime_00 <- arrange(crime_00,
                    state_name,
                    district_name,
                    year)

## Remove observations of total crimes for states and all India.
crime_00 <- filter(crime_00, ! district_name %in% c("Total",
                                                    "Total (All- India)"))

## Load file for 2001-2010.
crime_01_10 <- read_csv("crime_2001_2010.csv")

## Select and rename variables.
crime_01_10 <- select(crime_01_10,
                      state_name=`STATE/UT`,
                      district_name=DISTRICT,
                      year=YEAR,
                      riots=RIOTS,
                      murders=MURDER)

## Arrange data by state, district, and year.
crime_01_10 <- arrange(crime_01_10,
                       state_name,
                       district_name,
                       year)

## Remove observations of total crimes for states.
crime_01_10 <- filter(crime_01_10, district_name!="TOTAL")

## Remove observations from 2011 and 2012.
crime_01_10 <- filter(crime_01_10, year<=2010)

## Create single panel dataset.
crime_panel <- rbind(crime_99,
                     crime_00,
                     crime_01_10)

## Remove other data frames.
rm(crime_99,
   crime_00,
   crime_01_10)

## Change format of state and district names to title case.
crime_panel <- mutate(crime_panel,
                      state_name=str_to_title(state_name),
                      district_name=str_to_title(district_name))

## Remove observations from small non-state union territories.
crime_panel <- filter(crime_panel, ! state_name %in% c("A & N Islands",
                                                       "A&N Islands",
                                                       "A & N Island",
                                                       "D & N Haveli",
                                                       "D&N Haveli",
                                                       "Daman & Diu",
                                                       "Lakshadweep",
                                                       "Lakshadeep",
                                                       "Puducherry",
                                                       "Pondicherry"))

## Remove non-district observations.
## These observations include airport police districts,
## terrorism units, railway police, and other special police units.
crime_panel <- filter(crime_panel, ! district_name %in% c("I.g.i Airport",
                                                          "Igi Airport",
                                                          "I.g.i. Airport",
                                                          "I.g.i.airport",
                                                          "Kolkata Airport",
                                                          "Other Units",
                                                          "Delhi Ut Total",
                                                          "Border",
                                                          "Border District",
                                                          "Anti Terrorist Squad",
                                                          "C. I. D. Crime",
                                                          "C.i.d.",
                                                          "C.i.d",
                                                          "C. I. D.",
                                                          "Cbcid",
                                                          "Cid",
                                                          "Cid Crime",
                                                          "Crime Branch",
                                                          "Crime Jammu",
                                                          "Crime Kashmir",
                                                          "Crime Srinagar",
                                                          "Cyber Cell",
                                                          "Dcp Bbsr",
                                                          "Dcp Ctc",
                                                          "Economic Offences Unit",
                                                          "I&P Haryana",
                                                          "Irrigation & Power",
                                                          "K.railways",
                                                          "Metro",
                                                          "Metro Rail",
                                                          "Spl Cell",
                                                          "Spl Narcotic",
                                                          "Spl Traffic",
                                                          "Traffic Ps",
                                                          "Vigilance"))

crime_panel <- filter(crime_panel, !grepl("Railway", district_name))

crime_panel <- filter(crime_panel, !grepl("Rly.", district_name))

crime_panel <- filter(crime_panel, !grepl("G. R. P.", district_name))

crime_panel <- filter(crime_panel, !grepl("G.r.p.", district_name))

crime_panel <- filter(crime_panel, !grepl("Grp", district_name))

crime_panel <- filter(crime_panel, !grepl("G.r.p", district_name))

crime_panel <- filter(crime_panel, !grepl("Srp", district_name))

crime_panel <- filter(crime_panel, !grepl("W.rly", district_name))

crime_panel <- filter(crime_panel, !grepl("Guntakalrly.", district_name))

crime_panel <- filter(crime_panel, !grepl("Bieo", district_name))

crime_panel <- filter(crime_panel, !grepl("Caw", district_name))

crime_panel <- filter(crime_panel, !grepl("Discom", district_name))

crime_panel <- filter(crime_panel, !grepl("Eow", district_name))

crime_panel <- filter(crime_panel, !grepl("R.p.o.", district_name))

crime_panel <- filter(crime_panel, !grepl("R.p.o", district_name))

crime_panel <- filter(crime_panel, !grepl("S.t.f.", district_name))

crime_panel <- filter(crime_panel, !grepl("Spuwac", district_name))

crime_panel <- filter(crime_panel, !grepl("Stf", district_name))

crime_panel <- filter(crime_panel, !grepl("Chennaisuburban", district_name))

## Reformat parentheses, dashes, and other miscellaneous naming conventions.
crime_panel$district_name <- gsub(" \\(", " ", crime_panel$district_name)

crime_panel$district_name <- gsub("\\(", " ", crime_panel$district_name)

crime_panel$district_name <- gsub("\\)", "", crime_panel$district_name)

crime_panel$district_name <- gsub("-", " ", crime_panel$district_name)

crime_panel$district_name <- gsub("policedist", "", crime_panel$district_name)

crime_panel$district_name <- gsub(" Police District", "", crime_panel$district_name)

crime_panel$district_name <- gsub("Commr.", "City", crime_panel$district_name)

## Match all police district observations to 2001 district boundaries.

## The crime data are at the police district level. Some administrative districts
## have multiple police districts. As a result, I match all police districts to 
## their 2001 district boundaries to create a consistent balanced panel.

## Additionally, because new administrative districts are created during the time
## period, I must also match new administrative districts to their 2001 boundaries.

## Finally, I correct minor spelling differences in district names across years
## to ensure consistency. Below, I explain each change.

# Pre-2013, Agar was part of Shajapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Agar", "Shajapur", crime_panel$district_name)

# Ahmedabad City is Ahmadabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ahmedabad City", "Ahmadabad", crime_panel$district_name)

# Ahmedabad Rural is Ahmadabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ahmedabad Rural", "Ahmadabad", crime_panel$district_name)

# Ahmedabad is Ahmadabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ahmedabad", "Ahmadabad", crime_panel$district_name)

# Ahwa Dang is The Dangs.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ahwa Dang", "The Dangs", crime_panel$district_name)

crime_panel$district_name <- if_else(crime_panel$district_name=="Ahwa  Dang", "The Dangs", crime_panel$district_name)

# Alapuzha is an alternate spelling for Alleppey.
crime_panel$district_name <- if_else(crime_panel$district_name=="Alapuzha", "Alleppey", crime_panel$district_name)

crime_panel$district_name <- if_else(crime_panel$district_name=="Alappuzha", "Alleppey", crime_panel$district_name)

# Pre-2014, Alipurduar was part of Jalpaiguri.
crime_panel$district_name <- if_else(crime_panel$district_name=="Alipurduar", "Jalpaiguri", crime_panel$district_name)

# Pre-2008, Alirajpur was part of Jhabua.
crime_panel$district_name <- if_else(crime_panel$district_name=="Alirajpur", "Jhabua", crime_panel$district_name)

# Alwer is an alternate spelling for Alwar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Alwer", "Alwar", crime_panel$district_name)

# Pre-2012, Ambala was a single district.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ambala Rural", "Ambala", crime_panel$district_name)

# Pre-2012, Ambala was a single district.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ambala Urban", "Ambala", crime_panel$district_name)

# Amravati City is Amravati.
crime_panel$district_name <- if_else(crime_panel$district_name=="Amravati City", "Amravati", crime_panel$district_name)

# Amravati Rural is Amravati.
crime_panel$district_name <- if_else(crime_panel$district_name=="Amravati Rural", "Amravati", crime_panel$district_name)

# Amritsar Rural is Amritsar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Amritsar Rural", "Amritsar", crime_panel$district_name)

# Pre-2013, Amroha was named J.p. Nagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Amroha", "J.p.nagar", crime_panel$district_name)

# Anantpur is an alternate spelling for Anantapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Anantpur", "Anantapur", crime_panel$district_name)

# Pre-2009, Anjaw was part of Lohit.
crime_panel$district_name <- if_else(crime_panel$district_name=="Anjaw", "Lohit", crime_panel$district_name)

# Pre-2003, Anuppur was part of Shahdol.
crime_panel$district_name <- if_else(crime_panel$district_name=="Anuppur", "Shahdol", crime_panel$district_name)

# Pre-2014, Arvalli was part of Himatnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Arvalli", "Himatnagar", crime_panel$district_name)

# Ariyalur is part of Perambalur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ariyalur", "Perambalur", crime_panel$district_name)

# Pre-2001, Arwal was part of Jehanabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Arwal", "Jehanabad", crime_panel$district_name)

# Asansol is the seat of Barddhaman.
crime_panel$district_name <- if_else(crime_panel$district_name=="Asansol", "Barddhaman", crime_panel$district_name)

# Asansol Durgapur Pc is part of Barddhaman.
crime_panel$district_name <- if_else(crime_panel$district_name=="Asansol Durgapur Pc", "Barddhaman", crime_panel$district_name)

# Pre-2003, Ashok Nagar was part of Guna.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ashok Nagar", "Guna", crime_panel$district_name)

# Aurangabad City is part of Aurangabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Aurangabad City", "Aurangabad", crime_panel$district_name)

# Aurangabad Rural is part of Aurangabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Aurangabad Rural", "Aurangabad", crime_panel$district_name)

# Awantipora is part of Pulwama.
crime_panel$district_name <- if_else(crime_panel$district_name=="Awantipora", "Pulwama", crime_panel$district_name)

# Baddi is part of Solan.
crime_panel$district_name <- if_else(crime_panel$district_name=="Baddi", "Solan", crime_panel$district_name)

# Bagaha is part of Pashchim Champaran.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bagaha", "Pashchim Champaran", crime_panel$district_name)

# Baska is a misspelling of Baksa.
crime_panel$district_name <- if_else(crime_panel$district_name=="Baska", "Baksa", crime_panel$district_name)

# Pre-2005, Baksa was part of Kamrup.
crime_panel$district_name <- if_else(crime_panel$district_name=="Baksa", "Kamrup", crime_panel$district_name)

# Ballari is an alternate spelling for Bellary.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ballari", "Bellary", crime_panel$district_name)

# Pre 2012, Balod was part of Durg.
crime_panel$district_name <- if_else(crime_panel$district_name=="Balod", "Durg", crime_panel$district_name)

# Balodbazar is an alternate spelling for Baloda Bazar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Balodbazar", "Baloda Bazar", crime_panel$district_name)

# Pre-2012, Baloda Bazar was part of Raipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Baloda Bazar", "Raipur", crime_panel$district_name)

# Balrampur is in Uttar Pradesh, not Chhattisgarh.
crime_panel$state_name <- if_else(crime_panel$district_name=="Balrampur", "Uttar Pradesh", crime_panel$state_name)

# Pre-2008, Bandipora was part of Baramulla.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bandipora", "Baramulla", crime_panel$district_name)

# Bangalore City is an alternate spelling for Bangalore Commr.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bangalore City", "Bangalore Commr.", crime_panel$district_name)

# Bareily is an alternate spelling for Bareilly.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bareily", "Bareilly", crime_panel$district_name)

# Bengaluru City is an alternate spelling for Bangalore Commr.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bengaluru City", "Bangalore Commr.", crime_panel$district_name)

# Bengaluru District is an alternate name for Bangalore Rural.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bengaluru District", "Bangalore Rural", crime_panel$district_name)

# Banaskantha is an alternate spelling for Banas Kantha.
crime_panel$district_name <- if_else(crime_panel$district_name=="Banaskantha", "Banas Kantha", crime_panel$district_name)

# Pre 2006, Barnala was part of Sangrur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Barnala", "Sangrur", crime_panel$district_name)

# Barrackpur Pc is part of North 24 Parganas.
crime_panel$district_name <- if_else(crime_panel$district_name=="Barrackpur Pc", "North 24 Parganas", crime_panel$district_name)

# Batala is part of Gurdaspur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Batala", "Gurdaspur", crime_panel$district_name)

# Bathinda is an alternate spelling for Bhatinda.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bathinda", "Bhatinda", crime_panel$district_name)

# Bdn Cp is Bidhannagar police, in North 24 Parganas.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bdn Cp", "North 24 Parganas", crime_panel$district_name)

# Pre-2013, Belagavi City was part of Belgaum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Belagavi City", "Belgaum", crime_panel$district_name)

# Pre-2013, Belagavi District was part of Belgaum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Belagavi District", "Belgaum", crime_panel$district_name)

# Pre-2012, Bemetara was part of Durg.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bemetara", "Durg", crime_panel$district_name)

# Pre-2012, Bemetra was part of Durg.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bemetra", "Durg", crime_panel$district_name)

# Berhampur is part of Ganjam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Berhampur", "Ganjam", crime_panel$district_name)

# Bettiah is part of Pashchim Champaran.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bettiah", "Pashchim Champaran", crime_panel$district_name)

# Pre-2011, Bhim Nagar was part of Moradabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bhim Nagar", "Moradabad", crime_panel$district_name)

# Bidhannagar Pc is Bidhannagar police, in North 24 Parganas.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bidhannagar Pc", "North 24 Parganas", crime_panel$district_name)

# Pre 2007, Bizapur was part of Dantewara.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bizapur", "Dantewara", crime_panel$district_name)

# Bkp Cp is part of North 24 Parganas.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bkp Cp", "North 24 Parganas", crime_panel$district_name)

# Budgam is an alterante spelling for Badgam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Budgam", "Badgam", crime_panel$district_name)

# Bulandshahr is an alternate spelling for Bulandshahar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Bulandshahr", "Bulandshahar", crime_panel$district_name)

# Burdwan is an alternate spelling for Barddhaman.
crime_panel$district_name <- if_else(crime_panel$district_name=="Burdwan", "Barddhaman", crime_panel$district_name)

# Pre-2003, Burhanpur was part of Khandwa.
crime_panel$district_name <- if_else(crime_panel$district_name=="Burhanpur", "Khandwa", crime_panel$district_name)

# Calcutta is an alternate spelling for Kolkata.
crime_panel$district_name <- if_else(crime_panel$district_name=="Calcutta", "Kolkata", crime_panel$district_name)

# Pre-2008, Cbpura was part of Kolar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Cbpura", "Kolar", crime_panel$district_name)

# Chaibasa is part of Pashchimi Singhbhum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chaibasa", "Pashchimi Singhbhum", crime_panel$district_name)

# Chamaranjnagar is an alternate spelling for Chamarajnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chamaranjnagar", "Chamarajnagar", crime_panel$district_name)

# Pre-2014, Chhotaudepur was part of Vadodara City.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chhotaudepur", "Vadodara City", crime_panel$district_name)

# Pre-2008, Chikkaballapura was part of Kolar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chikkaballapura", "Kolar", crime_panel$district_name)

# Chikkamagaluru is an alternate spelling for Chickmagalur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chikkamagaluru", "Chickmagalur", crime_panel$district_name)

# Pre-2005, Chirang was part of Bongaigaon.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chirang", "Bongaigaon", crime_panel$district_name)

# Coimbatore City is in Coimbatore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Coimbatore City", "Coimbatore", crime_panel$district_name)

# Coimbatore Rural is in Coimbatore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Coimbatore Rural", "Coimbatore", crime_panel$district_name)

# Coimbatore Urban is in Coimbatore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Coimbatore Urban", "Coimbatore", crime_panel$district_name)

# Cp Amritsar is an alternate name for Amritsar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Cp Amritsar", "Amritsar", crime_panel$district_name)

# Cp Jalandhar is an alternate name for  Jalandhar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Cp Jalandhar", "Jalandhar", crime_panel$district_name)

# Cp Ludhiana is an alternate name for Ludhiana.
crime_panel$district_name <- if_else(crime_panel$district_name=="Cp Ludhiana", "Ludhiana", crime_panel$district_name)

# Chitrakoot is an alternate spelling for Chitrakoot Dham.
crime_panel$district_name <- if_else(crime_panel$district_name=="Chitrakoot", "Chitrakoot Dham", crime_panel$district_name)

# Cyberabad is in Rangareddy.
crime_panel$district_name <- if_else(crime_panel$district_name=="Cyberabad", "Rangareddy", crime_panel$district_name)

# Dakshina Kannada is an alternate spelling for Dakshin Kannada.
crime_panel$district_name <- if_else(crime_panel$district_name=="Dakshina Kannada", "Dakshin Kannada", crime_panel$district_name)

# Dang is an alternate name for The Dangs.
crime_panel$district_name <- if_else(crime_panel$district_name=="Dang", "The Dangs", crime_panel$district_name)

# Dantewada is an alternate spelling for Dantewara.
crime_panel$district_name <- if_else(crime_panel$district_name=="Dantewada", "Dantewara", crime_panel$district_name)

# Deogarh is an alternate spelling for Deoghar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Deogarh", "Deoghar", crime_panel$district_name)

# Deoghar is in Jharkhand, not Orissa.
crime_panel$district_name <- if_else(crime_panel$district_name=="Deogarh", "Deoghar", crime_panel$district_name)

# In Orissa, Deoghar is an alternate spelling for Debagarh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Deoghar" & crime_panel$state_name=="Odisha", "Debagarh", crime_panel$district_name)

# Pre-2013, Devbhumi Dwarka was part of Jamnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Devbhumi Dwarka", "Jamnagar", crime_panel$district_name)

# Datia is an alternate spelling for Datiya.
crime_panel$district_name <- if_else(crime_panel$district_name=="Datia", "Datiya", crime_panel$district_name)

# Dhalal is an alternate spelling for Dhalai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Dhalal", "Dhalai", crime_panel$district_name)

# Dharwad City is part of Dharwad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Dharwad City", "Dharwad", crime_panel$district_name)

# Dharwad Rural is part of Dharwad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Dharwad Rural", "Dharwad", crime_panel$district_name)

# Pre-2002, Ernakulam City was part of Ernakulam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ernakulam City", "Ernakulam", crime_panel$district_name)

# Pre-2002, Ernakulam Rural was part of Ernakulam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ernakulam Rural", "Ernakulam", crime_panel$district_name)

# Fatehgarh is part of Farrukhabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Fatehgarh", "Farrukhabad", crime_panel$district_name)

# Fathegarh is part of Farrukhabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Fathegarh", "Farrukhabad", crime_panel$district_name)

# Pre-2011, Fazilka was part of Ferozpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Fazilka", "Ferozpur", crime_panel$district_name)

# Ferozepur is an alternate spelling for Ferozpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ferozepur", "Ferozpur", crime_panel$district_name)

# Pre-2007 Ganderbal was part of Srinagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ganderbal", "Srinagar", crime_panel$district_name)

# Pre-2011, Gariyaband was part of Raipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Gariyaband", "Raipur", crime_panel$district_name)

# Pre-2013, Garo Hills North was part of Garo Hills East.
crime_panel$district_name <- if_else(crime_panel$district_name=="Garo Hills North", "Garo Hills East", crime_panel$district_name)

# Pre 2013, Garo Hills South West was part of Garo Hills West.
crime_panel$district_name <- if_else(crime_panel$district_name=="Garo Hills South W.", "Garo Hills West", crime_panel$district_name)

# Pre 2013, Garo Hills South West was part of Garo Hills West.
crime_panel$district_name <- if_else(crime_panel$district_name=="Garo Hills South West", "Garo Hills West", crime_panel$district_name)

# Pre 2014, Gir Somnath was part of Junagadh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Gir Somnath", "Junagadh", crime_panel$district_name)

# Gopalpara is an alternate spelling for Goalpara	.
crime_panel$district_name <- if_else(crime_panel$district_name=="Gopalpara", "Goalpara", crime_panel$district_name)

# Pre-2012, Gomati was part of South Tripura.
crime_panel$district_name <- if_else(crime_panel$district_name=="Gomati" & crime_panel$state_name=="Tripura", "South", crime_panel$district_name)

# Guntur Urban is in Guntur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Guntur Urban", "Guntur", crime_panel$district_name)

# Gurdapur is an alternate spelling for Gurdaspur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Gurdapur", "Gurdaspur", crime_panel$district_name)

# Guwahati City is part of Kamrup.
crime_panel$district_name <- if_else(crime_panel$district_name=="Guwahati City", "Kamrup", crime_panel$district_name)

# Guwahati City is part of Kamrup.
crime_panel$district_name <- if_else(crime_panel$district_name=="Guwahati", "Kamrup", crime_panel$district_name)

# Hamren is part of Karbi Anglong.
crime_panel$district_name <- if_else(crime_panel$district_name=="Hamren", "Karbi Anglong", crime_panel$district_name)

# Handwara is part of Kupwara.
crime_panel$district_name <- if_else(crime_panel$district_name=="Handwara", "Kupwara", crime_panel$district_name)

# Pre-2011, Hapur (renamed from Panchshil Nagar) was part of Ghaziabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Hapur", "Ghaziabad", crime_panel$district_name)

# Howrah City is part of Howrah.
crime_panel$district_name <- if_else(crime_panel$district_name=="Howrah City", "Howrah", crime_panel$district_name)

# Howrah Pc is part of Howrah.
crime_panel$district_name <- if_else(crime_panel$district_name=="Howrah Pc", "Howrah", crime_panel$district_name)

# Howrah Rural is part of Howrah.
crime_panel$district_name <- if_else(crime_panel$district_name=="Howrah Rural", "Howrah", crime_panel$district_name)

# Hubballi Dharwad City is in Dharwad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Hubballi Dharwad City", "Dharwad", crime_panel$district_name)

# Imphal East is part of Imphal.
crime_panel$district_name <- if_else(crime_panel$district_name=="Imphal East", "Imphal", crime_panel$district_name)

# Imphal West is part of Imphal.
crime_panel$district_name <- if_else(crime_panel$district_name=="Imphal West", "Imphal", crime_panel$district_name)

# Jagdalpur is an alternate name for Bastar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jagdalpur", "Bastar", crime_panel$district_name)

# Jagraon is part of Ludhiana.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jagraon", "Ludhiana", crime_panel$district_name)

# Pre-2013, Jaintia Hills East was part of Jaintia Hills.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaintia Hills East", "Jaintia Hills", crime_panel$district_name)

# Pre-2013, Jaintia Hills West was part of Jaintia Hills.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaintia Hills West", "Jaintia Hills", crime_panel$district_name)

# Jaipur East is part of Jaipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaipur East", "Jaipur", crime_panel$district_name)

# Jaipur North is part of Jaipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaipur North", "Jaipur", crime_panel$district_name)

# Jaipur Rural is part of Jaipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaipur Rural", "Jaipur", crime_panel$district_name)

# Jaipur South is part of Jaipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaipur South", "Jaipur", crime_panel$district_name)

# Jaipur West is part of Jaipur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jaipur West", "Jaipur", crime_panel$district_name)

# Jalandhar Rural is Jalandhar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jalandhar Rural", "Jalandhar", crime_panel$district_name)

# Jamshedpur is part of Purbi Singhbhum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jamshedpur", "Purbi Singhbhum", crime_panel$district_name)

# Pre-2001, Jamtara was part of Dumka.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jamtara", "Dumka", crime_panel$district_name)

# Pre-2012, Jhargram was part of Paschim Midnapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jhargram", "Paschim Midnapur", crime_panel$district_name)

# Jodhpur City is part of Jodhpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jodhpur City", "Jodhpur", crime_panel$district_name)

# Jodhpur East is part of Jodhpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jodhpur East", "Jodhpur", crime_panel$district_name)

# Jodhpur Rural is part of Jodhpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jodhpur Rural", "Jodhpur", crime_panel$district_name)

# Jodhpur West is part of Jodhpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Jodhpur West", "Jodhpur", crime_panel$district_name)

# Karimaganj is an alternate spelling for Karimganj.
crime_panel$district_name <- if_else(crime_panel$district_name=="Karimaganj", "Karimganj", crime_panel$district_name)

# Kochy City is part of Ernakulam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kochy City", "Ernakulam", crime_panel$district_name)

# K.g.f. is Kolar.
crime_panel$district_name <- if_else(crime_panel$district_name=="K.g.f.", "Kolar", crime_panel$district_name)

# Pre-2003, K/Kumey was part of Subansiri Lower.
crime_panel$district_name <- if_else(crime_panel$district_name=="K/Kumey", "Subansiri Lower", crime_panel$district_name)

# Kabirdham is an alternate name for Kawardha.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kabirdham", "Kawardha", crime_panel$district_name)

# Kachchh East G is part of Kutch.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kachchh East G", "Kutch", crime_panel$district_name)

# Kachchh West B is part of Kutch.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kachchh West B", "Kutch", crime_panel$district_name)

# Kalaburgi is an alternate name for Gulbarga.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kalaburgi", "Gulbarga", crime_panel$district_name)

# Kancheepuram is an alternate spelling for Kanchipuram.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kancheepuram", "Kanchipuram", crime_panel$district_name)

# Pre-2008, Kanshiram Nagar was part of Etah.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kanshiram Nagar", "Etah", crime_panel$district_name)

# Kanyakummari is an alternate spelling for Kanyakumari.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kanyakummari", "Kanyakumari", crime_panel$district_name)

# Kasganj is the new nape of Kanshiram Nagar, which pre-2008 was part of Etah.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kasganj", "Etah", crime_panel$district_name)

# Keonjhar is an alternate spelling for Kendujhar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Keonjhar", "Kendujhar", crime_panel$district_name)

# Khandwa was formerly known as East Nimar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khandwa", "East Nimar", crime_panel$district_name)

# Khanna is part of Ludhiana.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khanna", "Ludhiana", crime_panel$district_name)

# Khargon was formerly known as West Nimar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khargon", "West Nimar", crime_panel$district_name)

# Khargone is an alternate spelling for Khargon, which was formerly known as West Nimar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khargone", "West Nimar", crime_panel$district_name)

# Pre-2013, Khasi Hills South W. was part of West Khasi Hills.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khasi Hills South W.", "Khasi Hills West", crime_panel$district_name)

# Pre-2013, Khasi Hills South West was part of West Khasi Hills.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khasi Hills South West", "Khasi Hills West", crime_panel$district_name)

# Kheda is Kheda North.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kheda", "Kheda North", crime_panel$district_name)

# Pre-2013, Khowai was part of West Tripura.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khowai" & crime_panel$state_name=="Tripura", "West", crime_panel$district_name)

# Pre-2007, Khunti was part of Ranchi.
crime_panel$district_name <- if_else(crime_panel$district_name=="Khunti", "Ranchi", crime_panel$district_name)

# Kiphire was previously part of Tuensang.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kiphire", "Tuensang", crime_panel$district_name)

# Pre-2008, Kishtwar was part of Doda.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kishtwar", "Doda", crime_panel$district_name)

# Kodarma is an alternate spelling for Koderma.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kodarma", "Koderma", crime_panel$district_name)

# Pre-2011, Kollam City was part of Kollam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kollam City", "Kollam", crime_panel$district_name)

# Pre-2011, Kollam Rural was part of Kollam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kollam Rural", "Kollam", crime_panel$district_name)

# Pre-2012, Kondagaon was part of Bastar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kondagaon", "Bastar", crime_panel$district_name)

# Pre-2006, Kota City was part of Kota.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kota City", "Kota", crime_panel$district_name)

# Pre-2006, Kota Rural was part of Kota.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kota Rural", "Kota", crime_panel$district_name)

# Pre-2013, Kowai was part of West Tripura.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kowai" & crime_panel$state_name=="Tripura", "West", crime_panel$district_name)

# Pre-2003, Kozhikode City was part of Kozhikode.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kozhikode City", "Kozhikode", crime_panel$district_name)

# Pre-2003, Kozhikode Rural was part of Kozhikode.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kozhikode Rural", "Kozhikode", crime_panel$district_name)

# Pre-2004, Krishnagiri was part of Dharmapuri.
crime_panel$district_name <- if_else(crime_panel$district_name=="Krishnagiri", "Dharmapuri", crime_panel$district_name)

# Pre-2003, Kukung Kumey was part of Subansiri Lower.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kukung Kumey", "Subansiri Lower", crime_panel$district_name)

# Pre-2007, Kulgam was part of Anantnag.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kulgam", "Anantnag", crime_panel$district_name)

# Kutch East G is part of Kutch.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kutch East G", "Kutch", crime_panel$district_name)

# Kutch West Bhuj is part of Kutch.
crime_panel$district_name <- if_else(crime_panel$district_name=="Kutch West Bhuj", "Kutch", crime_panel$district_name)

# Lahaul & Spiti is an alternate spelling for Lahaul-Spiti.
crime_panel$district_name <- if_else(crime_panel$district_name=="Lahaul & Spiti", "Lahaul-Spiti", crime_panel$district_name)

# Lahaul Spiti is an alternate spelling for Lahaul-Spiti.
crime_panel$district_name <- if_else(crime_panel$district_name=="Lahaul Spiti", "Lahaul-Spiti", crime_panel$district_name)

# Lakhisaraj is an alternate spelling for Lakhisarai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Lakhisaraj", "Lakhisarai", crime_panel$district_name)

# Pre-2001, Latehar was part of Palamu.
crime_panel$district_name <- if_else(crime_panel$district_name=="Latehar", "Palamu", crime_panel$district_name)

# Loharadagga is an alternate spelling for Lohardagga.
crime_panel$district_name <- if_else(crime_panel$district_name=="Loharadagga", "Lohardagga", crime_panel$district_name)

# Pre-2013, Longding was part of Tirap.
crime_panel$district_name <- if_else(crime_panel$district_name=="Longding", "Tirap", crime_panel$district_name)

# Pre-2007, Longleng was part of Tuensang.
crime_panel$district_name <- if_else(crime_panel$district_name=="Longleng", "Tuensang", crime_panel$district_name)

# Lower Dibang Valley is part of Dibang Valley.
crime_panel$district_name <- if_else(crime_panel$district_name=="Lower Dibang Valley", "Dibang Valley", crime_panel$district_name)

# Ludhiana Rural is part of Ludhiana.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ludhiana Rural", "Ludhiana", crime_panel$district_name)

# Madurai City is part of Madurai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Madurai City", "Madurai", crime_panel$district_name)

# Madurai Rural is part of Madurai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Madurai Rural", "Madurai", crime_panel$district_name)

# Madurai Urban is part of Madurai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Madurai Urban", "Madurai", crime_panel$district_name)

# Mahaboob Nagar is an alternate spelling for Mahaboobnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mahaboob Nagar", "Mahaboobnagar", crime_panel$district_name)

# Mahendergarh is an alternate spelling for Mahendragarh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mahendergarh", "Mahendragarh", crime_panel$district_name)

# Majitha is part of Amritsar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Majitha", "Amritsar", crime_panel$district_name)

# Malkangiri is an alternate spelling for Malkangir.
crime_panel$district_name <- if_else(crime_panel$district_name=="Malkangiri", "Malkangir", crime_panel$district_name)

# Mangalore City is part of Dakshin Kannada.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mangalore City", "Dakshin Kannada", crime_panel$district_name)

# Mangaluru City is part of Dakshin Kannada.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mangaluru City", "Dakshin Kannada", crime_panel$district_name)

# Madhepura is an alternate spelling for Medhepura.
crime_panel$district_name <- if_else(crime_panel$district_name=="Madhepura", "Medhepura", crime_panel$district_name)

# Mahabob Nagar is an alternate spelling for Mahaboobnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mahabob Nagar", "Mahaboobnagar", crime_panel$district_name)

# Meshana is an alternate spelling for Mehsana.
crime_panel$district_name <- if_else(crime_panel$district_name=="Meshana", "Mehsana", crime_panel$district_name)

# Muraina is an alternate spelling for Morena.
crime_panel$district_name <- if_else(crime_panel$district_name=="Muraina", "Morena", crime_panel$district_name)

# Motihari is part of Purba Champaran.
crime_panel$district_name <- if_else(crime_panel$district_name=="Motihari", "Purba Champaran", crime_panel$district_name)

# Mumbai City is part of Mumbai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mumbai City", "Mumbai", crime_panel$district_name)

# Pre-2012, Mungali was part of Bilaspur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mungali", "Bilaspur", crime_panel$district_name)

# Pre-2012, Mungeli was part of Bilaspur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mungeli", "Bilaspur", crime_panel$district_name)

# Muzaffaranagar is an alternate spelling for Muzaffarnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Muzaffaranagar", "Muzaffarnagar", crime_panel$district_name)

# Mysuru City is an alternate spelling for Mysore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mysuru City", "Mysore", crime_panel$district_name)

# Mysuru District is an alternate spelling for Mysore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mysuru District", "Mysore", crime_panel$district_name)

# Mysore City is part of Mysore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mysore City", "Mysore", crime_panel$district_name)

# Mysore rural is part of Mysore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Mysore Rural", "Mysore", crime_panel$district_name)

# N. C. Hills is an alternate spelling for N.c.hills.
crime_panel$district_name <- if_else(crime_panel$district_name=="N. C. Hills", "N.c.hills", crime_panel$district_name)

# Nabarangpur is an alternate spelling for Nowrangpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Nabarangpur", "Nowrangpur", crime_panel$district_name)

# Nagpur City is part of Nagpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Nagpur City", "Nagpur", crime_panel$district_name)

# Nagpur Rural is part of Nagpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Nagpur Rural", "Nagpur", crime_panel$district_name)

# Narasinghpur is an alternate spelling for Narsinghpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Narasinghpur", "Narsinghpur", crime_panel$district_name)

# Pre-2003, Narayanpur was part of Bastar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Narayanpur", "Bastar", crime_panel$district_name)

# Nasik City is part of Nasik.
crime_panel$district_name <- if_else(crime_panel$district_name=="Nasik City", "Nasik", crime_panel$district_name)

# Nasik Rural is part of Nasik.
crime_panel$district_name <- if_else(crime_panel$district_name=="Nasik Rural", "Nasik", crime_panel$district_name)

# Naugachia is part of Bhagalpur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Naugachia", "Bhagalpur", crime_panel$district_name)

# Navi Mumbai is part of Mumbai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Navi Mumbai", "Mumbai", crime_panel$district_name)

# N.c. Hills is an alternate spelling for N.c.hills.
crime_panel$district_name <- if_else(crime_panel$district_name=="N.c. Hills", "N.c.hills", crime_panel$district_name)

# New Bombay is part of Mumbai.
crime_panel$district_name <- if_else(crime_panel$district_name=="New Bombay", "Mumbai", crime_panel$district_name)

# Nizambad is an alternate spelling for Nizamabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Nizambad", "Nizamabad", crime_panel$district_name)

# North 24 Parganas	is an alternate spelling for 24 Parganas North.
crime_panel$district_name <- if_else(crime_panel$district_name=="North 24 Parganas", "24 Parganas North", crime_panel$district_name)

# North East Delhi is an alternate spelling for North-East Delhi.
crime_panel$district_name <- if_else(crime_panel$district_name=="North East", "North-East", crime_panel$district_name)

# North West Delhi is an alternate spelling for North-West Delhi.
crime_panel$district_name <- if_else(crime_panel$district_name=="North West", "North-West", crime_panel$district_name)

# Palamau is an alternate spelling for Palamu.
crime_panel$district_name <- if_else(crime_panel$district_name=="Palamau", "Palamu", crime_panel$district_name)

# Palanpur is part of Banas Kantha.
crime_panel$district_name <- if_else(crime_panel$district_name=="Palanpur", "Banas Kantha", crime_panel$district_name)

# Pre-2014, Palghar was part of Thane.
crime_panel$district_name <- if_else(crime_panel$district_name=="Palghar", "Thane", crime_panel$district_name)

# Pre-2006, Palwal was part of Faridabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Palwal", "Faridabad", crime_panel$district_name)

# Panchmahals is an alternate spelling for Panchmahal.
crime_panel$district_name <- if_else(crime_panel$district_name=="Panchmahals", "Panchmahal", crime_panel$district_name)

# Pre-2011, Panchshil Nagar was part of Ghaziabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Panchshil Nagar", "Ghaziabad", crime_panel$district_name)

# Papum Pare City is part of Papum Pare.
crime_panel$district_name <- if_else(crime_panel$district_name=="Papum Pare City", "Papum Pare", crime_panel$district_name)

# Papum Pare Rural is part of Papum Pare.
crime_panel$district_name <- if_else(crime_panel$district_name=="Papum Pare Rural", "Papum Pare", crime_panel$district_name)

# Paschim Medinipur is part of Midnapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Paschim Medinipur", "Midnapur", crime_panel$district_name)

# Paschim Midnapur is part of Midnapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Paschim Midnapur", "Midnapur", crime_panel$district_name)

# Pashchim Champaran is an alternate spelling for Paschim Champaran.
crime_panel$district_name <- if_else(crime_panel$district_name=="Pashchim Champaran", "Paschim Champaran", crime_panel$district_name)

# Pre-2011, Pathankot was part of Gurdaspur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Pathankot", "Gurdaspur", crime_panel$district_name)

# Pre-2003, Peren was part of Kohima.
crime_panel$district_name <- if_else(crime_panel$district_name=="Peren", "Kohima", crime_panel$district_name)

# Pathoragarh is an alternate spelling for Pithoragarh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Pathoragarh", "Pithoragarh", crime_panel$district_name)

# Praapgarh is an alternate spelling for Pratapgarh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Praapgarh", "Pratapgarh", crime_panel$district_name)

# Pratapgarh is in Uttar Pradesh.
crime_panel$state_name <- if_else(crime_panel$district_name=="Pratapgarh", "Uttar Pradesh", crime_panel$state_name)

# Pre-2011, Prabuddh Nagar was part of Muzaffarnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Prabuddh Nagar", "Muzaffarnagar", crime_panel$district_name)

# Pune City is part of Pune.
crime_panel$district_name <- if_else(crime_panel$district_name=="Pune City", "Pune", crime_panel$district_name)

# Pune Rural is part of Pune.
crime_panel$district_name <- if_else(crime_panel$district_name=="Pune Rural", "Pune", crime_panel$district_name)

# Purab Medinipur is part of Midnapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Purab Medinipur", "Midnapur", crime_panel$district_name)

# Purab Midnapur is part of Midnapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Purab Midnapur", "Midnapur", crime_panel$district_name)

# Purba Champaran	is an alternate spelling for Purbi Champaran.
crime_panel$district_name <- if_else(crime_panel$district_name=="Purba Champaran", "Purbi Champaran", crime_panel$district_name)

# Rajahmundry is part of East Godavari.
crime_panel$district_name <- if_else(crime_panel$district_name=="Rajahmundry", "East Godavari", crime_panel$district_name)

# Rajkot City is part of Rajkot.
crime_panel$district_name <- if_else(crime_panel$district_name=="Rajkot City", "Rajkot", crime_panel$district_name)

# Rajkot Rural is part of Rajkot.
crime_panel$district_name <- if_else(crime_panel$district_name=="Rajkot Rural", "Rajkot", crime_panel$district_name)

# Ramabai Nagar is an alternate name for Kanpur Dehat.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ramabai Nagar", "Kanpur Dehat", crime_panel$district_name)

# Pre-2008, Ramanagar was part of Bangalore Rural.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ramanagar", "Bangalore Rural", crime_panel$district_name)

# Pre-2007 Ramban	was part of Doda.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ramban", "Doda", crime_panel$district_name)

# Pre-2007, Ramgarh was part of Hazaribagh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ramgarh", "Hazaribagh", crime_panel$district_name)

# Ramnathapurram is an alternate spelling Ramnathapuram.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ramnathapurram", "Ramnathapuram", crime_panel$district_name)

# Rangareddy is an alternate spelling for Ranga Reddy.
crime_panel$district_name <- if_else(crime_panel$district_name=="Rangareddy", "Ranga Reddy", crime_panel$district_name)

# Reasi is part of Udhampur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Reasi", "Udhampur", crime_panel$district_name)

# Ri Bhoi	is an alternate spelling for Ri-Bhoi.	
crime_panel$district_name <- if_else(crime_panel$district_name=="Ri Bhoi", "Ri-Bhoi", crime_panel$district_name)

# Riwa is an alternate spelling for Rewa.
crime_panel$district_name <- if_else(crime_panel$district_name=="Riwa", "Rewa", crime_panel$district_name)

# Ropar is an alternate spelling for Rupnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Ropar", "Rupnagar", crime_panel$district_name)

# Rourkela is part of Sundargarh.
crime_panel$district_name <- if_else(crime_panel$district_name=="Rourkela", "Sundargarh", crime_panel$district_name)

# Sabarkantha is an alternate name for Himatnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sabarkantha", "Himatnagar", crime_panel$district_name)

# Salem City is part of Salem.
crime_panel$district_name <- if_else(crime_panel$district_name=="Salem City", "Salem", crime_panel$district_name)

# Salem Rural is part of Salem.
crime_panel$district_name <- if_else(crime_panel$district_name=="Salem Rural", "Salem", crime_panel$district_name)

# Salem Urban is part of Salem.
crime_panel$district_name <- if_else(crime_panel$district_name=="Salem Urban", "Salem", crime_panel$district_name)

# Pre-2011, Sambhal was part of Moradabad.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sambhal", "Moradabad", crime_panel$district_name)

# Sant Ravidasnagar is an alternate spelling for St.ravidasnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sant Ravidasnagar", "St.ravidasnagar", crime_panel$district_name)

# Pre-2001, Saraikela was part of	Pashchimi Singhbhum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Saraikela", "Pashchimi Singhbhum", crime_panel$district_name)

# Sas Ngr is an alternate spelling for Sas Nagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sas Ngr", "Sas Nagar", crime_panel$district_name)

# Sawal Madhopur is an alternate spelling for Sawai Madhopur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sawal Madhopur", "Sawai Madhopur", crime_panel$district_name)

# Sehore is an alternate spelling for Sihore.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sehore", "Sihore", crime_panel$district_name)

# Pre-2013, Shamli was part of Muzaffarnagar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Shamli", "Muzaffarnagar", crime_panel$district_name)

# Pre-2007, Shopian was part of Pulwama.
crime_panel$district_name <- if_else(crime_panel$district_name=="Shopian", "Pulwama", crime_panel$district_name)

# Siang East was previously called Siang Upper.
crime_panel$district_name <- if_else(crime_panel$district_name=="Siang East", "Siang Upper", crime_panel$district_name)

# Siliguri Pc is a city part of both mostly (Darjeeling) and Jalpaiguri (assigned to Darjeeling).
crime_panel$district_name <- if_else(crime_panel$district_name=="Siliguri Pc", "Darjeeling", crime_panel$district_name)

# Siliguri_pc is a city part of both mostly (Darjeeling) and Jalpaiguri (assigned to Darjeeling).
crime_panel$district_name <- if_else(crime_panel$district_name=="Siliguri_pc", "Darjeeling", crime_panel$district_name)

# Simdega was previously part of Gumla.
crime_panel$district_name <- if_else(crime_panel$district_name=="Simdega", "Gumla", crime_panel$district_name)

# Pre-2008, Singrauli was part of Sidhi.
crime_panel$district_name <- if_else(crime_panel$district_name=="Singrauli", "Sidhi", crime_panel$district_name)

# Pre-2012, Sipahijala was part of West Tripura.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sipahijala" & crime_panel$state_name=="Tripura", "West", crime_panel$district_name)

# Sivni is an alternate spelling for Seoni.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sivni", "Seoni", crime_panel$district_name)

# Solapur City is part of Solapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Solapur City", "Solapur", crime_panel$district_name)

# Solapur Rural is part of Solapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Solapur Rural", "Solapur", crime_panel$district_name)

# Sopore is part of Baramulla.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sopore", "Baramulla", crime_panel$district_name)

# South 24 Parganas	is an alternate spelling for 24 Parganas South.
crime_panel$district_name <- if_else(crime_panel$district_name=="South 24 Parganas", "24 Parganas South", crime_panel$district_name)

# South West is an alternate spelling for South-West.
crime_panel$district_name <- if_else(crime_panel$district_name=="South West", "South-West", crime_panel$district_name)

# Pre-2012, Sukma was part of Bastar.
crime_panel$district_name <- if_else(crime_panel$district_name=="Sukma", "Bastar", crime_panel$district_name)

# Surat City is part of Surat.
crime_panel$district_name <- if_else(crime_panel$district_name=="Surat City", "Surat", crime_panel$district_name)

# Surat Rural is part of Surat.
crime_panel$district_name <- if_else(crime_panel$district_name=="Surat Rural", "Surat", crime_panel$district_name)

# Pre-2008, Tapi was part of Surat.
crime_panel$district_name <- if_else(crime_panel$district_name=="Tapi", "Surat", crime_panel$district_name)

# Taran Taran is an alternate spelling for Tarn Taran.
crime_panel$district_name <- if_else(crime_panel$district_name=="Taran Taran", "Tarn Taran", crime_panel$district_name)

# Thane City is part of Thane.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thane City", "Thane", crime_panel$district_name)

# Thane Rural is part of Thane.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thane Rural", "Thane", crime_panel$district_name)

crime_panel$district_name <- if_else(crime_panel$district_name=="Thanerural", "Thane", crime_panel$district_name)

# Thanjiavur is an alternate spelling for Thanjavur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thanjiavur", "Thanjavur", crime_panel$district_name)

# The Nilgiris is an alternate name for Nilgiris.
crime_panel$district_name <- if_else(crime_panel$district_name=="The Nilgiris", "Nilgiris", crime_panel$district_name)

# Thirunelveli City is part of Thirunelveli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thirunelveli City", "Tirunelveli", crime_panel$district_name)

# Thirunelveli Rural is part of Thirunelveli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thirunelveli Rural", "Tirunelveli", crime_panel$district_name)

# Thirunelveli Urban is part of Thirunelveli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thirunelveli Urban", "Tirunelveli", crime_panel$district_name)

# Thirunelveli is an alternate spelling for Tirunelveli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thirunelveli", "Tirunelveli", crime_panel$district_name)

# Thirunel Veli Rural is part of Tirunelveli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thirunel Veli Rural", "Tirunelveli", crime_panel$district_name)

# Thirunel Veli Urban is part of Tirunelveli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thirunel Veli Urban", "Tirunelveli", crime_panel$district_name)

# Thiruvannamalal is an alternate spelling for Thiruvannamalai.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thiruvannamalal", "Thiruvannamalai", crime_panel$district_name)

# Thrissur City is part of Trishshur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thrissur City", "Trishshur", crime_panel$district_name)

# Thrissur Rural is part of Trishshur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thrissur Rural", "Trishshur", crime_panel$district_name)

# Thrissur is an alternate spelling for Trishshur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Thrissur", "Trishshur", crime_panel$district_name)

# Tirupathi Urban is part of Chittoor.
crime_panel$district_name <- if_else(crime_panel$district_name=="Tirupathi Urban", "Chittoor", crime_panel$district_name)

# Tiruppur City is part of Tiruppur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Tiruppur City", "Tiruppur", crime_panel$district_name)

# Tiruvalluvar is an alternate spelling for Thiruvallur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Tiruvalluvar", "Thiruvallur", crime_panel$district_name)

# Trichy City is part of Trichy.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trichy City", "Tiruchchirappalli", crime_panel$district_name)

# Trichy Rural is part of Trichy.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trichy Rural", "Tiruchchirappalli", crime_panel$district_name)

# Trichy Urban is part of Trichy.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trichy Urban", "Tiruchchirappalli", crime_panel$district_name)

# Trichy is an alternate spelling for Tiruchchirappalli.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trichy", "Tiruchchirappalli", crime_panel$district_name)

# Trishshur is an alternate spelling for Thrissur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trishshur", "Thrissur", crime_panel$district_name)

# Trivandrum City	is part of Trivandrum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trivandrum City", "Trivandrum", crime_panel$district_name)

# Trivandrum Rural is part of Trivandrum.
crime_panel$district_name <- if_else(crime_panel$district_name=="Trivandrum Rural", "Trivandrum", crime_panel$district_name)

# Tumakuru is an alternate spelling for Tumkur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Tumakuru", "Tumkur", crime_panel$district_name)

# Pre-2005, Udalguri was part of Darrang.
crime_panel$district_name <- if_else(crime_panel$district_name=="Udalguri", "Darrang", crime_panel$district_name)

# Umaria is an alternate spelling for Umariya.
crime_panel$district_name <- if_else(crime_panel$district_name=="Umaria", "Umariya", crime_panel$district_name)

# Pre-2012, Unakoti was part of North Tripura.
crime_panel$district_name <- if_else(crime_panel$district_name=="Unakoti" & crime_panel$state_name=="Tripura", "North", crime_panel$district_name)

# Upper Dibang Valley	is part of Dibang Valley.
crime_panel$district_name <- if_else(crime_panel$district_name=="Upper Dibang Valley", "Dibang Valley", crime_panel$district_name)

# Uttara Kannada is an alternate spelling for Uttar Kannada.
crime_panel$district_name <- if_else(crime_panel$district_name=="Uttara Kannada", "Uttar Kannada", crime_panel$district_name)

# Vadodara City is part of Vadodara.
crime_panel$district_name <- if_else(crime_panel$district_name=="Vadodara City", "Vadodara", crime_panel$district_name)

# Vadodara Rural is part of Vadodara.
crime_panel$district_name <- if_else(crime_panel$district_name=="Vadodara Rural", "Vadodara", crime_panel$district_name)

# Vijayapura is an alternate name for Bijapur.
crime_panel$district_name <- if_else(crime_panel$district_name=="Vijayapura", "Bijapur", crime_panel$district_name)

# Vijayawada City is part of Krishna.
crime_panel$district_name <- if_else(crime_panel$district_name=="Vijayawada City", "Krishna", crime_panel$district_name)

# Vijayawada is part of Krishna.
crime_panel$district_name <- if_else(crime_panel$district_name=="Vijayawada", "Krishna", crime_panel$district_name)

# Viluppuram is an alternate spelling for Villupuram.
crime_panel$district_name <- if_else(crime_panel$district_name=="Viluppuram", "Villupuram", crime_panel$district_name)

# Visakha Rural is part of Vishakhapatnam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Visakha Rural", "Vishakhapatnam", crime_panel$district_name)

# Vishakhapatnam is an alternate spelling for Visakhapatnam.
crime_panel$district_name <- if_else(crime_panel$district_name=="Vishakhapatnam", "Visakhapatnam", crime_panel$district_name)

# Warangal Urban is part of Warangal.
crime_panel$district_name <- if_else(crime_panel$district_name=="Warangal Urban", "Warangal", crime_panel$district_name)

# Workha is an alternate spelling for Wokha.
crime_panel$district_name <- if_else(crime_panel$district_name=="Workha", "Wokha", crime_panel$district_name)

# Pre-2010, Yadgiri was part of Gulbarga.
crime_panel$district_name <- if_else(crime_panel$district_name=="Yadgiri", "Gulbarga", crime_panel$district_name)

## Rename some states to account for the creation of 
## new states and alternate state spellings.

# Rename Odisha to Orissa.
crime_panel$state_name <- if_else(crime_panel$state_name=="Odisha", "Orissa", crime_panel$state_name)

# Rename Jammu Kashmir to Jammu & Kashmir.
crime_panel$state_name <- if_else(crime_panel$state_name=="Jammu Kashmir", "Jammu & Kashmir", crime_panel$state_name)

# Change Telangana districts to Andhra Pradesh districts.
crime_panel$state_name <- if_else(crime_panel$state_name=="Telangana", "Andhra Pradesh", crime_panel$state_name)

crime_panel$state_name <- if_else(crime_panel$district_name %in% c("Bastar",
                                                                   "Bilaspur",
                                                                   "Dantewara",
                                                                   "Dhamtari",
                                                                   "Durg",
                                                                   "Janjgir",
                                                                   "Jashpur",
                                                                   "Kanker",
                                                                   "Kawardha",
                                                                   "Korba",
                                                                   "Koriya",
                                                                   "Mahasamund",
                                                                   "Raigarh",
                                                                   "Raipur",
                                                                   "Rajnandgaon",
                                                                   "Sarguja") & 
                                    crime_panel$year<=2000,
                                  "Chhattisgarh",
                                  crime_panel$state_name)

crime_panel$state_name <- if_else(crime_panel$district_name %in% c("Almora",
                                                                   "Bageshwar",
                                                                   "Chamoli",
                                                                   "Champawat",
                                                                   "Dehradun",
                                                                   "Haridwar",
                                                                   "Nainital",
                                                                   "Pithoragarh",
                                                                   "Pauri Garhwal",
                                                                   "Rudra Prayag",
                                                                   "Tehri Garhwal",
                                                                   "Udhamsingh Nagar",
                                                                   "Uttarkashi") & 
                                    crime_panel$year<=2000,
                                  "Uttarakhand",
                                  crime_panel$state_name)

crime_panel$state_name <- if_else(crime_panel$district_name %in% c("Bokaro",
                                                                   "Chatra",
                                                                   "Deoghar",
                                                                   "Dhanbad",
                                                                   "Dumka",
                                                                   "Garhwa",
                                                                   "Giridih",
                                                                   "Godda",
                                                                   "Gumla",
                                                                   "Hazaribagh",
                                                                   "Koderma",
                                                                   "Lohardagga",
                                                                   "Pakur",
                                                                   "Palamu",
                                                                   "Pashchimi Singhbhum",
                                                                   "Purbi Singhbhum",
                                                                   "Ranchi",
                                                                   "Sahebganj") & 
                                    crime_panel$year<=2000,
                                  "Jharkhand",
                                  crime_panel$state_name)

crime_panel$state_name <- if_else(crime_panel$state_name=="Delhi", "Delhi Ut", crime_panel$state_name)

crime_panel$state_name <- if_else(crime_panel$state_name=="Sikkam", "Sikkim", crime_panel$state_name)

## Group data by state, district, and year.
crime_panel <- group_by(crime_panel,
                        state_name,
                        district_name,
                        year)

## Create a balanced district panel after accounting for
## district and state corrections.
crime_panel <- summarize(crime_panel,
                         riots=sum(riots, na.rm = TRUE),
                         murders=sum(murders, na.rm = TRUE))

## Ungroup data.
crime_panel <- ungroup(crime_panel)

## Load district crosswalk to add 2001 district codes.
cw <- read_dta("district_crosswalk.dta")

## Rename variables for merging purposes.
cw <- rename(cw,
             state_name=state,
             district_name=district)

## Change format of state and district names to title case.
cw <- mutate(cw,
             state_name=str_to_title(state_name),
             district_name=str_to_title(district_name))

## Merge district crosswalk to add 2001 district codes.
crime_panel <- left_join(crime_panel, cw)

## Remove district crosswalk.
rm(cw)

## Arrange data by state, district, and year.
crime_panel <- arrange(crime_panel,
                       state_name,
                       district_name,
                       year)

## Reorder variables.
crime_panel <- select(crime_panel,
                      id,
                      state_name,
                      district_name,
                      everything())

## Delhi National Capital Region is composed of districts
## in Delhi and in surrounding states. Assign correct 
## state identifier to districts in the Delhi NCR.
crime_panel$state_name <- if_else(crime_panel$district_name %in% c("Gautambudh Nagar",
                                                                   "Meerut",
                                                                   "Ghaziabad",
                                                                   "Bulandshahar",
                                                                   "Baghpat",
                                                                   "Gurgaon",
                                                                   "Faridabad",
                                                                   "Rohtak",
                                                                   "Sonipat",
                                                                   "Rewari",
                                                                   "Panipat",
                                                                   "Alwar"),
                                  "Delhi Ut", crime_panel$state_name)

## Group by district.
crime_panel <- group_by(crime_panel, id)

## Create a measure of total rioting crimes during sample period.
crime_panel <- mutate(crime_panel,
                      total_riots=sum(riots, na.rm=TRUE))

## Ungroup data.
crime_panel <- ungroup(crime_panel)

## These commands create a district-level panel of textile investment
## to merge with the district panel of rioting.

## Load data on private sector investment projects.
capex <- read_csv("capex_investment.csv")

## Select and rename variables.
capex <- select(capex,
                year,
                pin_code=pincode,
                industry_code,
                industry,
                ownership,
                type_of_unit,
                deflated_amt=deflated.amt)

## Remove projects that do not have PIN codes for matching to districts.
## This results in the dropping of 52 textile projects.
capex <- drop_na(capex, pin_code)

## Load India PIN code-district crosswalk.
pin <- read_csv("pin_district_crosswalk.csv")

## Select and rename variables.
pin <- select(pin,
              pin_code=Pincode,
              district_name=District,
              state_name=StateName)

## Change format of district names to title case.
pin <- mutate(pin, district_name=str_to_title(district_name))

## Arrange observations by PIN code.
pin <- arrange(pin, pin_code)

## Remove duplicated PIN codes.
pin$duplicated <- duplicated(pin$pin_code)

pin <- filter(pin, duplicated==FALSE)

pin <- select(pin, -duplicated)

## Merge district crosswalk to add 2001 district codes.
capex <- left_join(capex, pin)

## Remove PIN code-district crosswalk.
rm(pin)

## Select and reorder variables.
capex <- select(capex,
                district_name,
                state_name,
                year,
                industry,
                ownership,
                type_of_unit,
                deflated_amt)

## Create vector of textile-related industries.
textile_industries <- c("Cloth",
                        "Cotton & blended yarn",
                        "Diversified cotton textile",
                        "Other textiles",
                        "Readymade garments",
                        "Textile processing")

## Keep only investments in textile related-industries.
capex_text <- filter(capex, industry %in% textile_industries)

## Remove origincal CapEx data and vector of textile-related industries.
rm(capex,
   textile_industries)

## Drop observations that did not have a matching PIN code.
## This results in 26 dropped textile-related investments.
capex_text <- drop_na(capex_text, district_name)

## Group data by state, district, and year.
capex_text <- group_by(capex_text,
                       district_name,
                       state_name,
                       year)

## Create a district panel.
capex_text <- summarize(capex_text,
                        textile_projects=n(),
                        textile_value=sum(deflated_amt, na.rm = TRUE))

# Ungroup data.
capex_text <- ungroup(capex_text)

# Arrange data by state, district, and year.
capex_text <- arrange(capex_text,
                      state_name,
                      district_name,
                      year)

## Remove observations from districts in small non-state union territories.
capex_text <- filter(capex_text, !district_name %in% c("Dadra & Nagar Haveli",
                                                       "Dadra  Nagar Haveli",
                                                       "Daman"))

## Keep only observations between 1999 and 2010.
capex_text <- filter(capex_text, year>=1999 & year<=2010)

## Match all district observations to 2001 district boundaries.

## Because new administrative districts are created during the time period,
## I must match new administrative districts to their 2001 boundaries.

## Additionally, I correct minor spelling differences in district names across 
## years to ensure consistency and correct merging. Below, I explain each change.

# Ahmedabad is an alternate spelling for Ahmadabad.
capex_text$district_name <- if_else(capex_text$district_name=="Ahmedabad", "Ahmadabad", capex_text$district_name)

# Bathinda is an alternate spelling for Bhatinda.
capex_text$district_name <- if_else(capex_text$district_name=="Bathinda", "Bhatinda", capex_text$district_name)

# Bengaluru is an alternate spelling for Bangalore Commr.
capex_text$district_name <- if_else(capex_text$district_name=="Bengaluru", "Bangalore Commr.", capex_text$district_name)

# Bengaluru Rural is an alternate spelling for Bangalore Rural.
capex_text$district_name <- if_else(capex_text$district_name=="Bengaluru Rural", "Bangalore Rural", capex_text$district_name)

# Bilaspur Hp is the Bilaspur in Himachal Pradesh.
capex_text$district_name <- if_else(capex_text$district_name=="Bilaspur Hp", "Bilaspur", capex_text$district_name)

# Central Delhi is Central.
capex_text$district_name <- if_else(capex_text$district_name=="Central Delhi", "Central", capex_text$district_name)

# Chandighar is an alternate spelling for Chandigarh.
capex_text$district_name <- if_else(capex_text$district_name=="Chandighar", "Chandigarh", capex_text$district_name)

# East Delhi is East.
capex_text$district_name <- if_else(capex_text$district_name=="East Delhi", "East", capex_text$district_name)

# Gandhi Nagar is an alternate spelling for Gandhinagar.
capex_text$district_name <- if_else(capex_text$district_name=="Gandhi Nagar", "Gandhinagar", capex_text$district_name)

# Gautam Buddha Nagar	is an alternate spelling for Gautambudh Nagar.
capex_text$district_name <- if_else(capex_text$district_name=="Gautam Buddha Nagar", "Gautambudh Nagar", capex_text$district_name)

# Hyderabad is Hyderabad City.
capex_text$district_name <- if_else(capex_text$district_name=="Hyderabad", "Hyderabad City", capex_text$district_name)

# Kachchh is an alternate spelling for Kutch.
capex_text$district_name <- if_else(capex_text$district_name=="Kachchh", "Kutch", capex_text$district_name)

# Mumbai Subueban is in Mumbai.
capex_text$district_name <- if_else(capex_text$district_name=="Mumbai Subueban", "Mumbai", capex_text$district_name)

# North 24 Parganas	is an alternate spelling for 24 Parganas North.
capex_text$district_name <- if_else(capex_text$district_name=="North 24 Parganas", "24 Parganas North", capex_text$district_name)

# North Delhi is North.
capex_text$district_name <- if_else(capex_text$district_name=="North Delhi", "North", capex_text$district_name)

# North West Delhi is North-West.
capex_text$district_name <- if_else(capex_text$district_name=="North West Delhi", "North-West", capex_text$district_name)

# Pre-2014, Palghar was part of Thane.
capex_text$district_name <- if_else(capex_text$district_name=="Palghar", "Thane", capex_text$district_name)

# Pre-2008, Ramanagar was part of Bangalore Rural.
capex_text$district_name <- if_else(capex_text$district_name=="Ramanagar", "Bangalore Rural", capex_text$district_name)

# Rangareddy is an alternate spelling for Ranga Reddy.
capex_text$district_name <- if_else(capex_text$district_name=="Rangareddy", "Ranga Reddy", capex_text$district_name)

# Pre-2014, Sangareddy was part of Medak.
capex_text$district_name <- if_else(capex_text$district_name=="Sangareddy", "Medak", capex_text$district_name)

# Pre-2012, Shahdara was part of North-East.
capex_text$district_name <- if_else(capex_text$district_name=="Shahdara", "North-East", capex_text$district_name)

# South Delhi is part of South.
capex_text$district_name <- if_else(capex_text$district_name=="South Delhi", "South", capex_text$district_name)

# South West Delhi is South-West.
capex_text$district_name <- if_else(capex_text$district_name=="South West Delhi", "South-West", capex_text$district_name)

# Tuticorin is an alternate spelling for Thoothugudi.
capex_text$district_name <- if_else(capex_text$district_name=="Tuticorin", "Thoothugudi", capex_text$district_name)

# West Delhi is West.
capex_text$district_name <- if_else(capex_text$district_name=="West Delhi", "West", capex_text$district_name)

## Rename some states to account for the creation of 
## new states and alternate state spellings.

# Rename Delhi to Delhi Ut.
capex_text$state_name <- if_else(capex_text$state_name=="Delhi", "Delhi Ut", capex_text$state_name)

# Rename Telangana districts to Andhra Pradesh districts for merging purposes.
capex_text$state_name <- if_else(capex_text$state_name=="Telangana", "Andhra Pradesh", capex_text$state_name)

# Chandigarh is its own state in the crime data.
capex_text$state_name <- if_else(capex_text$district_name=="Chandigarh", "Chandigarh", capex_text$state_name)

## Group data by state, district, and year.
capex_text <- group_by(capex_text,
                       state_name,
                       district_name,
                       year)

## Create a district panel after accounting for
## district and state corrections.
capex_text <- summarize(capex_text,
                        textile_projects=sum(textile_projects, na.rm = TRUE),
                        textile_value=sum(textile_value, na.rm = TRUE))

## Ungroup data.
capex_text <- ungroup(capex_text)

## Load district crosswalk to add 2001 district codes.
cw <- read_dta("district_crosswalk.dta")

## Rename variables for merging purposes.
cw <- rename(cw,
             state_name=state,
             district_name=district)

## Change format of state and district names to title case.
cw <- mutate(cw,
             state_name=str_to_title(state_name),
             district_name=str_to_title(district_name))

## Merge district crosswalk to add 2001 district codes.
capex_text <- left_join(capex_text, cw)

## Remove district crosswalk.
rm(cw)

## Arrange data by state, district, and year.
capex_text <- arrange(capex_text,
                      state_name,
                      district_name,
                      year)

## Reorder variables.
capex_text <- select(capex_text,
                     id,
                     state_name,
                     district_name,
                     everything())

## Delhi National Capital Region is composed of districts
## in Delhi and in surrounding states. Assign correct 
## state identifier to districts in the Delhi NCR.
capex_text$state_name <- if_else(capex_text$district_name %in% c("Gautambudh Nagar",
                                                                 "Meerut",
                                                                 "Ghaziabad",
                                                                 "Bulandshahar",
                                                                 "Baghpat",
                                                                 "Gurgaon",
                                                                 "Faridabad",
                                                                 "Rohtak",
                                                                 "Sonipat",
                                                                 "Rewari",
                                                                 "Panipat",
                                                                 "Alwar"),
                                 "Delhi Ut", capex_text$state_name)

## Merge crime and textile investment data.
crime_panel <- left_join(crime_panel, capex_text)

## Remove textile investment data frame.
rm(capex_text)

## Replace missing textile investment projects with zeros to reflect no investment.
crime_panel$textile_projects[is.na(crime_panel$textile_projects)] <- 0

## Replace missing textile investment value with zeros to reflect no investment.
crime_panel$textile_value[is.na(crime_panel$textile_value)] <- 0

## Account for the fact that data on investment values is missing for some districts.
crime_panel$textile_value <- ifelse(crime_panel$textile_projects>0 & crime_panel$textile_value==0,
                                    NA,
                                    crime_panel$textile_value)

## These commands create a district-level panel of textile employment
## to merge with the district panel of rioting.

## Load individual-level data from the 61st National Sample Survey.
nss <- read_dta("Employment_by_NIC4_Nss61_Sch10_w_id.dta")

## Limit data only to individuals employed in textile-related industries.
nss <- filter(nss, nic4_code98_ >=1700 & nic4_code98_ <1900)

## Group data by district.
nss <- group_by(nss, id)

## Create a district panel of individuals employed in 
## textile-related industries.
nss <- summarize(nss,
                 n_text=sum(total_n, na.rm = TRUE))

## Ungroup data.
nss <- ungroup(nss)

## Calculate each district's share of textile-related employment.
nss <- mutate(nss, text_share=(n_text/sum(n_text))*100)

## Load district crosswalk to add 2001 district codes.
cw <- read_dta("district_crosswalk.dta")

## Merge district crosswalk to add 2001 district codes.
nss <- full_join(nss, cw)

## Remove district crosswalk.
rm(cw)

## Replace missing values with zeros to reflect no textile employment.
nss$text_share[is.na(nss$text_share)] <- 0

## Rename and reorder variables.
nss <- select(nss,
              id,
              state_name=state,
              district_name=district,
              text_share)

## Delhi National Capital Region is composed of districts
## in Delhi and in surrounding states. Assign correct 
## state identifier to districts in the Delhi NCR.
nss$state_name <- if_else(nss$id==799, "DELHI UT", nss$state_name)

nss$state_name <- if_else(nss$district_name %in% c("GAUTAMBUDH NAGAR",
                                                   "MEERUT",
                                                   "GHAZIABAD",
                                                   "BULANDSHAHAR",
                                                   "BAGHPAT",
                                                   "GURGAON",
                                                   "FARIDABAD",
                                                   "ROHTAK",
                                                   "SONIPAT",
                                                   "REWARI",
                                                   "PANIPAT",
                                                   "ALWAR"),
                          "DELHI UT", nss$state_name)

## Change format of state and district names to title case.
nss <- mutate(nss,
              state_name=str_to_title(state_name),
              district_name=str_to_title(district_name))

## Merge crime and textile employment data.
crime_panel <- left_join(crime_panel, nss)

## Create a post-MFA expiration indicator variable.
crime_panel$post <- if_else(crime_panel$year>=2005, 1, 0)

## Create the primary measure of exposure to the post-MFA textile shock.
crime_panel <- mutate(crime_panel, treatedXpost=text_share*post)

## These commands create a district-level cross-section of pre-treatment
## control variables to merge with the district panel of rioting.

## Load district-level control variables.
controls <- read_csv("district_controls.csv")

## Merge crime and control variable data.
crime_panel <- left_join(crime_panel, controls)

## Create measures of riots and murders per capita.
crime_panel <- mutate(crime_panel,
                      riots_pop=riots/(tot_p/1000),
                      murders_pop=murders/(tot_p/1000))

## Create a series of year indicator variables.
crime_panel <- dummy_cols(crime_panel, select_columns = "year")

## Create variables of logged population interacted with
## year indicator variables.
crime_panel <- mutate(crime_panel,
                      popX2000=log(tot_p)*year_2000,
                      popX2001=log(tot_p)*year_2001,
                      popX2002=log(tot_p)*year_2002,
                      popX2003=log(tot_p)*year_2003,
                      popX2004=log(tot_p)*year_2004,
                      popX2005=log(tot_p)*year_2005,
                      popX2006=log(tot_p)*year_2006,
                      popX2007=log(tot_p)*year_2007,
                      popX2008=log(tot_p)*year_2008,
                      popX2009=log(tot_p)*year_2009,
                      popX2010=log(tot_p)*year_2010)

## Create variables of employment interacted with
## year indicator variables.
crime_panel <- mutate(crime_panel,
                      workX2000=percent_work*year_2000,
                      workX2001=percent_work*year_2001,
                      workX2002=percent_work*year_2002,
                      workX2003=percent_work*year_2003,
                      workX2004=percent_work*year_2004,
                      workX2005=percent_work*year_2005,
                      workX2006=percent_work*year_2006,
                      workX2007=percent_work*year_2007,
                      workX2008=percent_work*year_2008,
                      workX2009=percent_work*year_2009,
                      workX2010=percent_work*year_2010)

## Create variables of literacy interacted with
## year indicator variables.
crime_panel <- mutate(crime_panel,
                      litX2000=percent_lit*year_2000,
                      litX2001=percent_lit*year_2001,
                      litX2002=percent_lit*year_2002,
                      litX2003=percent_lit*year_2003,
                      litX2004=percent_lit*year_2004,
                      litX2005=percent_lit*year_2005,
                      litX2006=percent_lit*year_2006,
                      litX2007=percent_lit*year_2007,
                      litX2008=percent_lit*year_2008,
                      litX2009=percent_lit*year_2009,
                      litX2010=percent_lit*year_2010)

## Create variables of Scheduled Caste rate interacted with
## year indicator variables.
crime_panel <- mutate(crime_panel,
                      scX2000=percent_sc*year_2000,
                      scX2001=percent_sc*year_2001,
                      scX2002=percent_sc*year_2002,
                      scX2003=percent_sc*year_2003,
                      scX2004=percent_sc*year_2004,
                      scX2005=percent_sc*year_2005,
                      scX2006=percent_sc*year_2006,
                      scX2007=percent_sc*year_2007,
                      scX2008=percent_sc*year_2008,
                      scX2009=percent_sc*year_2009,
                      scX2010=percent_sc*year_2010)

## Create a year-by-year treatment indicator.
crime_panel <-mutate(crime_panel,
                     textX1999=text_share*year_1999,
                     textX2000=text_share*year_2000,
                     textX2001=text_share*year_2001,
                     textX2002=text_share*year_2002,
                     textX2003=text_share*year_2003,
                     textX2004=text_share*year_2004,
                     textX2005=text_share*year_2005,
                     textX2006=text_share*year_2006,
                     textX2007=text_share*year_2007,
                     textX2008=text_share*year_2008,
                     textX2009=text_share*year_2009,
                     textX2010=text_share*year_2010)


###################################
## District-level migration data ##
###################################

## These commands create a district-level panel of total internal migration
## to save for analysis and merge with the district panel of rioting.

###################
# Total migration #
###################

## Load data from 2011 census on district-level migration.
migration <- read_csv("district_migration.csv")

## Keep observations of total migration regardless of gender.
migration <- filter(migration, migration_type=="total")

## Keep observations of migration in past ten years.
migration <- filter(migration, migration_length %in% c("oneyr",
                                                       "onefouryr",
                                                       "fivenineyr"))

## Create a time period (pre/post-MFA expiration) variable.
migration$period <- if_else(migration$migration_length=="fivenineyr", 1, migration$period)

migration$period <- if_else(migration$migration_length=="onefouryr", 2, migration$period)

migration$period <- if_else(migration$migration_length=="oneyr", 2, migration$period)

## Select and reorder variables.
migration <- select(migration,
                    id,
                    period,
                    state_name,
                    district_name,
                    beyond_state,
                    within_state)

## Arrange data by district and time period.
migration <- arrange(migration,
                     id,
                     period)

## Correct some district identifiers for merging.

# Siang districts
migration$id <- if_else(migration$id %in% c(1208, 1209), 120809, migration$id)

migration$district_name <- if_else(migration$id==120809, "Siang Upper", migration$district_name)

# Imphal districts
migration$id <- if_else(migration$id %in% c(1406, 1407), 140607, migration$id)

migration$district_name <- if_else(migration$id==140607, "Imphal", migration$district_name)

# Mumbai districts
migration$id <- if_else(migration$id %in% c(2722, 2723), 272223, migration$id)

migration$district_name <- if_else(migration$id==272223, "Mumbai", migration$district_name)

# Perambalur districts
migration$id <- if_else(migration$id %in% c(3316, 3317), 331617, migration$id)

migration$district_name <- if_else(migration$id==331617, "Perambalur", migration$district_name)

# Andaman and Nicobar Island districts
migration$id <- if_else(migration$id %in% c(3501, 3502), 350102, migration$id)

migration$district_name <- if_else(migration$id==350102, "A&N Islands", migration$district_name)

## Remove districts without district identifiers.
migration <- drop_na(migration, id)

## Group data by state, district, and time period.
migration <- group_by(migration,
                      id,
                      state_name,
                      district_name,
                      period)

## Create balanced panel of internal migration.
migration <- summarize_all(migration, sum)

## Ungroup data.
migration <- ungroup(migration)

## Merge internal migration and control variable data.
migration <- left_join(migration, controls)

migration <- mutate(migration, within_rate=(within_state/tot_p)*100)

migration <- mutate(migration, beyond_rate=(beyond_state/tot_p)*100)

## Create a post-MFA expiration indicator variable.
migration$post <- if_else(migration$period==2, 1, 0)

## Create version of internal migration data to merge with crime data.
migration_merge <- select(migration,
                          id,
                          post,
                          beyond_rate)

## Merge crime and internal migration data.
crime_panel <- left_join(crime_panel,
                         migration_merge)

## Remove mergeable internal migration data.
rm(migration_merge)

## Create version of textile employment data to merge with
## internal migration data.
nss <- select(nss,
              id,
              text_share)

## Merge internal migration and textile employment data.
migration <- left_join(migration, nss)

## Create the primary measure of exposure to the post-MFA textile shock.
migration <- mutate(migration, treatedXpost=text_share*post)

## Select and reorder variables.
migration <- select(migration,
                    id,
                    state_name,
                    district_name,
                    period,
                    text_share,
                    post,
                    treatedXpost,
                    within_rate,
                    beyond_rate,
                    tot_p:percent_work)

## Remove Daman, an industrial zone in a non-state union territory with virtually no local population.
migration <- filter(migration, district_name!="Daman")

## Save Analysis Dataset 2:
## District panel of total migration
write_csv(migration, "analysis_dataset_2.csv")

## Remove total migration data.
rm(migration)

##################
# Male migration #
##################

## These commands create a district-level panel of male internal migration
## to save for analysis and merge with the district panel of rioting.

## Load data from 2011 census on district-level migration.
migration <- read_csv("district_migration.csv")

## Keep observations of total migration regardless of gender.
migration <- filter(migration, migration_type=="males")

## Keep observations of migration in past ten years.
migration <- filter(migration, migration_length %in% c("oneyr",
                                                       "onefouryr",
                                                       "fivenineyr"))

## Create a time period (pre/post-MFA expiration) variable.
migration$period <- if_else(migration$migration_length=="fivenineyr", 1, migration$period)

migration$period <- if_else(migration$migration_length=="onefouryr", 2, migration$period)

migration$period <- if_else(migration$migration_length=="oneyr", 2, migration$period)

## Select and reorder variables.
migration <- select(migration,
                    id,
                    period,
                    state_name,
                    district_name,
                    beyond_state,
                    within_state)

## Arrange data by district and time period.
migration <- arrange(migration,
                     id,
                     period)

## Correct some district identifiers for merging.

# Siang districts
migration$id <- if_else(migration$id %in% c(1208, 1209), 120809, migration$id)

migration$district_name <- if_else(migration$id==120809, "Siang Upper", migration$district_name)

# Imphal districts
migration$id <- if_else(migration$id %in% c(1406, 1407), 140607, migration$id)

migration$district_name <- if_else(migration$id==140607, "Imphal", migration$district_name)

# Mumbai districts
migration$id <- if_else(migration$id %in% c(2722, 2723), 272223, migration$id)

migration$district_name <- if_else(migration$id==272223, "Mumbai", migration$district_name)

# Perambalur districts
migration$id <- if_else(migration$id %in% c(3316, 3317), 331617, migration$id)

migration$district_name <- if_else(migration$id==331617, "Perambalur", migration$district_name)

# Andaman and Nicobar Island districts
migration$id <- if_else(migration$id %in% c(3501, 3502), 350102, migration$id)

migration$district_name <- if_else(migration$id==350102, "A&N Islands", migration$district_name)

## Remove districts without district identifiers.
migration <- drop_na(migration, id)

## Group data by state, district, and time period.
migration <- group_by(migration,
                      id,
                      state_name,
                      district_name,
                      period)

## Create balanced panel of internal migration.
migration <- summarize_all(migration, sum)

## Ungroup data.
migration <- ungroup(migration)

## Merge internal migration and control variable data.
migration <- left_join(migration, controls)

migration <- mutate(migration, within_rate=(within_state/tot_p)*100)

migration <- mutate(migration, beyond_rate=(beyond_state/tot_p)*100)

## Create a post-MFA expiration indicator variable.
migration$post <- if_else(migration$period==2, 1, 0)

## Create version of textile employment data to merge with
## internal migration data.
nss <- select(nss,
              id,
              text_share)

## Merge internal migration and textile employment data.
migration <- left_join(migration, nss)

## Create the primary measure of exposure to the post-MFA textile shock.
migration <- mutate(migration, treatedXpost=text_share*post)

## Select and reorder variables.
migration <- select(migration,
                    id,
                    state_name,
                    district_name,
                    period,
                    text_share,
                    post,
                    treatedXpost,
                    within_rate,
                    beyond_rate,
                    tot_p:percent_work)

## Remove Daman, an industrial zone in a non-state union territory with virtually no local population.
migration <- filter(migration, district_name!="Daman")

## Save Analysis Dataset 3:
## District panel of male migration
write_csv(migration, "analysis_dataset_3.csv")

## Remove male migration data.
rm(migration)

####################
# Female migration #
####################

## These commands create a district-level panel of female internal migration
## to save for analysis and merge with the district panel of rioting.

## Load data from 2011 census on district-level migration.
migration <- read_csv("district_migration.csv")

## Keep observations of total migration regardless of gender.
migration <- filter(migration, migration_type=="females")

## Keep observations of migration in past ten years.
migration <- filter(migration, migration_length %in% c("oneyr",
                                                       "onefouryr",
                                                       "fivenineyr"))

## Create a time period (pre/post-MFA expiration) variable.
migration$period <- if_else(migration$migration_length=="fivenineyr", 1, migration$period)

migration$period <- if_else(migration$migration_length=="onefouryr", 2, migration$period)

migration$period <- if_else(migration$migration_length=="oneyr", 2, migration$period)

## Select and reorder variables.
migration <- select(migration,
                    id,
                    period,
                    state_name,
                    district_name,
                    beyond_state,
                    within_state)

## Arrange data by district and time period.
migration <- arrange(migration,
                     id,
                     period)

## Correct some district identifiers for merging.

# Siang districts
migration$id <- if_else(migration$id %in% c(1208, 1209), 120809, migration$id)

migration$district_name <- if_else(migration$id==120809, "Siang Upper", migration$district_name)

# Imphal districts
migration$id <- if_else(migration$id %in% c(1406, 1407), 140607, migration$id)

migration$district_name <- if_else(migration$id==140607, "Imphal", migration$district_name)

# Mumbai districts
migration$id <- if_else(migration$id %in% c(2722, 2723), 272223, migration$id)

migration$district_name <- if_else(migration$id==272223, "Mumbai", migration$district_name)

# Perambalur districts
migration$id <- if_else(migration$id %in% c(3316, 3317), 331617, migration$id)

migration$district_name <- if_else(migration$id==331617, "Perambalur", migration$district_name)

# Andaman and Nicobar Island districts
migration$id <- if_else(migration$id %in% c(3501, 3502), 350102, migration$id)

migration$district_name <- if_else(migration$id==350102, "A&N Islands", migration$district_name)

## Remove districts without district identifiers.
migration <- drop_na(migration, id)

## Group data by state, district, and time period.
migration <- group_by(migration,
                      id,
                      state_name,
                      district_name,
                      period)

## Create balanced panel of internal migration.
migration <- summarize_all(migration, sum)

## Ungroup data.
migration <- ungroup(migration)

## Merge internal migration and control variable data.
migration <- left_join(migration, controls)

migration <- mutate(migration, within_rate=(within_state/tot_p)*100)

migration <- mutate(migration, beyond_rate=(beyond_state/tot_p)*100)

## Create a post-MFA expiration indicator variable.
migration$post <- if_else(migration$period==2, 1, 0)

## Create version of textile employment data to merge with
## internal migration data.
nss <- select(nss,
              id,
              text_share)

## Merge internal migration and textile employment data.
migration <- left_join(migration, nss)

## Create the primary measure of exposure to the post-MFA textile shock.
migration <- mutate(migration, treatedXpost=text_share*post)

## Select and reorder variables.
migration <- select(migration,
                    id,
                    state_name,
                    district_name,
                    period,
                    text_share,
                    post,
                    treatedXpost,
                    within_rate,
                    beyond_rate,
                    tot_p:percent_work)

## Remove Daman, an industrial zone in a non-state union territory with virtually no local population.
migration <- filter(migration, district_name!="Daman")

## Save Analysis Dataset 4:
## District panel of female migration
write_csv(migration, "analysis_dataset_4.csv")

## Remove female migration data.
rm(migration)

## Remove textile employment and control variable data.
rm(nss)

## Create triple interaction terms for triple difference analysis.
crime_panel <- mutate(crime_panel,
                      treatedXpostXmigration=text_share*post*beyond_rate,
                      treatedXmigration=text_share*beyond_rate,
                      postXmigration=post*beyond_rate)

###################################################
## District-level investment in other industries ##
###################################################

## These commands create a district-level panel of investment in other industries.
private <- read_csv("capex_investment_otherindustries.csv")

## Select and rename variables.
private <- select(private,
                  id,
                  year,
                  machinery_projects=machinery,
                  automobiles_projects=automobiles,
                  metals_projects=metals,
                  chemicals_projects=chemicals,
                  pharma_projects=pharma,
                  food_projects=food,
                  furnleathrub_projects=furnleathrub)

## Merge crime and private investment data.
crime_panel <- left_join(crime_panel, private)

## Remove private investment data.
rm(private)

## Replace missing values with zeros to reflect no investment.
crime_panel$machinery_projects[is.na(crime_panel$machinery_projects)] <- 0

crime_panel$automobiles_projects[is.na(crime_panel$automobiles_projects)] <- 0

crime_panel$metals_projects[is.na(crime_panel$metals_projects)] <- 0

crime_panel$chemicals_projects[is.na(crime_panel$chemicals_projects)] <- 0

crime_panel$pharma_projects[is.na(crime_panel$pharma_projects)] <- 0

crime_panel$food_projects[is.na(crime_panel$food_projects)] <- 0

crime_panel$furnleathrub_projects[is.na(crime_panel$furnleathrub_projects)] <- 0

## Before removing observations of Rajasthan, save textile concentration and
## investment dataset including Rajasthan for mapping purposes.

## Select variables.
investment <- select(crime_panel,
                     state_name,
                     district_name,
                     id,
                     year,
                     textile_projects,
                     text_share)

## Save Analysis Dataset 5:
## District panel of textile concentration and investment, including Rajasthan
write_csv(investment, "analysis_dataset_5.csv")

## Remove observations of Rajasthan from the data.
crime_panel <- filter(crime_panel,
                      state_name!="Rajasthan")

## Remove observations without district identifiers.
crime_panel <- drop_na(crime_panel,
                       id)

## Load data on district linguistic fractionalization.
district_frac <- read_csv("district_lang_frac.csv")

## Convert district identifier to numeric.
district_frac <- mutate(district_frac,
                        id=as.numeric(id))

## Correct some district identifiers for merging.

# Mumbai districts
district_frac$id <- if_else(district_frac$id %in% c(2722, 2723), 272223, district_frac$id)

# Perambalur districts
district_frac$id <- if_else(district_frac$id %in% c(3316, 3317), 331617, district_frac$id)

# Siang districts
district_frac$id <- if_else(district_frac$id %in% c(1208, 1209), 120809, district_frac$id)

# Imphal districts
district_frac$id <- if_else(district_frac$id %in% c(1406, 1407), 140607, district_frac$id)

## Group data by district.
district_frac <- group_by(district_frac,
                          id)

## Calculate district-level probability of two speakers
## speaking the same mother tongue.
district_frac <- summarize(district_frac,
                           prob_samelang=mean(prob_samelang))

## Ungroup data.
district_frac <- ungroup(district_frac)

## Merge district fractionalization data.
crime_panel <- left_join(crime_panel,
                         district_frac)

## Remove district fractionalization data.
rm(district_frac)

## Calculate district-level probability of two speakers
## speaking a different mother tongue.
crime_panel <- mutate(crime_panel,
                      prob_difflang=1-prob_samelang)

## Remove original probability of same language.
crime_panel <- select(crime_panel,
                      -prob_samelang)

## Create triple interaction terms for triple difference analysis.
crime_panel <- mutate(crime_panel,
                      treatedXpostXdifflang=text_share*post*prob_difflang,
                      treatedXdifflang=text_share*prob_difflang,
                      postXdifflang=post*prob_difflang)


## Load 2011 Census migration data.
migration <- read_csv("migration_cens11_collapsed.csv")

#### CHECK THIS ####
migration <- filter(migration, migration_type=="total")

migration <- filter(migration, ! migration_length %in% c("ten19yr",
                                                         "twenty",
                                                         "total"))

migration$period <- if_else(migration$migration_length=="fivenineyr", 1, migration$period)

migration$period <- if_else(migration$migration_length=="onefouryr", 2, migration$period)

migration$period <- if_else(migration$migration_length=="oneyr", 2, migration$period)

migration <- select(migration,
                    id,
                    period,
                    state_name,
                    district_name,
                    beyond_state,
                    a_n_islands:west_bengal)

migration <- arrange(migration,
                     id,
                     period)

## Correct some district identifiers for merging.

## Siang districts
migration$id <- if_else(migration$id %in% c(1208, 1209), 120809, migration$id)

migration$district_name <- if_else(migration$id==120809, "Siang Upper", migration$district_name)

## Imphal districts
migration$id <- if_else(migration$id %in% c(1406, 1407), 140607, migration$id)

migration$district_name <- if_else(migration$id==140607, "Imphal", migration$district_name)

## Mumbai districts
migration$id <- if_else(migration$id %in% c(2722, 2723), 272223, migration$id)

migration$district_name <- if_else(migration$id==272223, "Mumbai", migration$district_name)

## Perambalur districts
migration$id <- if_else(migration$id %in% c(3316, 3317), 331617, migration$id)

migration$district_name <- if_else(migration$id==331617, "Perambalur", migration$district_name)

## A&N Islands districts
migration$id <- if_else(migration$id %in% c(3501, 3502), 350102, migration$id)

migration$district_name <- if_else(migration$id==350102, "A&N Islands", migration$district_name)

## Remove districts without district identifiers.
migration <- drop_na(migration, id)

## Group data by district, state, and time period.
migration <- group_by(migration,
                      id,
                      state_name,
                      district_name,
                      period)

## Create balanced panel of internal migration.
migration <- summarize_all(migration, sum, na.rm=TRUE)

## Ungroup data.
migration <- ungroup(migration)

## Create district-state panel of in-migration.
migration <- gather(migration, key=state_name, value=migrants, a_n_islands:west_bengal)

## Calculate share of migrants from each state for each district.
migration <- mutate(migration,
                    migrant_share=migrants/beyond_state)

## Remove raw migrant variable.
migration <- select(migration, -beyond_state)

## Load data on linguistic difference between districts and states.
district_state <- read_csv("district_state_language.csv")

## Convert district identifier to numeric.
district_state <- mutate(district_state, id=as.numeric(id))

## Calculate probability that two speakers from a given district
## and state speak a different mother tongue.
district_state <- mutate(district_state, 
                         prob_difflang=1-prob_samelang)

## Remove probability of same language variable.
district_state <- select(district_state,
                         -prob_samelang)

## Convert state name variable to factor
district_state <- mutate(district_state,
                         state_name=as.factor(state_name))

## Reformat states for merging purposes.
district_state <- mutate(district_state,
                         state_name=fct_recode(state_name,
                                               "a_n_islands"="Andaman & Nicobar Islands",
                                               "andhra_pradesh"="Andhra Pradesh",
                                               "arunachal_pradesh"="Arunachal Pradesh",
                                               "assam"="Assam",
                                               "bihar"="Bihar",
                                               "chandigargh"="Chandigarh",
                                               "chhatisgargh"="Chhattisgarh",
                                               "dadra_nagar_haveli"="Dadra & Nagar Haveli",
                                               "daman_diu"="Daman & Diu",
                                               "goa"="Goa",
                                               "gujarat"="Gujarat",
                                               "haryana"="Haryana",
                                               "himachal_pradesh"="Himachal Pradesh",
                                               "jammu_kashmir"="Jammu & Kashmir",
                                               "jharkhand"="Jharkhand",
                                               "karnataka"="Karnataka",
                                               "kerala"="Kerala",
                                               "lakshadweep"="Lakshadweep",
                                               "madhya_pradesh"="Madhya Pradesh",
                                               "maharashtra"="Maharashtra",
                                               "manipur"="Manipur (Excl.Sub-Divisions)",
                                               "meghalaya"="Meghalaya",
                                               "mizoram"="Mizoram",
                                               "delhi"="N.c.t. Of Delhi",
                                               "nagaland"="Nagaland",
                                               "orissa"="Orissa",
                                               "pondicherry"="Pondicherry",
                                               "punjab"="Punjab",
                                               "rajasthan"="Rajasthan",
                                               "sikkim"="Sikkim",
                                               "tamil_nadu"="Tamil Nadu",
                                               "tripura"="Tripura",
                                               "uttar_pradesh"="Uttar Pradesh",
                                               "uttaranchal"="Uttaranchal",
                                               "west_bengal"="West Bengal"))

## Correct some district identifiers for merging.

## Mumbai districts
district_state$id <- if_else(district_state$id %in% c(2722, 2723), 272223, district_state$id)

## Perambalur districts
district_state$id <- if_else(district_state$id %in% c(3316, 3317), 331617, district_state$id)

## A&N Islands districts
district_state$id <- if_else(district_state$id %in% c(3501, 3502), 350102, district_state$id)

## Siang districts
district_state$id <- if_else(district_state$id %in% c(1208, 1209), 120809, district_state$id)

## Imphal districts
district_state$id <- if_else(district_state$id %in% c(1406, 1407), 140607, district_state$id)

## Group data by district and state.
district_state <- group_by(district_state,
                           id,
                           state_name)

## Calculate district-level probability that two speakers from a given district
## and state speak a different mother tongue.
district_state <- summarize(district_state,
                            prob_difflang=mean(prob_difflang))

## Ungroup data
district_state <- ungroup(district_state)

## Merge migration and linguistic difference data.
migration <- left_join(migration,
                       district_state)

## Remove linguistic difference data.
rm(district_state)

## Calculate weighted probabilities to generate summary measure below.
migration <- mutate(migration,
                    weighted_prob=migrant_share*prob_difflang)

## Group data by district and time period.
migration <- group_by(migration,
                      id,
                      period)

## Calculate probability that two speakers from a given district
## and state speak a different mother tongue, WEIGHTED by migrants from each state.
migration <- summarize(migration,
                       prob_difflang_weighted=sum(weighted_prob))

## Ungroup data.
migration <- ungroup(migration)

## Create post period identifier.
migration$post <- if_else(migration$period==2, 1, 0)

## Remove period variable.
migration <- select(migration, -period)

## Merge lingustic difference data.
crime_panel <- left_join(crime_panel,
                         migration)

## Create triple interaction terms for triple difference analysis.
crime_panel <- mutate(crime_panel,
                      treatedXpostXdiff=text_share*post*log(prob_difflang_weighted),
                      treatedXdiff=text_share*log(prob_difflang_weighted),
                      postXdiff=post*log(prob_difflang_weighted))

## Save Analysis Dataset 1.
write_csv(crime_panel, "analysis_dataset_1.csv")


##############################################################################
## Analysis Dataset 6: Constituency panel of nativist voting and covariates ##
##############################################################################

## The following commands create Analysis Dataset 5:
## Constituency panel of nativist voting and covariates.

## Load Bhavnani's Indian state elections dataset.
elections <- read.dta13("elections_bhavnani.dta")

## Select and rename variables.
elections <- select(elections,
                    state_name=st_name,
                    year,
                    ac_no,
                    ac_name,
                    ac_type,
                    party_abbrev=partyabbre,
                    votes=totvotpoll,
                    registered=electors)

## Limit data to Maharashtran elections during time period.
elections <- filter(elections, state_name=="Maharashtra" & year>=2004)

## Create indicator variables for Maharashtra Navnirman Sena candidates.
elections$mns <- if_else(elections$party_abbrev=="MNS", 1, 0)

## Create MNS vote variable.
elections <- mutate(elections,
                    mns_vote=votes*mns)

## Remove MNS indicator variable.
elections <- select(elections,
                    -mns)

## Group data by constituency, constituency type, and year.
elections <- group_by(elections,
                      ac_name,
                      ac_type,
                      year)

## Create constituency-level balanced panel of votes for MNS candidates.
elections <- summarize(elections,
                       mns_vote=sum(mns_vote, na.rm=TRUE),
                       total_vote=sum(votes, na.rm = TRUE))

## Ungroup data.
elections <- ungroup(elections)

## Create vote share variables for MNS.
elections <- mutate(elections,
                    mns_vote=(mns_vote/total_vote)*100)

## Load constituency-district crosswalk for Maharashtra.
cw <- read_csv("maharashtra_crosswalk.csv")

## Rename and select variables.
cw <- select(cw,
             ac_name=`Assembly Name`,
             district_name=`District Name`)

## Remove numbering for constituency names.
cw <- separate(cw, ac_name, into=c("junk", "ac_name"), sep = "- ")

cw <- select(cw, -junk)

## Correct constituency type identifier.
cw$ac_type <- ifelse(grepl("\\(SC\\)", cw$ac_name), "SC", NA)

cw$ac_type <- ifelse(grepl("\\(ST\\)", cw$ac_name), "ST", cw$ac_type)

cw$ac_type[is.na(cw$ac_type)] <- "GEN"

## Remove constituency type identifiers from constituency names.
cw$ac_name <- gsub("\\(SC\\)", "", cw$ac_name)

cw$ac_name <- gsub("\\(ST\\)", "", cw$ac_name)

elections$ac_name <- gsub("\\(S.T.\\)", "", elections$ac_name)

## Remove extraneous white space from constituency names.
elections <- mutate(elections, ac_name=str_trim(ac_name))

cw <- mutate(cw, ac_name=str_trim(ac_name))

## Correct a series of alternative spellings for merging purposes.
elections$ac_name <- if_else(elections$ac_name=="Ahmadpur", "Ahmedpur", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Ahmednagar North", "Ahmednagar City", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Ahmednagar South", "Ahmednagar City", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Ambernath", "Ambarnath", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Amravati", "Amrawati", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Arjuni-Morgaon", "Arjuni Morgaon", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Brahmapuri", "Bramhapuri", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Chandvad", "Chandwad", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Devlali", "Deolali", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Dhamamgaon Railway", "Dhamangaon Railway", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Dhule", "Dhule City", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Edlabad", "Muktainagar", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Ghansawangi", "Gansavangi", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Gondiya", "Gondia", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Jath", "Jat", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Jath", "Jat", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Khanapur", "Khanpur", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Khanapur Atpadi", "Khanpur", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Khed-Alandi", "Khed Alandi", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Malshiras", "Malshiran", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Mankhurd Shivaji Nagar", "Mankhurd shivajinagar", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Mumba Devi", "Mumbadevi", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Murtajapur", "Murtizapur", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Murtijapur", "Murtizapur", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Naigaum", "Naigaon", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="North Solapur", "Solapur City North", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Ovala - Majiwada", "Ovala majiwada", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Phulambri", "Pholambari", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Purandhar", "Purandar", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Sangamner", "Sangmner", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Sangole", "Sangola", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Shahade", "Shahada", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Sheogaon", "Shevgaon", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Shriwardhan", "Shrivardhan", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Sindkhed Raja", "Sindhkhed Raja", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Sindkheda", "Sindhkheda", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Sindkhedraja", "Sindhkhed Raja", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Solapur City South", "Solapur South", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="South Solapur", "Solapur South", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Tasgaon", "Tasgaon-Kavathe Mahankal", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Tasgaon - Kavathe Mahankal", "Tasgaon-Kavathe Mahankal", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Ulhas Nagar", "Ulhasnagar", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Umarga", "Omerga", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Umarkhadi", "Umarkhed", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Vadgaol Sheri", "Vadgaon Sheri", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Vikramgad", "Vekramgrth", elections$ac_name)

elections$ac_name <- if_else(elections$ac_name=="Vileparle", "Vile Parle", elections$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Aakola West", "Akola West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Andheri  West", "Andheri West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Aurangabad (Central)", "Aurangabad Central", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Aurangbad (East)", "Aurangabad East", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Aurangabad (West)", "Aurangabad West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Bhandup  West", "Bhandup West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Bhiwandi  West", "Bhiwandi West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Ghatkopar  West", "Ghatkopar West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="kagal", "Kagal", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Kalyan  West", "Kalyan West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Malad  West", "Malad West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Malegaon (Central)", "Malegaon Central", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Malegaon (Outer)", "Malegaon Outer", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Nashik (Central)", "Nashik Central", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Nashik  West", "Nashik West", cw$ac_name)

cw$ac_name <- if_else(cw$ac_name=="Vandre  West", "Vandre West", cw$ac_name)

## Remove constituency type identifier from crosswalk before merging.
cw <- select(cw, -ac_type)

## Merge constituency elections data with district crosswalk.
elections <- left_join(elections, cw)

## Correct some missing district identifiers based on external research.
elections$district_name <- if_else(elections$ac_name=="Adyar", "Bhandara", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Akola", "Akola", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Akrani", "Nandurabar", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Ambad", "Jalna", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Amboli", "Sindhudurg", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Andheri", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bandra", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bhadrawati", "Chandrapur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bhandup", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bhavani Peth", "Pune", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bhilwadi Wangi", "Sangli", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bhiwandi", "Thane", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Biloli", "Nanded", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Bopodi", "Pune", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Borgaon Manju", "Akola", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Chandur", "Amaravati", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Chausala", "Beed", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Chinchpokli", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Dabhadi", "Nashik", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Dadar", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Dahisar", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Darwha", "Yavatmal", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Devgad", "Sindhudurg", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Gadhinglaj", "Kolhapur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Ghatkopar", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Haveli", "Pune", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Her", "Latur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Jalamb", "Buldhana", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Jalgaon", "Jalgaon", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Jaoli", "Satara", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Jawhar", "Thane", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kalamb", "Osmanabad", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kalmeshwar", "Nagpur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kalvan", "Raigad", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kalyan", "Thane", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kamptee", "Nagpur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kandhar", "Nanded", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kandivali", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kavathe Mahankal", "Sangli", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kelapur", "Yavatmal", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Khalapur", "Raigad", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Khatav", "Satara", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Khed", "Ratnagiri", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kherwadi", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Khetwadi", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kolhapur", "Kolhapur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Kusumba", "Buldhana", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Lakhandur", "Bhandara", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Latur", "Latur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Malad", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Malbar Hill", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Malegaon", "Nashik", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Malvan", "Nandurabar", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mangalvedhe", "Solapur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mangaon", "Raigad", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mangrulpir", "Washim", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Manjlegaon", "Beed", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Matunga", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mazgaon", "Raigad", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Medshi", "Washim", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mira Bhayandar", "Thane", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mudkhed", "Nanded", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Mulshi", "Thane", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Nagar-Akola", "Akola", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Nagpada", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Nanded", "Nanded", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Nashik", "Nashik", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Nehrunagar", "Yavatmal", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Opera House", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Panhala", "Kolhapur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Parel", "Mumbai City", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Parola", "Jalgaon", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Pathardi", "Ahmednagar", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Pulgaon", "Wardha", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Renapur", "Latur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Sangameshwar", "Ratnagiri", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Sangrul", "Kolhapur", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Santacruz", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Shahade", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Sironcha", "Gadchiroli", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Surgana", "Nashik", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Talode", "Nandurabar", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Trombay", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Vadgaon", "Pune", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Vengurla", "Sindhudurg", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Versova", "Mumbai Suburban", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Wada", "Palghar", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Walgaon", "Amaravati", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Walva", "Sangli", elections$district_name)

elections$district_name <- if_else(elections$ac_name=="Yaval", "Jalgaon", elections$district_name)

## Remove observations without a district identifier.
elections <- drop_na(elections, district_name)

## Load individual-level data from the 61st National Sample Survey.
nss <- read_dta("Employment_by_NIC4_Nss61_Sch10_w_id.dta")

## Limit data only to individuals employed in textile-related industries.
nss <- filter(nss, nic4_code98_ >=1700 & nic4_code98_ <1900)

## Group data by district.
nss <- group_by(nss, id)

## Create a district panel of individuals employed in 
## textile-related industries.
nss <- summarize(nss,
                 n_text=sum(total_n, na.rm = TRUE))

## Ungroup data.
nss <- ungroup(nss)

## Calculate each district's share of textile-related employment.
nss <- mutate(nss, text_share=(n_text/sum(n_text))*100)

## Load district crosswalk to add 2001 district codes.
cw <- read_dta("district_crosswalk.dta")

## Merge district crosswalk to add 2001 district codes.
nss <- full_join(nss, cw)

## Replace missing values with zeros to reflect no textile employment.
nss$text_share[is.na(nss$text_share)] <- 0

## Rename and reorder variables.
nss <- select(nss,
              id,
              state_name=state,
              district_name=district,
              text_share)

## Remove non-Maharashtra observations.
nss <- filter(nss, state_name=="MAHARASHTRA")

## Select and reorder variables.
nss <- select(nss,
              id,
              district_name,
              text_share)

## Change format of district names to title case.
nss <- mutate(nss, district_name=str_to_title(district_name))

## Rename some districts for merging purposes.
elections$district_name <- if_else(elections$district_name=="Amaravati", "Amravati", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Gondiya", "Gondia", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Mumbai City", "Mumbai", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Mumbai Suburban", "Mumbai", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Nandurabar", "Nandurbar", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Nashik", "Nasik", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Palghar", "Thane", elections$district_name)

elections$district_name <- if_else(elections$district_name=="Raigad", "Raigarh", elections$district_name)

## Merge elections and textile employment data.
elections <- left_join(elections, nss)

## Remove textile employment data and crosswalk.
rm(nss,
   cw)

## Reorder variables.
elections <- select(elections,
                    id,
                    district_name,
                    ac_name,
                    ac_type,
                    year,
                    text_share,
                    everything())

## Create a post-MFA expiration indicator variable.
elections$post <- if_else(elections$year>=2005, 1, 0)

## Create the primary measure of exposure to the post-MFA textile shock.
elections <- mutate(elections, treatedXpost=text_share*post)

## Merge elections and control variable data.
elections <- left_join(elections, controls)

## Remove control variables.
rm(controls)

## Create a series of year indicator variables.
elections <- dummy_cols(elections, select_columns = "year")

## Save Analysis Dataset 6:
## Constituency panel of nativist voting and covariates
write_csv(elections, "analysis_dataset_6.csv")


#############################################################################
## Analysis Dataset 7: World Population Policies internal migration policy ##
#############################################################################

## The following commands create Analysis Dataset 6:
## World Population Policies internal migration policy

## Load WPP data for 1976.
wpp_76 <- read_xls("1976_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_76 <- select(wpp_76,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_76 <- drop_na(wpp_76,
                  country_name,
                  country_code)

## Recode missing values.
wpp_76$migration_rururb <- ifelse(wpp_76$migration_rururb=="No data available",
                                  NA, wpp_76$migration_rururb)

## Recode missing values.
wpp_76$migration_agglom <- ifelse(wpp_76$migration_agglom=="No data available",
                                  NA, wpp_76$migration_agglom)

## Create year variable.
wpp_76 <- mutate(wpp_76, year=1976)

## Load WPP data for 1986.
wpp_86 <- read_xls("1986_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_86 <- select(wpp_86,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_86 <- drop_na(wpp_86,
                  country_name,
                  country_code)

## Recode missing values.
wpp_86$migration_rururb <- ifelse(wpp_86$migration_rururb=="No data available",
                                  NA, wpp_86$migration_rururb)

## Recode missing values.
wpp_86$migration_agglom <- ifelse(wpp_86$migration_agglom=="No data available",
                                  NA, wpp_86$migration_agglom)

## Create year variable.
wpp_86 <- mutate(wpp_86, year=1986)

## Load WPP data for 1996.
wpp_96 <- read_xls("1996_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_96 <- select(wpp_96,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_96 <- drop_na(wpp_96,
                  country_name,
                  country_code)

## Recode missing values.
wpp_96$migration_rururb <- ifelse(wpp_96$migration_rururb=="No data available",
                                  NA, wpp_96$migration_rururb)

## Recode missing values.
wpp_96$migration_agglom <- ifelse(wpp_96$migration_agglom=="No data available",
                                  NA, wpp_96$migration_agglom)

## Create year variable.
wpp_96 <- mutate(wpp_96, year=1996)

## Load WPP data for 2001.
wpp_01 <- read_xls("2001_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_01 <- select(wpp_01,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_01 <- drop_na(wpp_01,
                  country_name,
                  country_code)

## Recode missing values.
wpp_01$migration_rururb <- ifelse(wpp_01$migration_rururb=="No data available",
                                  NA, wpp_01$migration_rururb)

## Recode missing values.
wpp_01$migration_agglom <- ifelse(wpp_01$migration_agglom=="No data available",
                                  NA, wpp_01$migration_agglom)

## Create year variable.
wpp_01 <- mutate(wpp_01, year=2001)

## Load WPP data for 2003.
wpp_03 <- read_xls("2003_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_03 <- select(wpp_03,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_03 <- drop_na(wpp_03,
                  country_name,
                  country_code)

## Recode missing values.
wpp_03$migration_rururb <- ifelse(wpp_03$migration_rururb=="No data available",
                                  NA, wpp_03$migration_rururb)

## Recode missing values.
wpp_03$migration_agglom <- ifelse(wpp_03$migration_agglom=="No data available",
                                  NA, wpp_03$migration_agglom)

## Create year variable.
wpp_03 <- mutate(wpp_03, year=2003)

## Load WPP data for 2005.
wpp_05 <- read_xls("2005_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_05 <- select(wpp_05,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_05 <- drop_na(wpp_05,
                  country_name,
                  country_code)

## Recode missing values.
wpp_05$migration_rururb <- ifelse(wpp_05$migration_rururb=="No data available",
                                  NA, wpp_05$migration_rururb)

## Recode missing values.
wpp_05$migration_agglom <- ifelse(wpp_05$migration_agglom=="No data available",
                                  NA, wpp_05$migration_agglom)

## Create year variable.
wpp_05 <- mutate(wpp_05, year=2005)

## Load WPP data for 2007.
wpp_07 <- read_xls("2007_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_07 <- select(wpp_07,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_07 <- drop_na(wpp_07,
                  country_name,
                  country_code)

## Recode missing values.
wpp_07$migration_rururb <- ifelse(wpp_07$migration_rururb=="No data available",
                                  NA, wpp_07$migration_rururb)

## Recode missing values.
wpp_07$migration_agglom <- ifelse(wpp_07$migration_agglom=="No data available",
                                  NA, wpp_07$migration_agglom)

## Create year variable.
wpp_07 <- mutate(wpp_07, year=2007)

## Load WPP data for 2009.
wpp_09 <- read_xls("2009_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_09 <- select(wpp_09,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_09 <- drop_na(wpp_09,
                  country_name,
                  country_code)

## Recode missing values.
wpp_09$migration_rururb <- ifelse(wpp_09$migration_rururb=="No data available",
                                  NA, wpp_09$migration_rururb)

## Recode missing values.
wpp_09$migration_agglom <- ifelse(wpp_09$migration_agglom=="No data available",
                                  NA, wpp_09$migration_agglom)

## Create year variable.
wpp_09 <- mutate(wpp_09, year=2009)

## Load WPP data for 2011.
wpp_11 <- read_xls("2011_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_11 <- select(wpp_11,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`,
                 migration_agglom=`Policy on migration into urban agglomerations`)

## Remove non-country observations.
wpp_11 <- drop_na(wpp_11,
                  country_name,
                  country_code)

## Recode missing values.
wpp_11$migration_rururb <- ifelse(wpp_11$migration_rururb=="No data available",
                                  NA, wpp_11$migration_rururb)

## Recode missing values.
wpp_11$migration_agglom <- ifelse(wpp_11$migration_agglom=="No data available",
                                  NA, wpp_11$migration_agglom)

## Create year variable.
wpp_11 <- mutate(wpp_11, year=2011)

## Load WPP data for 2013.
wpp_13 <- read_xls("2013_WPPDataset_AllVariables.xls", skip = 1)

## Select necessary variables.
wpp_13 <- select(wpp_13,
                 country_name=`Country  name`,
                 country_code=`Country code`,
                 development_level=`Development level`,
                 least_developed=`Least developed country`,
                 migration_rururb=`Policy on migration from rural to urban areas`)

## Create variable for merging purposes.
wpp_13 <- mutate(wpp_13, migration_agglom=NA)

## Remove non-country observations.
wpp_13 <- drop_na(wpp_13,
                  country_name,
                  country_code)

## Recode missing values.
wpp_13$migration_rururb <- ifelse(wpp_13$migration_rururb=="No data available",
                                  NA, wpp_13$migration_rururb)

## Create year variable.
wpp_13 <- mutate(wpp_13, year=2013)

## Merge all years of WPP together.
wpp <- rbind(wpp_76,
             wpp_86,
             wpp_96,
             wpp_01,
             wpp_03,
             wpp_05,
             wpp_07,
             wpp_09,
             wpp_11,
             wpp_13)

## Remove individual year datasets.
rm(wpp_76,
   wpp_86,
   wpp_96,
   wpp_01,
   wpp_03,
   wpp_05,
   wpp_07,
   wpp_09,
   wpp_11,
   wpp_13)

## Arrange data by country and year.
wpp <- arrange(wpp,
               country_name,
               year)

## Code countries as least developed.
wpp$development_level <- if_else(wpp$least_developed=="Yes", "Least dev. region", wpp$development_level)

## Remove more developed countries.
wpp <- filter(wpp, development_level!="More dev. region")

## Create indicator for policies that lower rural-urban migration.
wpp$lower_rururb_migration <- if_else(wpp$migration_rururb=="Lower", 1, 0)

## Create indicator for policies that lower migration to urban agglomerations.
wpp$lower_agglom_migration <- if_else(wpp$migration_agglom=="Lower", 1, 0)

## Group data by year.
wpp <- group_by(wpp,
                year)

## Create yearly panel of migration policy variables.
wpp <- summarize(wpp,
                 lower_rururb_migration=mean(lower_rururb_migration, na.rm = TRUE)*100,
                 lower_agglom_migration=mean(lower_agglom_migration, na.rm = TRUE)*100)

## Create long panel of WPP data for graphing purposes.
wpp <- gather(wpp,
              key = Policy,
              value = percent,
              lower_rururb_migration,
              lower_agglom_migration)

## Create policy identifier variable for graphing purposes.
wpp$Policy <- if_else(wpp$Policy=="lower_rururb_migration",
                      "Lower rural-urban migration",
                      "Lower migration to urban agglom.")

## Save Analysis Dataset 7:
## World Population Policies internal migration policy
write_csv(wpp, "analysis_dataset_7.csv")


