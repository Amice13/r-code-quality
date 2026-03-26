# Benefit-Incidence Analysis

#!#!#! NOTE: You will need to setup work directories and download an external dataset from the World Bank (free)
#!#!#! To find where your input is needed, search: "INPUTNEEDED"

### USER INPUT ### DEACTIVATED
# user0 <- Use income (1) or expenditure (2)?
# user1 <- Use sample (0) or national estimates - World Bank (1)?

user0 <- 2
user1 <- 1

### END OF USER INPUT ###


#####
require(tidyverse)
require(dplyr)
require(haven)
require(foreign)
require(cowplot)
require(statar)
require(rineq)
require(broom)


# crosstab function
source("http://pcwww.liv.ac.uk/%7Ewilliam/R/crosstab.r") # "crosstab" function
# Help: http://rstudio-pubs-static.s3.amazonaws.com/6975_c4943349b6174f448104a5513fed59a9.html


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}


# No scientific notation
options(scipen = 999)


################################################################################################
# Household Living Standard Survey
################################################

#!#!#! You must register and download the UNPS data from the World Bank repository (free) at https://microdata.worldbank.org/index.php/catalog/3460 INPUTNEEDED
#!#!#! This program use the CSV files (make sure you select CSV format for downloading)

setwd("FILL IN YOUR WORK DIRECTORY") #INPUTNEEDED

hhinfo <- read.csv("gsec1.csv") # Household information
hhinfo2 <- read.csv("gsec2.csv") # Determines who is head of household
hhinc <- read.csv("gsec8.csv") # Earnings by wage-earner
hhothinc <- read.csv("gsec11_2.csv") # Other income for household (remittance, rentals, investments)
hhasset1 <- read.csv("gsec14.csv") # Household assets
hhasset2 <- read.csv("gsec14b.csv") # Household assets (historical)
hhexp1 <- read.csv("gsec15c.csv") # Non-durable goods purchased in last 30 days
hhexp2 <- read.csv("gsec15d.csv") # Semi-durable goods purchased in last 365 days
hhcoping <- read.csv("gsec16.csv") # Shocks and coping mechanisms

# Show # obs per district
crosstab(hhinfo, row.vars = "district_name")
hhinfo <- hhinfo %>% rename(hhid = HHID)
hhinfo <- full_join(hhinfo,hhinfo2,by="hhid")

# Select ID of Head of Households only
hhinfo_head <- hhinfo %>% filter(h2q4==1) # Filter done for head of the household



### INCOME
# Join hhinfo_head to hhinc
hhinc_head <- inner_join(hhinfo_head,hhinc,by="pid")

# Convert reported earnings to average monthly salary (25 working days per month, 5 working days per week & 8 working hours a day)
hhinc_head <- mutate(hhinc_head,income =
                       ifelse(h8q31c==1, h8q31a*8*25,
                              ifelse(h8q31c==2, h8q31a*25,
                                     ifelse(h8q31c==3, h8q31a*5,
                                            ifelse(h8q31c==4, h8q31a,NA)))))

# Drop income==NA
hhinc_head <- hhinc_head %>% filter(!is.na(income))

# Generate USD
hhinc_head <- hhinc_head %>% mutate(income_USD = income/3727)


# Calculate & print the mean of hhinc_head by district
stats_hhinc_head <- hhinc_head %>% group_by(district_name) %>% summarise_at(vars(income),list(Mean=mean))

# Sort by income (ascending)
sort(hhinc_head$income)

# District specific
hhinc_head_gulu <- hhinc_head %>% filter(district_name == "Gulu")
hhinc_head_jinja <- hhinc_head %>% filter(district_name == "Jinja")
hhinc_head_mbarara <- hhinc_head %>% filter(district_name == "Mbarara")
hhinc_head_wakiso <- hhinc_head %>% filter(district_name == "Wakiso" | district_name == "Kampala") # Kampala included in Wakiso


# Create quintiles, deciles and centiles with weights
hhinc_head$income_n5 <- xtile(hhinc_head$income, n=5, wt=hhinc_head$h_xwgt_W5)
hhinc_head$income_n10 <- xtile(hhinc_head$income, n=10, wt=hhinc_head$h_xwgt_W5)
hhinc_head$income_n100 <- xtile(hhinc_head$income, n=100, wt=hhinc_head$h_xwgt_W5)

# District specific
hhinc_head_gulu$income <- xtile(hhinc_head_gulu$income, n=100, wt=hhinc_head_gulu$h_xwgt_W5)
hhinc_head_jinja$income <- xtile(hhinc_head_jinja$income, n=100, wt=hhinc_head_jinja$h_xwgt_W5)
hhinc_head_mbarara$income <- xtile(hhinc_head_mbarara$income, n=100, wt=hhinc_head_mbarara$h_xwgt_W5)
hhinc_head_wakiso$income <- xtile(hhinc_head_wakiso$income, n=100, wt=hhinc_head_wakiso$h_xwgt_W5)


# Generate range of values per centile
hhinc_head_max <- as.data.frame(tapply(hhinc_head$income, hhinc_head$income, max))
names(hhinc_head_max)[names(hhinc_head_max == "tapply(hhinc_head$income, hhinc_head$income, max)")] <- "upperboundincome"

# District specific (to highest count of obs)
hhinc_head_max_gulu <- as.data.frame(tapply(hhinc_head_gulu$income, hhinc_head_gulu$income, max))
hhinc_head_max_jinja <- as.data.frame(tapply(hhinc_head_jinja$income, hhinc_head_jinja$income, max))
hhinc_head_max_mbarara <- as.data.frame(tapply(hhinc_head_mbarara$income, hhinc_head_mbarara$income, max))
hhinc_head_max_wakiso <- as.data.frame(tapply(hhinc_head_wakiso$income, hhinc_head_wakiso$income, max))
names(hhinc_head_max_gulu)[names(hhinc_head_max_gulu == "tapply(hhinc_head_gulu$income, hhinc_head_gulu$income, max)")] <- "upperboundincome"
names(hhinc_head_max_jinja)[names(hhinc_head_max_jinja == "tapply(hhinc_head_jinja$income, hhinc_head_jinja$income, max)")] <- "upperboundincome"
names(hhinc_head_max_mbarara)[names(hhinc_head_max_mbarara == "tapply(hhinc_head_mbarara$income, hhinc_head_mbarara$income, max)")] <- "upperboundincome"
names(hhinc_head_max_wakiso)[names(hhinc_head_max_wakiso == "tapply(hhinc_head_wakiso$income, hhinc_head_wakiso$income, max)")] <- "upperboundincome"



### CONSUMPTION
# Distribution based on consumption
hhexp1$consumption <- rowSums(hhexp1[, c(7, 9, 11)], na.rm = T) # Sum all 3 cost variables of consumption per item (column)
hhexp1 <- hhexp1 %>% group_by(hhid) %>% mutate(totalconsumption = cumsum(consumption)) # Sum cost of all items (row)

# Keep max value only
hhexp1 <- hhexp1 %>% group_by(hhid) %>% arrange(desc(totalconsumption)) %>% slice(1) %>% ungroup()
hhexp1 <- hhexp1 %>% mutate(hh.x = hhid)

# Join exp and hhinfo (for weights)
hhexp <- inner_join(hhinfo_head,hhexp1,by="hh.x")

# Drop totalconsumption==NA
hhexp <- hhexp %>% filter(!is.na(totalconsumption))

# Generate USD
hhexp <- hhexp %>% mutate(totalconsumption_USD = totalconsumption/3727)


# Sort by income (ascending)
sort(hhexp$totalconsumption)

# District specific
hhexp_gulu <- hhexp %>% filter(district_name == "Gulu")
hhexp_jinja <- hhexp %>% filter(district_name == "Jinja")
hhexp_mbarara <- hhexp %>% filter(district_name == "Mbarara")
hhexp_wakiso <- hhexp %>% filter(district_name == "Wakiso" | district_name == "Kampala") # Kampala included in Wakiso


# Create quintiles, deciles and centiles with weights
hhexp$exp_n5 <- xtile(hhexp$totalconsumption, n=5, wt=hhexp$h_xwgt_W5)
hhexp$exp_n10 <- xtile(hhexp$totalconsumption, n=10, wt=hhexp$h_xwgt_W5)
hhexp$exp_n100 <- xtile(hhexp$totalconsumption, n=100, wt=hhexp$h_xwgt_W5)

# District specific
hhexp_gulu$exp_n100 <- xtile(hhexp_gulu$totalconsumption, n=100, wt=hhexp_gulu$h_xwgt_W5)
hhexp_jinja$exp_n100 <- xtile(hhexp_jinja$totalconsumption, n=100, wt=hhexp_jinja$h_xwgt_W5)
hhexp_mbarara$exp_n100 <- xtile(hhexp_mbarara$totalconsumption, n=100, wt=hhexp_mbarara$h_xwgt_W5)
hhexp_wakiso$exp_n100 <- xtile(hhexp_wakiso$totalconsumption, n=100, wt=hhexp_wakiso$h_xwgt_W5)


# Generate range of values per centile
hhexp_max <- as.data.frame(tapply(hhexp$totalconsumption, hhexp$exp_n100, max))
names(hhexp_max)[names(hhexp_max == "tapply(hhexp$totalconsumption, hhexp$exp_n100, max)")] <- "upperboundexpenses"

# District specific (to highest count of obs)
hhexp_max_gulu <- as.data.frame(tapply(hhexp_gulu$totalconsumption, hhexp_gulu$exp_n100, max))
hhexp_max_jinja <- as.data.frame(tapply(hhexp_jinja$totalconsumption, hhexp_jinja$exp_n100, max))
hhexp_max_mbarara <- as.data.frame(tapply(hhexp_mbarara$totalconsumption, hhexp_mbarara$exp_n100, max))
hhexp_max_wakiso <- as.data.frame(tapply(hhexp_wakiso$totalconsumption, hhexp_wakiso$exp_n100, max))
names(hhexp_max_gulu)[names(hhexp_max_gulu == "tapply(hhexp_gulu$totalconsumption, hhexp_gulu$exp_n100, max)")] <- "upperboundexpenses"
names(hhexp_max_jinja)[names(hhexp_max_jinja == "tapply(hhexp_jinja$totalconsumption, hhexp_jinja$exp_n100, max)")] <- "upperboundexpenses"
names(hhexp_max_mbarara)[names(hhexp_max_mbarara == "tapply(hhexp_mbarara$totalconsumption, hhexp_mbarara$exp_n100, max)")] <- "upperboundexpenses"
names(hhexp_max_wakiso)[names(hhexp_max_wakiso == "tapply(hhexp_wakiso$totalconsumption, hhexp_wakiso$exp_n100, max)")] <- "upperboundexpenses"


# Adjustment for inflation & PPP: from 2016 USD to 2018 USD
# (inflation 2018 --> 2010) / ((Conversion rate LCU --> USD 2018) / 2010) = 0.993865696
# (inflation 2018 --> 2010) = 171.1417273 / 158.0152532
# ((Conversion rate LCU --> USD 2018) / 2010) = 3727.071722 / 3420.098007
#hhinc_head_max <- hhinc_head_max %>% mutate(upperboundincome_corrected = upperboundincome*0.993865696)
#hhinc_head_max <- hhinc_head_max %>% mutate(upperboundincome_corrected = round(upperboundincome_corrected,digits = 0))

#hhexp_max <- hhexp_max %>% mutate(upperboundexpenses_corrected = upperboundexpenses*0.993865696)
#hhexp_max <- hhexp_max %>% mutate(upperboundexpenses_corrected = round(upperboundexpenses_corrected,digits = 0))


#####
# Merge head of household income and household consumption for national income distribution graph (by SES)
hhincexp <- hhexp
hhincexp <- hhincexp %>% select(hhid.x, district, district_name, scounty_code, parish_code, parish_name, village_name, urban, year, month, day, h_xwgt_W5, wave, hh.x, totalconsumption, totalconsumption_USD, exp_n5, exp_n10, exp_n100)
hhinc_head <- hhinc_head %>% select(hhid.x, h8q19a, h8q19B, h8q20A, h8q20B, h8q21a, hh, income, income_USD, income_n5, income_n10, income_n100)
hhincexp <- left_join(hhincexp,hhinc_head,by="hhid.x")

hhincexp <- hhincexp %>% mutate(exp_n5 = case_when(
  exp_n5 == 1 ~ 0.2,
  exp_n5 == 2 ~ 0.4,
  exp_n5 == 3 ~ 0.6,
  exp_n5 == 4 ~ 0.8,
  exp_n5 == 5 ~ 1.0))
hhincexp <- hhincexp %>% mutate(exp_n10 = exp_n10/10)
hhincexp <- hhincexp %>% mutate(exp_n100 = exp_n100/100)

hhincexp_noNA <- hhincexp %>% filter(!is.na(income))



################################################################################################
# Set up DOVE datasets
################################################

# Import Societal COI data
setwd("FILL IN YOUR WORK DIRECTORY IF DIFFERENT FROM WHERE WORLD BANK DATA IS LOCATED") #INPUTNEEDED
ugasoc <- read_dta("UGA_Societal.dta") # Stata file available at


# Cost variables
# dmc_facility: Facility costs minus caregiver OOP at facility
# dmc_caregiver_c: Caregiver direct medical costs at current facility
# dmc_caregiver_ba: Caregiver direct medical costs at facilities before and after
# nmc_caregiver: Caregiver direct non-medical costs overall
# ic_caregiver: Caregiver indirect costs overall (based on head of household income)


# Standardize faciliy cost variable
ugasoc <- ugasoc %>% mutate(dmc_facility = ifelse(disease==3&visittype==1,dmc_facility_p_ipd,
                                                  ifelse(disease==3&visittype==2,dmc_facility_p_opd,
                                                         ifelse(disease==1&visittype==1,dmc_facility_d_ipd,
                                                                ifelse(disease==1&visittype==2,dmc_facility_d_opd,
                                                                       ifelse(disease==2&visittype==1,dmc_facility_m_ipd,dmc_facility_m_opd))))))


# Select only relevant variables
ugasoc <- ugasoc %>% select(dmc_facility,dmc_caregiver_ba,dmc_caregiver_c,nmc_caregiver,ic_caregiver,district,facility, disease, visittype, los, c3_caretakerid, gender_caregiver, age_caregiver, marital, educ_caregiver, ethnicity, residence, gender_child, agechild, rel_cg_child, a3, facility, sector, facilitytype, facilitytype_public, facilitytype_private, hhhead_income_month, hhcaregiver_income_month, hh_income_month, hh_totalexp_month, hh_totalexpnf_month, time_d_p_ipd, time_d_p_opd, time_d_d_ipd, time_d_d_opd, time_d_m_ipd, time_d_m_opd, distance, distance_before, distance_current, distance_after,f3,f4, f4_bcg, f4_polioopvbirth, f4_polioopv1, f4_polioopv2, f4_polioopv3, f4_polioipv, f4_penta1, f4_penta2, f4_penta3, f4_pcv1, f4_pcv2, f4_pcv3, f4_measles1, f4_measles2, f4_mr2)

# Income as integer
ugasoc <- ugasoc %>% filter(!is.na(hhhead_income_month))
ugasoc <- ugasoc %>% mutate(hhhead_income_month = round(hhhead_income_month,digits = 0))
ugasoc <- ugasoc %>% mutate(hhhead_income_month = as.integer(hhhead_income_month))

# Expenses as integer
ugasoc <- ugasoc %>% filter(!is.na(hh_totalexpnf_month))
ugasoc <- ugasoc %>% mutate(hh_totalexpnf_month = round(hh_totalexpnf_month,digits = 0))
ugasoc <- ugasoc %>% mutate(hh_totalexpnf_month = as.integer(hh_totalexpnf_month))


# Sum total time
ugasoc <- ugasoc %>% mutate(time = case_when(
  !is.na(time_d_p_ipd) ~ time_d_p_ipd,
  !is.na(time_d_p_opd) ~ time_d_p_opd,
  !is.na(time_d_d_ipd) ~ time_d_d_ipd,
  !is.na(time_d_d_opd) ~ time_d_d_opd,
  !is.na(time_d_m_ipd) ~ time_d_m_ipd,
  !is.na(time_d_m_opd) ~ time_d_m_opd))

# Time as numeric (2 digits)
ugasoc <- ugasoc %>% filter(!is.na(time))
ugasoc <- ugasoc %>% mutate(time = round(time,digits = 2))
ugasoc <- ugasoc %>% mutate(time = as.numeric(time))

# Distance as numeric (1 digit)
ugasoc <- ugasoc %>% filter(!is.na(distance))
ugasoc <- ugasoc %>% mutate(distance = round(distance,digits = 1))
ugasoc <- ugasoc %>% mutate(distance = as.numeric(distance))


# Sort by income (ascending)
ugasoc$hhhead_income_month <- sort(ugasoc$hhhead_income_month)


# Recode variables: more readable and dummy vars
ugasoc <- ugasoc %>%
  mutate(disease_label = case_when(
  disease == 1 ~ "diarrhea",
  disease == 2 ~ "measles",
  disease == 3 ~ "pneumonia"))
ugasoc <- ugasoc %>%
  mutate(sector_label = case_when(
    sector == 1 ~ "public",
    sector == 2 ~ "PFP",
    sector == 3 ~ "PNFP"))
ugasoc <- ugasoc %>%
  mutate(sector_public = case_when(
    sector == 1 ~ 1,
    sector == 2 ~ 0,
    sector == 3 ~ 0))
ugasoc <- ugasoc %>%
  mutate(male_child = case_when(
    gender_child == 1 ~ 0,
    gender_child == 2 ~ 1))
ugasoc <- ugasoc %>%
  mutate(male_caregiver = case_when(
    gender_caregiver == 1 ~ 0,
    gender_caregiver == 2 ~ 1))
ugasoc <- ugasoc %>%
  mutate(district_label = case_when(
    a3 == "Jinja district" ~ "3_Jinja",
    a3 == "Gulu district" ~ "2_Gulu",
    a3 == "Mbarara district" ~ "4_Mbarara",
    a3 == "Wakiso district" ~ "1_Wakiso"))
ugasoc <- ugasoc %>%
  mutate(ipd = case_when(
    visittype == 2 ~ 0,
    visittype == 1 ~ 1))
ugasoc <- ugasoc %>%
  mutate(ipd_label = case_when(
    ipd == 0 ~ "Ambulatory",
    ipd == 1 ~ "Hospitalized"))
ugasoc <- ugasoc %>%
  mutate(facilitytype_public_label = case_when(
    facilitytype_public == 1 ~ "HC2",
    facilitytype_public == 2 ~ "HC3",
    facilitytype_public == 3 ~ "HC4",
    facilitytype_public == 4 ~ "RRH"))
ugasoc <- ugasoc %>%
  mutate(married = case_when(
    marital == "Married" ~ 1,
    marital != "Married" ~ 0))
ugasoc <- ugasoc %>%
  mutate(Bagisu = case_when(
    ethnicity == "Bagisu" ~ 1,
    ethnicity != "Bagisu" ~ 0))
ugasoc <- ugasoc %>%
  mutate(residence_label = as.factor(case_when(
    residence == 1 ~ "3_rural",
    residence == 2 ~ "2_semiurban",
    residence == 3 ~ "1_urban")))
ugasoc <- ugasoc %>%
  mutate(education = as.factor(case_when(
    educ_caregiver == 1 ~ "1_Noeduc",
    educ_caregiver == 2 ~ "2_Incprimary",
    educ_caregiver == 3 ~ "3_Primary",
    educ_caregiver == 4 ~ "4_Olevel",
    educ_caregiver == 5 ~ "5_Alevel",
    educ_caregiver == 6 ~ "6_Tertiary")))
ugasoc <- within(ugasoc, education <- relevel(education, ref = "1_Noeduc"))
ugasoc <- ugasoc %>%
  mutate(f3_label = as.factor(case_when(
    f3 == "Yes vaccinated and card seen" ~ "1_yes_cardseen",
    f3 == "Yes vaccinated, but card not seen" ~ "2_yes_cardnotseen",
    f3 == "Yes vaccinated, but no card" ~ "3_yes_nocard",
    f3 == "Not vaccinated, no card" ~ "4_no")))
ugasoc <- ugasoc %>%
  mutate(immunizationcard_seen = as.factor(case_when(
    f3 == "Yes vaccinated and card seen" ~ 1,
    f3 != "Yes vaccinated and card seen" ~ 0)))


# Convert to USD
ugasoc <- ugasoc %>% mutate(dmc_facility_UGX = dmc_facility)
ugasoc <- ugasoc %>% mutate(dmc_facility = dmc_facility/3727)
ugasoc <- ugasoc %>% mutate(dmc_caregiver_c_UGX = dmc_caregiver_c)
ugasoc <- ugasoc %>% mutate(dmc_caregiver_c = dmc_caregiver_c/3727)
ugasoc <- ugasoc %>% mutate(dmc_caregiver_ba_UGX = dmc_caregiver_ba)
ugasoc <- ugasoc %>% mutate(dmc_caregiver_ba = dmc_caregiver_ba/3727)
ugasoc <- ugasoc %>% mutate(nmc_caregiver_UGX = nmc_caregiver)
ugasoc <- ugasoc %>% mutate(nmc_caregiver = nmc_caregiver/3727)
ugasoc <- ugasoc %>% mutate(ic_caregiver_UGX = ic_caregiver)
ugasoc <- ugasoc %>% mutate(ic_caregiver = ic_caregiver/3727)



# Filter by disease and type of care
ugasoc_p_ipd <- ugasoc %>% filter(disease==3&visittype==1) # Inpatient pneumonia (p_ipd)
ugasoc_p_opd <- ugasoc %>% filter(disease==3&visittype==2) # Outpatient pneumonia (p_opd)
ugasoc_d_ipd <- ugasoc %>% filter(disease==1&visittype==1) # Inpatient diarrhea (d_ipd)
ugasoc_d_opd <- ugasoc %>% filter(disease==1&visittype==2) # Outpatient diarrhea (d_opd)
ugasoc_m_ipd <- ugasoc %>% filter(disease==2&visittype==1) # Inpatient measles (m_ipd)
ugasoc_m_opd <- ugasoc %>% filter(disease==2&visittype==2) # Outpatient measles (m_opd)




# Associate sample income with WB centiles
# Functions for conversions WB centiles --> deciles & quantiles
mutate_inc_WBcentiles <- function(dataset){dataset %>% mutate(hhhead_income_wb = case_when(
  hhhead_income_month <= 10000 ~ 0.01,
  hhhead_income_month > 10000 & hhhead_income_month <= 20000 ~ 0.02,
  hhhead_income_month > 20000 & hhhead_income_month <= 25000 ~ 0.03,
  hhhead_income_month > 25000 & hhhead_income_month <= 30000 ~ 0.04,
  hhhead_income_month > 30000 & hhhead_income_month <= 40000 ~ 0.05,
  hhhead_income_month > 40000 & hhhead_income_month <= 50000 ~ 0.06,
  hhhead_income_month > 50000 & hhhead_income_month <= 60000 ~ 0.10,
  hhhead_income_month > 60000 & hhhead_income_month <= 70000 ~ 0.12,
  hhhead_income_month > 70000 & hhhead_income_month <= 75000 ~ 0.13,
  hhhead_income_month > 75000 & hhhead_income_month <= 80000 ~ 0.14,
  hhhead_income_month > 80000 & hhhead_income_month <= 91650 ~ 0.15,
  hhhead_income_month > 91650 & hhhead_income_month <= 100000 ~ 0.16,
  hhhead_income_month > 100000 & hhhead_income_month <= 120000 ~ 0.22,
  hhhead_income_month > 120000 & hhhead_income_month <= 125000 ~ 0.23,
  hhhead_income_month > 125000 & hhhead_income_month <= 150000 ~ 0.29,
  hhhead_income_month > 150000 & hhhead_income_month <= 175000 ~ 0.34,
  hhhead_income_month > 175000 & hhhead_income_month <= 180000 ~ 0.35,
  hhhead_income_month > 180000 & hhhead_income_month <= 200000 ~ 0.36,
  hhhead_income_month > 200000 & hhhead_income_month <= 245000 ~ 0.40,
  hhhead_income_month > 245000 & hhhead_income_month <= 250000 ~ 0.41,
  hhhead_income_month > 250000 & hhhead_income_month <= 260000 ~ 0.49,
  hhhead_income_month > 260000 & hhhead_income_month <= 275000 ~ 0.50,
  hhhead_income_month > 275000 & hhhead_income_month <= 300000 ~ 0.51,
  hhhead_income_month > 300000 & hhhead_income_month <= 330000 ~ 0.58,
  hhhead_income_month > 330000 & hhhead_income_month <= 350000 ~ 0.59,
  hhhead_income_month > 350000 & hhhead_income_month <= 375000 ~ 0.61,
  hhhead_income_month > 375000 & hhhead_income_month <= 395000 ~ 0.65,
  hhhead_income_month > 395000 & hhhead_income_month <= 400000 ~ 0.66,
  hhhead_income_month > 400000 & hhhead_income_month <= 418000 ~ 0.68,
  hhhead_income_month > 418000 & hhhead_income_month <= 430000 ~ 0.69,
  hhhead_income_month > 430000 & hhhead_income_month <= 450000 ~ 0.70,
  hhhead_income_month > 450000 & hhhead_income_month <= 467000 ~ 0.73,
  hhhead_income_month > 467000 & hhhead_income_month <= 480000 ~ 0.74,
  hhhead_income_month > 480000 & hhhead_income_month <= 490000 ~ 0.75,
  hhhead_income_month > 490000 & hhhead_income_month <= 500000 ~ 0.76,
  hhhead_income_month > 500000 & hhhead_income_month <= 550000 ~ 0.84,
  hhhead_income_month > 550000 & hhhead_income_month <= 600000 ~ 0.85,
  hhhead_income_month > 600000 & hhhead_income_month <= 650000 ~ 0.87,
  hhhead_income_month > 650000 & hhhead_income_month <= 700000 ~ 0.88,
  hhhead_income_month > 700000 & hhhead_income_month <= 750000 ~ 0.89,
  hhhead_income_month > 750000 & hhhead_income_month <= 800000 ~ 0.91,
  hhhead_income_month > 800000 & hhhead_income_month <= 875000 ~ 0.93,
  hhhead_income_month > 875000 & hhhead_income_month <= 1000000 ~ 0.94,
  hhhead_income_month > 1000000 & hhhead_income_month <= 1200000 ~ 0.95,
  hhhead_income_month > 1200000 & hhhead_income_month <= 1250000 ~ 0.96,
  hhhead_income_month > 1250000 & hhhead_income_month <= 1600000 ~ 0.97,
  hhhead_income_month > 1600000 & hhhead_income_month <= 2000000 ~ 0.98,
  hhhead_income_month > 2000000 & hhhead_income_month <= 2800000 ~ 0.99,
  hhhead_income_month > 2800000 ~ 1.0))} # Generate WB income centiles (was hhhead_income_month > 2800000 & hhhead_income_month <= 8000000)
mutate_inc_WBdeciles <- function(dataset){dataset %>% mutate(hhhead_income_n10_wb = case_when(
  hhhead_income_wb <= 0.1 ~ 0.1,
  hhhead_income_wb > 0.1 & hhhead_income_wb <= 0.2 ~ 0.2,
  hhhead_income_wb > 0.2 & hhhead_income_wb <= 0.3 ~ 0.3,
  hhhead_income_wb > 0.3 & hhhead_income_wb <= 0.4 ~ 0.4,
  hhhead_income_wb > 0.4 & hhhead_income_wb <= 0.5 ~ 0.5,
  hhhead_income_wb > 0.5 & hhhead_income_wb <= 0.6 ~ 0.6,
  hhhead_income_wb > 0.6 & hhhead_income_wb <= 0.7 ~ 0.7,
  hhhead_income_wb > 0.7 & hhhead_income_wb <= 0.8 ~ 0.8,
  hhhead_income_wb > 0.8 & hhhead_income_wb <= 0.9 ~ 0.9,
  hhhead_income_wb > 0.9 & hhhead_income_wb <= 1.0 ~ 1.0))} # WB centiles --> deciles
mutate_inc_WBquantiles <- function(dataset){dataset %>% mutate(hhhead_income_n5_wb = case_when(
  hhhead_income_wb <= 0.2 ~ 0.2,
  hhhead_income_wb > 0.2 & hhhead_income_wb <= 0.4 ~ 0.4,
  hhhead_income_wb > 0.4 & hhhead_income_wb <= 0.6 ~ 0.6,
  hhhead_income_wb > 0.6 & hhhead_income_wb <= 0.8 ~ 0.8,
  hhhead_income_wb > 0.8 & hhhead_income_wb <= 1.0 ~ 1.0))}  # WB centiles --> quantiles


# Associate sample consumption (non-food expenditure) with WB centiles
mutate_exp_WBcentiles <- function(dataset){dataset %>% mutate(hhexp_wb = case_when(
  hh_totalexpnf_month <= 19300 ~ 0.01,
  hh_totalexpnf_month > 19300 & hh_totalexpnf_month <= 24200 ~ 0.02,
  hh_totalexpnf_month > 24200 & hh_totalexpnf_month <= 26700 ~ 0.03,
  hh_totalexpnf_month > 26700 & hh_totalexpnf_month <= 28200 ~ 0.04,
  hh_totalexpnf_month > 28200 & hh_totalexpnf_month <= 30800 ~ 0.05,
  hh_totalexpnf_month > 30800 & hh_totalexpnf_month <= 32800 ~ 0.06,
  hh_totalexpnf_month > 32800 & hh_totalexpnf_month <= 35100 ~ 0.07,
  hh_totalexpnf_month > 35100 & hh_totalexpnf_month <= 36400 ~ 0.08,
  hh_totalexpnf_month > 36400 & hh_totalexpnf_month <= 38000 ~ 0.09,
  hh_totalexpnf_month > 38000 & hh_totalexpnf_month <= 39200 ~ 0.10,
  hh_totalexpnf_month > 39200 & hh_totalexpnf_month <= 41250 ~ 0.11,
  hh_totalexpnf_month > 41250 & hh_totalexpnf_month <= 43200 ~ 0.12,
  hh_totalexpnf_month > 43200 & hh_totalexpnf_month <= 44650 ~ 0.13,
  hh_totalexpnf_month > 44650 & hh_totalexpnf_month <= 46700 ~ 0.14,
  hh_totalexpnf_month > 46700 & hh_totalexpnf_month <= 48400 ~ 0.15,
  hh_totalexpnf_month > 48400 & hh_totalexpnf_month <= 49800 ~ 0.16,
  hh_totalexpnf_month > 49800 & hh_totalexpnf_month <= 51200 ~ 0.17,
  hh_totalexpnf_month > 51200 & hh_totalexpnf_month <= 52900 ~ 0.18,
  hh_totalexpnf_month > 52900 & hh_totalexpnf_month <= 54500 ~ 0.19,
  hh_totalexpnf_month > 54500 & hh_totalexpnf_month <= 55700 ~ 0.20,
  hh_totalexpnf_month > 55700 & hh_totalexpnf_month <= 57100 ~ 0.21,
  hh_totalexpnf_month > 57100 & hh_totalexpnf_month <= 58600 ~ 0.22,
  hh_totalexpnf_month > 58600 & hh_totalexpnf_month <= 59800 ~ 0.23,
  hh_totalexpnf_month > 59800 & hh_totalexpnf_month <= 61000 ~ 0.24,
  hh_totalexpnf_month > 61000 & hh_totalexpnf_month <= 62400 ~ 0.25,
  hh_totalexpnf_month > 62400 & hh_totalexpnf_month <= 64000 ~ 0.26,
  hh_totalexpnf_month > 64000 & hh_totalexpnf_month <= 64800 ~ 0.27,
  hh_totalexpnf_month > 64800 & hh_totalexpnf_month <= 66400 ~ 0.28,
  hh_totalexpnf_month > 66400 & hh_totalexpnf_month <= 68200 ~ 0.29,
  hh_totalexpnf_month > 68200 & hh_totalexpnf_month <= 70200 ~ 0.30,
  hh_totalexpnf_month > 70200 & hh_totalexpnf_month <= 71500 ~ 0.31,
  hh_totalexpnf_month > 71500 & hh_totalexpnf_month <= 72800 ~ 0.32,
  hh_totalexpnf_month > 72800 & hh_totalexpnf_month <= 74000 ~ 0.33,
  hh_totalexpnf_month > 74000 & hh_totalexpnf_month <= 75000 ~ 0.34,
  hh_totalexpnf_month > 75000 & hh_totalexpnf_month <= 76700 ~ 0.35,
  hh_totalexpnf_month > 76700 & hh_totalexpnf_month <= 78400 ~ 0.36,
  hh_totalexpnf_month > 78400 & hh_totalexpnf_month <= 79900 ~ 0.37,
  hh_totalexpnf_month > 79900 & hh_totalexpnf_month <= 82000 ~ 0.38,
  hh_totalexpnf_month > 82000 & hh_totalexpnf_month <= 84100 ~ 0.39,
  hh_totalexpnf_month > 84100 & hh_totalexpnf_month <= 86200 ~ 0.40,
  hh_totalexpnf_month > 86200 & hh_totalexpnf_month <= 88000 ~ 0.41,
  hh_totalexpnf_month > 88000 & hh_totalexpnf_month <= 90000 ~ 0.42,
  hh_totalexpnf_month > 90000 & hh_totalexpnf_month <= 91100 ~ 0.43,
  hh_totalexpnf_month > 91100 & hh_totalexpnf_month <= 92800 ~ 0.44,
  hh_totalexpnf_month > 92800 & hh_totalexpnf_month <= 94600 ~ 0.45,
  hh_totalexpnf_month > 94600 & hh_totalexpnf_month <= 96300 ~ 0.46,
  hh_totalexpnf_month > 96300 & hh_totalexpnf_month <= 97700 ~ 0.47,
  hh_totalexpnf_month > 97700 & hh_totalexpnf_month <= 99200 ~ 0.48,
  hh_totalexpnf_month > 99200 & hh_totalexpnf_month <= 101900 ~ 0.49,
  hh_totalexpnf_month > 101900 & hh_totalexpnf_month <= 103700 ~ 0.50,
  hh_totalexpnf_month > 103700 & hh_totalexpnf_month <= 106000 ~ 0.51,
  hh_totalexpnf_month > 106000 & hh_totalexpnf_month <= 108000 ~ 0.52,
  hh_totalexpnf_month > 108000 & hh_totalexpnf_month <= 110900 ~ 0.53,
  hh_totalexpnf_month > 110900 & hh_totalexpnf_month <= 114000 ~ 0.54,
  hh_totalexpnf_month > 114000 & hh_totalexpnf_month <= 117000 ~ 0.55,
  hh_totalexpnf_month > 117000 & hh_totalexpnf_month <= 119150 ~ 0.56,
  hh_totalexpnf_month > 119150 & hh_totalexpnf_month <= 121400 ~ 0.57,
  hh_totalexpnf_month > 121400 & hh_totalexpnf_month <= 123700 ~ 0.58,
  hh_totalexpnf_month > 123700 & hh_totalexpnf_month <= 126200 ~ 0.59,
  hh_totalexpnf_month > 126200 & hh_totalexpnf_month <= 129000 ~ 0.60,
  hh_totalexpnf_month > 129000 & hh_totalexpnf_month <= 131200 ~ 0.61,
  hh_totalexpnf_month > 131200 & hh_totalexpnf_month <= 134600 ~ 0.62,
  hh_totalexpnf_month > 134600 & hh_totalexpnf_month <= 137700 ~ 0.63,
  hh_totalexpnf_month > 137700 & hh_totalexpnf_month <= 140900 ~ 0.64,
  hh_totalexpnf_month > 140900 & hh_totalexpnf_month <= 143100 ~ 0.65,
  hh_totalexpnf_month > 143100 & hh_totalexpnf_month <= 146600 ~ 0.66,
  hh_totalexpnf_month > 146600 & hh_totalexpnf_month <= 151900 ~ 0.67,
  hh_totalexpnf_month > 151900 & hh_totalexpnf_month <= 155500 ~ 0.68,
  hh_totalexpnf_month > 155500 & hh_totalexpnf_month <= 158000 ~ 0.69,
  hh_totalexpnf_month > 158000 & hh_totalexpnf_month <= 161700 ~ 0.70,
  hh_totalexpnf_month > 161700 & hh_totalexpnf_month <= 163500 ~ 0.71,
  hh_totalexpnf_month > 163500 & hh_totalexpnf_month <= 167700 ~ 0.72,
  hh_totalexpnf_month > 167700 & hh_totalexpnf_month <= 172500 ~ 0.73,
  hh_totalexpnf_month > 172500 & hh_totalexpnf_month <= 177600 ~ 0.74,
  hh_totalexpnf_month > 177600 & hh_totalexpnf_month <= 185000 ~ 0.75,
  hh_totalexpnf_month > 185000 & hh_totalexpnf_month <= 191000 ~ 0.76,
  hh_totalexpnf_month > 191000 & hh_totalexpnf_month <= 196600 ~ 0.77,
  hh_totalexpnf_month > 196600 & hh_totalexpnf_month <= 204500 ~ 0.78,
  hh_totalexpnf_month > 204500 & hh_totalexpnf_month <= 212050 ~ 0.79,
  hh_totalexpnf_month > 212050 & hh_totalexpnf_month <= 218600 ~ 0.80,
  hh_totalexpnf_month > 218600 & hh_totalexpnf_month <= 225500 ~ 0.81,
  hh_totalexpnf_month > 225500 & hh_totalexpnf_month <= 233400 ~ 0.82,
  hh_totalexpnf_month > 233400 & hh_totalexpnf_month <= 240700 ~ 0.83,
  hh_totalexpnf_month > 240700 & hh_totalexpnf_month <= 249800 ~ 0.84,
  hh_totalexpnf_month > 249800 & hh_totalexpnf_month <= 264300 ~ 0.85,
  hh_totalexpnf_month > 264300 & hh_totalexpnf_month <= 278200 ~ 0.86,
  hh_totalexpnf_month > 278200 & hh_totalexpnf_month <= 289900 ~ 0.87,
  hh_totalexpnf_month > 289900 & hh_totalexpnf_month <= 298500 ~ 0.88,
  hh_totalexpnf_month > 298500 & hh_totalexpnf_month <= 315100 ~ 0.89,
  hh_totalexpnf_month > 315100 & hh_totalexpnf_month <= 335300 ~ 0.90,
  hh_totalexpnf_month > 335300 & hh_totalexpnf_month <= 365150 ~ 0.91,
  hh_totalexpnf_month > 365150 & hh_totalexpnf_month <= 391200 ~ 0.92,
  hh_totalexpnf_month > 391200 & hh_totalexpnf_month <= 433600 ~ 0.93,
  hh_totalexpnf_month > 433600 & hh_totalexpnf_month <= 472400 ~ 0.94,
  hh_totalexpnf_month > 472400 & hh_totalexpnf_month <= 512400 ~ 0.95,
  hh_totalexpnf_month > 512400 & hh_totalexpnf_month <= 580300 ~ 0.96,
  hh_totalexpnf_month > 580300 & hh_totalexpnf_month <= 690700 ~ 0.97,
  hh_totalexpnf_month > 690700 & hh_totalexpnf_month <= 831500 ~ 0.98,
  hh_totalexpnf_month > 831500 & hh_totalexpnf_month <= 1218700 ~ 0.99,
  hh_totalexpnf_month > 1218700 ~ 1.0))} # (was hh_totalexpnf_month > 1218700 & hh_totalexpnf_month <= 62828000)
mutate_exp_WBdeciles <- function(dataset){dataset %>% mutate(hhexp_n10_wb = case_when(
  hhexp_wb <= 0.1 ~ 0.1,
  hhexp_wb > 0.1 & hhexp_wb <= 0.2 ~ 0.2,
  hhexp_wb > 0.2 & hhexp_wb <= 0.3 ~ 0.3,
  hhexp_wb > 0.3 & hhexp_wb <= 0.4 ~ 0.4,
  hhexp_wb > 0.4 & hhexp_wb <= 0.5 ~ 0.5,
  hhexp_wb > 0.5 & hhexp_wb <= 0.6 ~ 0.6,
  hhexp_wb > 0.6 & hhexp_wb <= 0.7 ~ 0.7,
  hhexp_wb > 0.7 & hhexp_wb <= 0.8 ~ 0.8,
  hhexp_wb > 0.8 & hhexp_wb <= 0.9 ~ 0.9,
  hhexp_wb > 0.9 & hhexp_wb <= 1.0 ~ 1.0))}
mutate_exp_WBquantiles <- function(dataset){dataset %>% mutate(hhexp_n5_wb = case_when(
  hhexp_wb <= 0.2 ~ 0.2,
  hhexp_wb > 0.2 & hhexp_wb <= 0.4 ~ 0.4,
  hhexp_wb > 0.4 & hhexp_wb <= 0.6 ~ 0.6,
  hhexp_wb > 0.6 & hhexp_wb <= 0.8 ~ 0.8,
  hhexp_wb > 0.8 & hhexp_wb <= 1.0 ~ 1.0))}


# Create sample quintiles, deciles and centiles for ALL VISITS (public & private)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(hhhead_income_n5 = ntile(hhhead_income_month,5) * 0.2) # Quantiles
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(hhhead_income_n10 = ntile(hhhead_income_month,10) * 0.1) # Deciles
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(hhhead_income = ntile(hhhead_income_month,100) * 0.01) # Centiles
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(hhhead_income_n5 = ntile(hhhead_income_month,5) * 0.2) # Quantiles
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(hhhead_income_n10 = ntile(hhhead_income_month,10) * 0.1) # Deciles
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(hhhead_income = ntile(hhhead_income_month,100) * 0.01) # Centiles
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(hhhead_income_n5 = ntile(hhhead_income_month,5) * 0.2) # Quantiles
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(hhhead_income_n10 = ntile(hhhead_income_month,10) * 0.1) # Deciles
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(hhhead_income = ntile(hhhead_income_month,100) * 0.01) # Centiles
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(hhhead_income_n5 = ntile(hhhead_income_month,5) * 0.2) # Quantiles
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(hhhead_income_n10 = ntile(hhhead_income_month,10) * 0.1) # Deciles
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(hhhead_income = ntile(hhhead_income_month,100) * 0.01) # Centiles
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(hhhead_income_n5 = ntile(hhhead_income_month,5) * 0.2) # Quantiles
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(hhhead_income_n10 = ntile(hhhead_income_month,10) * 0.1) # Deciles
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(hhhead_income = ntile(hhhead_income_month,100) * 0.01) # Centiles
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(hhhead_income_n5 = ntile(hhhead_income_month,5) * 0.2) # Quantiles
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(hhhead_income_n10 = ntile(hhhead_income_month,10) * 0.1) # Deciles
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(hhhead_income = ntile(hhhead_income_month,100) * 0.01) # Centiles

# Create sample quintiles, deciles and centiles for ALL VISITS (public & private)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(hhexp_n5 = ntile(hh_totalexpnf_month,5) * 0.2) # Quantiles
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(hhexp_n10 = ntile(hh_totalexpnf_month,10) * 0.1) # Deciles
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(hhexp = ntile(hh_totalexpnf_month,100) * 0.01) # Centiles
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(hhexp_n5 = ntile(hh_totalexpnf_month,5) * 0.2) # Quantiles
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(hhexp_n10 = ntile(hh_totalexpnf_month,10) * 0.1) # Deciles
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(hhexp = ntile(hh_totalexpnf_month,100) * 0.01) # Centiles
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(hhexp_n5 = ntile(hh_totalexpnf_month,5) * 0.2) # Quantiles
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(hhexp_n10 = ntile(hh_totalexpnf_month,10) * 0.1) # Deciles
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(hhexp = ntile(hh_totalexpnf_month,100) * 0.01) # Centiles
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(hhexp_n5 = ntile(hh_totalexpnf_month,5) * 0.2) # Quantiles
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(hhexp_n10 = ntile(hh_totalexpnf_month,10) * 0.1) # Deciles
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(hhexp = ntile(hh_totalexpnf_month,100) * 0.01) # Centiles
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(hhexp_n5 = ntile(hh_totalexpnf_month,5) * 0.2) # Quantiles
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(hhexp_n10 = ntile(hh_totalexpnf_month,10) * 0.1) # Deciles
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(hhexp = ntile(hh_totalexpnf_month,100) * 0.01) # Centiles
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(hhexp_n5 = ntile(hh_totalexpnf_month,5) * 0.2) # Quantiles
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(hhexp_n10 = ntile(hh_totalexpnf_month,10) * 0.1) # Deciles
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(hhexp = ntile(hh_totalexpnf_month,100) * 0.01) # Centiles

# Associate sample income with WB centiles
ugasoc_p_ipd <- mutate_inc_WBcentiles(ugasoc_p_ipd)
ugasoc_p_opd <- mutate_inc_WBcentiles(ugasoc_p_opd)
ugasoc_d_ipd <- mutate_inc_WBcentiles(ugasoc_d_ipd)
ugasoc_d_opd <- mutate_inc_WBcentiles(ugasoc_d_opd)
ugasoc_m_ipd <- mutate_inc_WBcentiles(ugasoc_m_ipd)
ugasoc_m_opd <- mutate_inc_WBcentiles(ugasoc_m_opd)

# Conversions WB centiles --> deciles & quantiles
ugasoc_p_ipd <- mutate_inc_WBdeciles(ugasoc_p_ipd)
ugasoc_p_opd <- mutate_inc_WBdeciles(ugasoc_p_opd)
ugasoc_d_ipd <- mutate_inc_WBdeciles(ugasoc_d_ipd)
ugasoc_d_opd <- mutate_inc_WBdeciles(ugasoc_d_opd)
ugasoc_m_ipd <- mutate_inc_WBdeciles(ugasoc_m_ipd)
ugasoc_m_opd <- mutate_inc_WBdeciles(ugasoc_m_opd)
ugasoc_p_ipd <- mutate_inc_WBquantiles(ugasoc_p_ipd)
ugasoc_p_opd <- mutate_inc_WBquantiles(ugasoc_p_opd)
ugasoc_d_ipd <- mutate_inc_WBquantiles(ugasoc_d_ipd)
ugasoc_d_opd <- mutate_inc_WBquantiles(ugasoc_d_opd)
ugasoc_m_ipd <- mutate_inc_WBquantiles(ugasoc_m_ipd)
ugasoc_m_opd <- mutate_inc_WBquantiles(ugasoc_m_opd)

# Associate sample expenses with WB centiles
ugasoc_p_ipd <- mutate_exp_WBcentiles(ugasoc_p_ipd)
ugasoc_p_opd <- mutate_exp_WBcentiles(ugasoc_p_opd)
ugasoc_d_ipd <- mutate_exp_WBcentiles(ugasoc_d_ipd)
ugasoc_d_opd <- mutate_exp_WBcentiles(ugasoc_d_opd)
ugasoc_m_ipd <- mutate_exp_WBcentiles(ugasoc_m_ipd)
ugasoc_m_opd <- mutate_exp_WBcentiles(ugasoc_m_opd)

# Conversions WB centiles --> deciles & quantiles
ugasoc_p_ipd <- mutate_exp_WBdeciles(ugasoc_p_ipd)
ugasoc_p_opd <- mutate_exp_WBdeciles(ugasoc_p_opd)
ugasoc_d_ipd <- mutate_exp_WBdeciles(ugasoc_d_ipd)
ugasoc_d_opd <- mutate_exp_WBdeciles(ugasoc_d_opd)
ugasoc_m_ipd <- mutate_exp_WBdeciles(ugasoc_m_ipd)
ugasoc_m_opd <- mutate_exp_WBdeciles(ugasoc_m_opd)
ugasoc_p_ipd <- mutate_exp_WBquantiles(ugasoc_p_ipd)
ugasoc_p_opd <- mutate_exp_WBquantiles(ugasoc_p_opd)
ugasoc_d_ipd <- mutate_exp_WBquantiles(ugasoc_d_ipd)
ugasoc_d_opd <- mutate_exp_WBquantiles(ugasoc_d_opd)
ugasoc_m_ipd <- mutate_exp_WBquantiles(ugasoc_m_ipd)
ugasoc_m_opd <- mutate_exp_WBquantiles(ugasoc_m_opd)

# Filter for public healthcare facilities only
public_p_ipd <- ugasoc_p_ipd %>% filter(sector==1)
public_p_opd <- ugasoc_p_opd %>% filter(sector==1)
public_d_ipd <- ugasoc_d_ipd %>% filter(sector==1)
public_d_opd <- ugasoc_d_opd %>% filter(sector==1)
public_m_ipd <- ugasoc_m_ipd %>% filter(sector==1)
public_m_opd <- ugasoc_m_opd %>% filter(sector==1)

# Create proportions of public expenditure on healthcare (gov)
public_p_ipd <- public_p_ipd %>% mutate(dmc_facility_prop = (dmc_facility + dmc_caregiver_c) / sum(dmc_facility + dmc_caregiver_c, na.rm = T))
public_p_opd <- public_p_opd %>% mutate(dmc_facility_prop = (dmc_facility + dmc_caregiver_c) / sum(dmc_facility + dmc_caregiver_c, na.rm = T))
public_d_ipd <- public_d_ipd %>% mutate(dmc_facility_prop = (dmc_facility + dmc_caregiver_c) / sum(dmc_facility + dmc_caregiver_c, na.rm = T))
public_d_opd <- public_d_opd %>% mutate(dmc_facility_prop = (dmc_facility + dmc_caregiver_c) / sum(dmc_facility + dmc_caregiver_c, na.rm = T))
public_m_ipd <- public_m_ipd %>% mutate(dmc_facility_prop = (dmc_facility + dmc_caregiver_c) / sum(dmc_facility + dmc_caregiver_c, na.rm = T))
public_m_opd <- public_m_opd %>% mutate(dmc_facility_prop = (dmc_facility + dmc_caregiver_c) / sum(dmc_facility + dmc_caregiver_c, na.rm = T))

# Create proportions of public expenditure on healthcare minus current OOP (net)
public_p_ipd <- public_p_ipd %>% mutate(dmc_facility = ifelse(dmc_facility<0,0,dmc_facility))
public_p_opd <- public_p_opd %>% mutate(dmc_facility = ifelse(dmc_facility<0,0,dmc_facility))
public_d_ipd <- public_d_ipd %>% mutate(dmc_facility = ifelse(dmc_facility<0,0,dmc_facility))
public_d_opd <- public_d_opd %>% mutate(dmc_facility = ifelse(dmc_facility<0,0,dmc_facility))
public_m_ipd <- public_m_ipd %>% mutate(dmc_facility = ifelse(dmc_facility<0,0,dmc_facility))
public_m_opd <- public_m_opd %>% mutate(dmc_facility = ifelse(dmc_facility<0,0,dmc_facility))

public_p_ipd <- public_p_ipd %>% mutate(dmc_facility_net_prop = dmc_facility / sum(dmc_facility, na.rm = T))
public_p_opd <- public_p_opd %>% mutate(dmc_facility_net_prop = dmc_facility / sum(dmc_facility, na.rm = T))
public_d_ipd <- public_d_ipd %>% mutate(dmc_facility_net_prop = dmc_facility / sum(dmc_facility, na.rm = T))
public_d_opd <- public_d_opd %>% mutate(dmc_facility_net_prop = dmc_facility / sum(dmc_facility, na.rm = T))
public_m_ipd <- public_m_ipd %>% mutate(dmc_facility_net_prop = dmc_facility / sum(dmc_facility, na.rm = T))
public_m_opd <- public_m_opd %>% mutate(dmc_facility_net_prop = dmc_facility / sum(dmc_facility, na.rm = T))

# Create proportions of public expenditure on healthcare minus all OOP (net2)
public_p_ipd <- public_p_ipd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba) / sum(dmc_facility - dmc_caregiver_ba, na.rm = T))
public_p_opd <- public_p_opd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba) / sum(dmc_facility - dmc_caregiver_ba, na.rm = T))
public_d_ipd <- public_d_ipd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba) / sum(dmc_facility - dmc_caregiver_ba, na.rm = T))
public_d_opd <- public_d_opd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba) / sum(dmc_facility - dmc_caregiver_ba, na.rm = T))
public_m_ipd <- public_m_ipd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba) / sum(dmc_facility - dmc_caregiver_ba, na.rm = T))
public_m_opd <- public_m_opd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba) / sum(dmc_facility - dmc_caregiver_ba, na.rm = T))

# Create proportions of public expenditure on healthcare minus all OOP & non-medical costs (net3)
public_p_ipd <- public_p_ipd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba - nmc_caregiver) / sum(dmc_facility - dmc_caregiver_ba - nmc_caregiver, na.rm = T))
public_p_opd <- public_p_opd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba - nmc_caregiver) / sum(dmc_facility - dmc_caregiver_ba - nmc_caregiver, na.rm = T))
public_d_ipd <- public_d_ipd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba - nmc_caregiver) / sum(dmc_facility - dmc_caregiver_ba - nmc_caregiver, na.rm = T))
public_d_opd <- public_d_opd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba - nmc_caregiver) / sum(dmc_facility - dmc_caregiver_ba - nmc_caregiver, na.rm = T))
public_m_ipd <- public_m_ipd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba - nmc_caregiver) / sum(dmc_facility - dmc_caregiver_ba - nmc_caregiver, na.rm = T))
public_m_opd <- public_m_opd %>% mutate(dmc_facility_net2_prop = (dmc_facility - dmc_caregiver_ba - nmc_caregiver) / sum(dmc_facility - dmc_caregiver_ba - nmc_caregiver, na.rm = T))

# Sum caregiver OOP (crg)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(dmc_caregiver = (dmc_caregiver_c + dmc_caregiver_ba))
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(dmc_caregiver = (dmc_caregiver_c + dmc_caregiver_ba))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(dmc_caregiver = (dmc_caregiver_c + dmc_caregiver_ba))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(dmc_caregiver = (dmc_caregiver_c + dmc_caregiver_ba))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(dmc_caregiver = (dmc_caregiver_c + dmc_caregiver_ba))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(dmc_caregiver = (dmc_caregiver_c + dmc_caregiver_ba))

# Create proportions of caregiver OOP (crg)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(dmc_caregiver_prop = dmc_caregiver / sum(dmc_caregiver, na.rm = T))
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(dmc_caregiver_prop = dmc_caregiver / sum(dmc_caregiver, na.rm = T))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(dmc_caregiver_prop = dmc_caregiver / sum(dmc_caregiver, na.rm = T))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(dmc_caregiver_prop = dmc_caregiver / sum(dmc_caregiver, na.rm = T))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(dmc_caregiver_prop = dmc_caregiver / sum(dmc_caregiver, na.rm = T))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(dmc_caregiver_prop = dmc_caregiver / sum(dmc_caregiver, na.rm = T))


# Sum caregiver OOP with non-medical costs (crg2)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(all_caregiver = (dmc_caregiver + nmc_caregiver))
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(all_caregiver = (dmc_caregiver + nmc_caregiver))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(all_caregiver = (dmc_caregiver + nmc_caregiver))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(all_caregiver = (dmc_caregiver + nmc_caregiver))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(all_caregiver = (dmc_caregiver + nmc_caregiver))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(all_caregiver = (dmc_caregiver + nmc_caregiver))

# Create proportions of caregiver OOP with non-medical costs (crg2)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(all_caregiver_prop = all_caregiver / sum(all_caregiver, na.rm = T))
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(all_caregiver_prop = all_caregiver / sum(all_caregiver, na.rm = T))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(all_caregiver_prop = all_caregiver / sum(all_caregiver, na.rm = T))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(all_caregiver_prop = all_caregiver / sum(all_caregiver, na.rm = T))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(all_caregiver_prop = all_caregiver / sum(all_caregiver, na.rm = T))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(all_caregiver_prop = all_caregiver / sum(all_caregiver, na.rm = T))


# Add row 0 for "No population" (origin of x-axis)
public_p_ipd <- add_row(public_p_ipd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0, dmc_facility_prop=0, dmc_facility_net_prop=0, dmc_facility_net2_prop=0)
public_p_opd <- add_row(public_p_opd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0, dmc_facility_prop=0, dmc_facility_net_prop=0, dmc_facility_net2_prop=0)
public_d_ipd <- add_row(public_d_ipd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0, dmc_facility_prop=0, dmc_facility_net_prop=0, dmc_facility_net2_prop=0)
public_d_opd <- add_row(public_d_opd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0, dmc_facility_prop=0, dmc_facility_net_prop=0, dmc_facility_net2_prop=0)
public_m_ipd <- add_row(public_m_ipd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0, dmc_facility_prop=0, dmc_facility_net_prop=0, dmc_facility_net2_prop=0)
public_m_opd <- add_row(public_m_opd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0, dmc_facility_prop=0, dmc_facility_net_prop=0, dmc_facility_net2_prop=0)
#public_p_ipd[nrow(public_p_ipd)+1,] <- 0
#public_p_opd[nrow(public_p_opd)+1,] <- 0
#public_d_ipd[nrow(public_d_ipd)+1,] <- 0
#public_d_opd[nrow(public_d_opd)+1,] <- 0
#public_m_ipd[nrow(public_m_ipd)+1,] <- 0
#public_m_opd[nrow(public_m_opd)+1,] <- 0


# Create proportions of head of household income (inc)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(inc_hhhead_prop = hhhead_income_month / sum(hhhead_income_month, na.rm = T))
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(inc_hhhead_prop = hhhead_income_month / sum(hhhead_income_month, na.rm = T))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(inc_hhhead_prop = hhhead_income_month / sum(hhhead_income_month, na.rm = T))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(inc_hhhead_prop = hhhead_income_month / sum(hhhead_income_month, na.rm = T))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(inc_hhhead_prop = hhhead_income_month / sum(hhhead_income_month, na.rm = T))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(inc_hhhead_prop = hhhead_income_month / sum(hhhead_income_month, na.rm = T))

# Create proportions of caregiver consumption (exp)
ugasoc_p_ipd <- ugasoc_p_ipd %>% mutate(exp_caregiver_prop = hh_totalexpnf_month / sum(hh_totalexpnf_month, na.rm = T))
ugasoc_p_opd <- ugasoc_p_opd %>% mutate(exp_caregiver_prop = hh_totalexpnf_month / sum(hh_totalexpnf_month, na.rm = T))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(exp_caregiver_prop = hh_totalexpnf_month / sum(hh_totalexpnf_month, na.rm = T))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(exp_caregiver_prop = hh_totalexpnf_month / sum(hh_totalexpnf_month, na.rm = T))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(exp_caregiver_prop = hh_totalexpnf_month / sum(hh_totalexpnf_month, na.rm = T))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(exp_caregiver_prop = hh_totalexpnf_month / sum(hh_totalexpnf_month, na.rm = T))

# Create proportions for hhincexp
hhincexp <- hhincexp %>% mutate(wb_inc_prop = income_USD / sum(income_USD, na.rm = T))
hhincexp <- hhincexp %>% mutate(wb_exp_prop = totalconsumption_USD / sum(totalconsumption_USD, na.rm = T))



# Bind healthcare expenditure variables (Adds 0 instead of NA)
full_join_NA <- function(x, y, ...) {
  full_join(x = x, y = y, by = ...) %>% 
    mutate_each(funs(replace(., which(is.na(.)), 0)))
}

################################################################################################
# Set up graph functions
################################################

# Graph benefit-incidence plots

graph_gov <- function(dataset,
                           title=paste0("PHE by ",write.graph1," Centile"),
                           subtitle=NULL,
                           label_y="Cumul. sum of total PHE (%)",
                           label_x=paste0(write.graph1," Centiles ",write.graph2),
                           ci=NULL,
                           ci2=NULL){
  ggplot(dataset) +
    geom_line(aes(x = Centile, y = cumsum(gov_prop)), color="black") +
    geom_point(aes(x = Centile, y = cumsum(gov_prop)), color="black") +
    geom_line(aes(x = Centile, y = cumsum(net_prop)), color="blue") +
    geom_point(aes(x = Centile, y = cumsum(net_prop)), color="blue") +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) +
    geom_abline(intercept = 0, slope = 1) +
    annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci," (gross benefit)"), color="black") +
    annotate("text", x = 0.25, y = 0.9, label = paste0("C = ",ci2," (net benefit)"), color="blue")
}
graph_crg <- function(dataset,
                           title=paste0("OOP by ",write.graph1," Centile"),
                           subtitle=NULL,
                           label_y="Cumul. sum of total OOP",
                           label_x=paste0(write.graph1," Centiles ",write.graph2),
                           ci=NULL,
                           ci2=NULL){
  ggplot(dataset, aes(x=Centile,y=crg_prop_cumul)) +
    geom_line(aes(x=Centile,y=crg_prop_cumul), color="firebrick4") +
    geom_point(aes(x=Centile,y=crg_prop_cumul), color="firebrick4") +
    geom_line(aes(x=Centile,y=crg2_prop_cumul), color="forestgreen") +
    geom_point(aes(x=Centile,y=crg2_prop_cumul), color="forestgreen") +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) +
    geom_abline(intercept = 0, slope = 1) +
    annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci," (medical costs only)"), color="firebrick4") +
    annotate("text", x = 0.25, y = 0.9, label = paste0("C = ",ci2," (all costs)"), color="forestgreen")
}
graph_time <- function(dataset,
                           title=paste0("Time Loss by ",write.graph1," Centile"),
                           subtitle=NULL,
                           label_y="Cumul. sum of total time (%)",
                           label_x=paste0(write.graph1," Centiles ",write.graph2),
                           ci=NULL){
  ggplot(dataset, aes(x=Centile,y=time_prop_cumul)) +
    geom_line(color="black") +
    geom_point(color="black") +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) +
    geom_abline(intercept = 0, slope = 1) +
    annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci))
}
graph_distance <- function(dataset,
                            title=paste0("Distance Travelled by ",write.graph1," Centile"),
                            subtitle=NULL,
                            label_y="Cumul. sum of total distance (%)",
                            label_x=paste0(write.graph1," Centiles ",write.graph2),
                            ci=NULL){
  ggplot(dataset, aes(x=Centile,y=distance_prop_cumul)) +
    geom_line(color="black") +
    geom_point(color="black") +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) +
    geom_abline(intercept = 0, slope = 1) +
    annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci))
}
graph_vc <- function(dataset,
                       title=paste0("Vaccine coverage by ",write.graph1," Centile"),
                       subtitle=NULL,
                       label_y="Cumulative Proportion of Children with Complete Immunization",
                       label_x=paste0(write.graph1," Centiles ",write.graph2),
                       ci=NULL){
  ggplot(dataset, aes(x=Centile,y=vc_prop_cumul)) +
    geom_line(color="black") +
    geom_point(color="black") +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) +
    geom_abline(intercept = 0, slope = 1) +
    annotate("text", x = 0.2, y = 1, label = paste0("C = ",ci))
}

graph_gini <- function(dataset,
                     title=paste0("Wealth distribution (",write.graph1,")"),
                     subtitle=NULL,
                     label_y="Cumulative Consumption",
                     label_x=paste0(write.graph1," Centiles ",write.graph2),
                     ci=NULL,
                     ci2=concentration_uga_hhincexp_exp){
ggplot(dataset, aes(x=Centile,y=exp_prop_cumul)) +
  geom_line(data = uga_hhincexp, aes(x=Centile,y=wb_exp_prop_cumul), color="gray40") +
  geom_line(data = dataset, aes(x=Centile,y=exp_prop_cumul), color="deeppink3") +
  geom_point(data = dataset, aes(x=Centile,y=exp_prop_cumul), color="deeppink3") +
  labs(title = title,
       subtitle = subtitle,
       y = label_y,
       x = label_x) +
  geom_abline(intercept = 0, slope = 1) +
  annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci," (sample)"), color="deeppink3") +
  annotate("text", x = 0.25, y = 0.9, label = paste0("C = ",ci2," (national)"), color="gray40")
}
graph_gini_crg <- function(dataset,
                      title="Healthcare expenditures & national consumption",
                      subtitle=NULL,
                      label_y="Cumulative Consumption",
                      label_x=paste0(write.graph1," Centiles ",write.graph2),
                      ci=NULL,
                      ci2=concentration_uga_hhincexp_exp){
  ggplot(dataset, aes(x=Centile,y=exp_prop_cumul)) +
    geom_line(data = uga_hhincexp, aes(x=Centile,y=wb_exp_prop_cumul), color="gray40") +
    geom_line(aes(x=Centile,y=crg2_prop_cumul), color="forestgreen") +
    geom_point(aes(x=Centile,y=crg2_prop_cumul), color="forestgreen") +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) +
    geom_abline(intercept = 0, slope = 1) +
    scale_y_continuous(sec.axis = dup_axis(name = "Cumul. sum of total OOP")) +
    annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci," (all costs)"), color="forestgreen") +
    annotate("text", x = 0.25, y = 0.9, label = paste0("C = ",ci2," (national)"), color="gray40")
}

# Frequency plots

graph_freq <- function(dataset,title=paste0("Count by ",write.graph1," Centile"),subtitle=NULL,label_y=NULL,label_x=paste0(write.graph1," Centile ",write.graph2)){
  ggplot(dataset, aes(if (user0==1 & user1==0){x = hhhead_income} else if (user0==1 & user1==1) {x = hhhead_income_wb} else if (user0==2 & user1==0) {x = hhexp} else {x = hhexp_wb})) +
    geom_histogram(bins = 20) +
    labs(title = title,
         subtitle = subtitle,
         y = label_y,
         x = label_x) #+
    #scale_x_discrete(limits = seq(0,1,0.2))
}



################################################
# GINI graphs & coefficients (World Bank)
################################################
# Population wealth distribution

# Sum proportion and cumulative amounts of healthcare spendings quantile/decile/centile
#ugasoc_hhincexp_inc <- aggregate(hhincexp$wb_inc_prop, by=list(Centile=hhincexp$exp_n100), FUN=sum)
#ugasoc_hhincexp_inc_amount <- aggregate(hhincexp$income_USD, by=list(Centile=hhincexp$exp_n100), FUN=sum)
#ugasoc_hhincexp_inc <- full_join_NA(x = ugasoc_hhincexp_inc, y = ugasoc_hhincexp_inc_amount, by = "Centile")
#ugasoc_hhincexp_inc <- if(1 %in% ugasoc_hhincexp_inc$Centile == F) {add_row(ugasoc_hhincexp_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_hhincexp_inc}
ugasoc_hhincexp_exp <- aggregate(hhincexp$wb_exp_prop, by=list(Centile=hhincexp$exp_n100), FUN=sum)
ugasoc_hhincexp_exp_amount <- aggregate(hhincexp$totalconsumption_USD, by=list(Centile=hhincexp$exp_n100), FUN=sum)
ugasoc_hhincexp_exp <- full_join_NA(x = ugasoc_hhincexp_exp, y = ugasoc_hhincexp_exp_amount, by = "Centile")
ugasoc_hhincexp_exp <- if(1 %in% ugasoc_hhincexp_exp$Centile == F) {add_row(ugasoc_hhincexp_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_hhincexp_exp}
#ugasoc_hhincexp_inc <- arrange(ugasoc_hhincexp_inc, Centile)
ugasoc_hhincexp_exp <- arrange(ugasoc_hhincexp_exp, Centile)

# Rename variables
#ugasoc_hhincexp_inc <- rename(ugasoc_hhincexp_inc, wb_inc_prop = x.x)
#ugasoc_hhincexp_inc <- rename(ugasoc_hhincexp_inc, wb_inc = x.y)
ugasoc_hhincexp_exp <- rename(ugasoc_hhincexp_exp, wb_exp_prop = x.x)
ugasoc_hhincexp_exp <- rename(ugasoc_hhincexp_exp, wb_exp = x.y)

# Bind healthcare expenditure variables (Adds 0 instead of NA)
#uga_hhincexp <- full_join_NA(ugasoc_hhincexp_inc, ugasoc_hhincexp_exp, by = "Centile")
uga_hhincexp <- ugasoc_hhincexp_exp
uga_hhincexp <- arrange(uga_hhincexp, Centile)

# Add cumulative sum
#uga_hhincexp <- mutate(uga_hhincexp, wb_inc_prop_cumul = cumsum(wb_inc_prop))
#uga_hhincexp <- mutate(uga_hhincexp, wb_inc_cumul = cumsum(wb_inc))
uga_hhincexp <- mutate(uga_hhincexp, wb_exp_prop_cumul = cumsum(wb_exp_prop))
uga_hhincexp <- mutate(uga_hhincexp, wb_exp_cumul = cumsum(wb_exp))

# Calculate concentration index
# Exclude observation 0
df_concentration_uga_hhincexp <- uga_hhincexp %>% filter(Centile != 0)


# Calculate
#concentration_uga_hhincexp_inc <- round((2/mean(df_concentration_uga_hhincexp$wb_inc)) * cov(df_concentration_uga_hhincexp$wb_inc, df_concentration_uga_hhincexp$Centile), digits = 4)
concentration_uga_hhincexp_exp <- round((2/mean(df_concentration_uga_hhincexp$wb_exp)) * cov(df_concentration_uga_hhincexp$wb_exp, df_concentration_uga_hhincexp$Centile), digits = 4)



#####
# Sample wealth distribution by disease and type of care

# Sum proportion and cumulative amounts of consumption centile
ugasoc_p_ipd_inc <- aggregate(ugasoc_p_ipd$inc_hhhead_prop, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_inc_amount <- aggregate(ugasoc_p_ipd$hhhead_income_month, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_inc <- full_join_NA(x = ugasoc_p_ipd_inc, y = ugasoc_p_ipd_inc_amount, by = "Centile")
ugasoc_p_ipd_inc <- if(1 %in% ugasoc_p_ipd_inc$Centile == F) {add_row(ugasoc_p_ipd_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_inc}
ugasoc_p_ipd_exp <- aggregate(ugasoc_p_ipd$exp_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_exp_amount <- aggregate(ugasoc_p_ipd$hh_totalexpnf_month, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_exp <- full_join_NA(x = ugasoc_p_ipd_exp, y = ugasoc_p_ipd_exp_amount, by = "Centile")
ugasoc_p_ipd_exp <- if(1 %in% ugasoc_p_ipd_exp$Centile == F) {add_row(ugasoc_p_ipd_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_exp}
ugasoc_p_ipd_inc <- arrange(ugasoc_p_ipd_inc, Centile)
ugasoc_p_ipd_exp <- arrange(ugasoc_p_ipd_exp, Centile)
ugasoc_p_opd_inc <- aggregate(ugasoc_p_opd$inc_hhhead_prop, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_inc_amount <- aggregate(ugasoc_p_opd$hhhead_income_month, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_inc <- full_join_NA(x = ugasoc_p_opd_inc, y = ugasoc_p_opd_inc_amount, by = "Centile")
ugasoc_p_opd_inc <- if(1 %in% ugasoc_p_opd_inc$Centile == F) {add_row(ugasoc_p_opd_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_inc}
ugasoc_p_opd_exp <- aggregate(ugasoc_p_opd$exp_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_exp_amount <- aggregate(ugasoc_p_opd$hh_totalexpnf_month, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_exp <- full_join_NA(x = ugasoc_p_opd_exp, y = ugasoc_p_opd_exp_amount, by = "Centile")
ugasoc_p_opd_exp <- if(1 %in% ugasoc_p_opd_exp$Centile == F) {add_row(ugasoc_p_opd_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_exp}
ugasoc_p_opd_inc <- arrange(ugasoc_p_opd_inc, Centile)
ugasoc_p_opd_exp <- arrange(ugasoc_p_opd_exp, Centile)

ugasoc_d_ipd_inc <- aggregate(ugasoc_d_ipd$inc_hhhead_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_inc_amount <- aggregate(ugasoc_d_ipd$hhhead_income_month, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_inc <- full_join_NA(x = ugasoc_d_ipd_inc, y = ugasoc_d_ipd_inc_amount, by = "Centile")
ugasoc_d_ipd_inc <- if(1 %in% ugasoc_d_ipd_inc$Centile == F) {add_row(ugasoc_d_ipd_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_inc}
ugasoc_d_ipd_exp <- aggregate(ugasoc_d_ipd$exp_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_exp_amount <- aggregate(ugasoc_d_ipd$hh_totalexpnf_month, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_exp <- full_join_NA(x = ugasoc_d_ipd_exp, y = ugasoc_d_ipd_exp_amount, by = "Centile")
ugasoc_d_ipd_exp <- if(1 %in% ugasoc_d_ipd_exp$Centile == F) {add_row(ugasoc_d_ipd_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_exp}
ugasoc_d_ipd_inc <- arrange(ugasoc_d_ipd_inc, Centile)
ugasoc_d_ipd_exp <- arrange(ugasoc_d_ipd_exp, Centile)
ugasoc_d_opd_inc <- aggregate(ugasoc_d_opd$inc_hhhead_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_inc_amount <- aggregate(ugasoc_d_opd$hhhead_income_month, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_inc <- full_join_NA(x = ugasoc_d_opd_inc, y = ugasoc_d_opd_inc_amount, by = "Centile")
ugasoc_d_opd_inc <- if(1 %in% ugasoc_d_opd_inc$Centile == F) {add_row(ugasoc_d_opd_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_inc}
ugasoc_d_opd_exp <- aggregate(ugasoc_d_opd$exp_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_exp_amount <- aggregate(ugasoc_d_opd$hh_totalexpnf_month, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_exp <- full_join_NA(x = ugasoc_d_opd_exp, y = ugasoc_d_opd_exp_amount, by = "Centile")
ugasoc_d_opd_exp <- if(1 %in% ugasoc_d_opd_exp$Centile == F) {add_row(ugasoc_d_opd_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_exp}
ugasoc_d_opd_inc <- arrange(ugasoc_d_opd_inc, Centile)
ugasoc_d_opd_exp <- arrange(ugasoc_d_opd_exp, Centile)

ugasoc_m_ipd_inc <- aggregate(ugasoc_m_ipd$inc_hhhead_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_inc_amount <- aggregate(ugasoc_m_ipd$hhhead_income_month, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_inc <- full_join_NA(x = ugasoc_m_ipd_inc, y = ugasoc_m_ipd_inc_amount, by = "Centile")
ugasoc_m_ipd_inc <- if(1 %in% ugasoc_m_ipd_inc$Centile == F) {add_row(ugasoc_m_ipd_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_inc}
ugasoc_m_ipd_exp <- aggregate(ugasoc_m_ipd$exp_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_exp_amount <- aggregate(ugasoc_m_ipd$hh_totalexpnf_month, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_exp <- full_join_NA(x = ugasoc_m_ipd_exp, y = ugasoc_m_ipd_exp_amount, by = "Centile")
ugasoc_m_ipd_exp <- if(1 %in% ugasoc_m_ipd_exp$Centile == F) {add_row(ugasoc_m_ipd_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_exp}
ugasoc_m_ipd_inc <- arrange(ugasoc_m_ipd_inc, Centile)
ugasoc_m_ipd_exp <- arrange(ugasoc_m_ipd_exp, Centile)
ugasoc_m_opd_inc <- aggregate(ugasoc_m_opd$inc_hhhead_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_inc_amount <- aggregate(ugasoc_m_opd$hhhead_income_month, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_inc <- full_join_NA(x = ugasoc_m_opd_inc, y = ugasoc_m_opd_inc_amount, by = "Centile")
ugasoc_m_opd_inc <- if(1 %in% ugasoc_m_opd_inc$Centile == F) {add_row(ugasoc_m_opd_inc, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_inc}
ugasoc_m_opd_exp <- aggregate(ugasoc_m_opd$exp_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_exp_amount <- aggregate(ugasoc_m_opd$hh_totalexpnf_month, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_exp <- full_join_NA(x = ugasoc_m_opd_exp, y = ugasoc_m_opd_exp_amount, by = "Centile")
ugasoc_m_opd_exp <- if(1 %in% ugasoc_m_opd_exp$Centile == F) {add_row(ugasoc_m_opd_exp, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_exp}
ugasoc_m_opd_inc <- arrange(ugasoc_m_opd_inc, Centile)
ugasoc_m_opd_exp <- arrange(ugasoc_m_opd_exp, Centile)


# Sum proportion and cumulative amounts of healthcare spendings centile
ugasoc_p_ipd_crg2 <- aggregate(ugasoc_p_ipd$all_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_crg2_amount <- aggregate(ugasoc_p_ipd$all_caregiver, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_crg2 <- full_join_NA(x = ugasoc_p_ipd_crg2, y = ugasoc_p_ipd_crg2_amount, by = "Centile")
ugasoc_p_ipd_crg2 <- if(1 %in% ugasoc_p_ipd_crg2$Centile == F) {add_row(ugasoc_p_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg2}
ugasoc_p_ipd_crg2 <- arrange(ugasoc_p_ipd_crg2, Centile)

ugasoc_p_opd_crg2 <- aggregate(ugasoc_p_opd$all_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_crg2_amount <- aggregate(ugasoc_p_opd$all_caregiver, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_crg2 <- full_join_NA(x = ugasoc_p_opd_crg2, y = ugasoc_p_opd_crg2_amount, by = "Centile")
ugasoc_p_opd_crg2 <- if(1 %in% ugasoc_p_opd_crg2$Centile == F) {add_row(ugasoc_p_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg2}
ugasoc_p_opd_crg2 <- arrange(ugasoc_p_opd_crg2, Centile)

ugasoc_d_ipd_crg2 <- aggregate(ugasoc_d_ipd$all_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_crg2_amount <- aggregate(ugasoc_d_ipd$all_caregiver, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_crg2 <- full_join_NA(x = ugasoc_d_ipd_crg2, y = ugasoc_d_ipd_crg2_amount, by = "Centile")
ugasoc_d_ipd_crg2 <- if(1 %in% ugasoc_d_ipd_crg2$Centile == F) {add_row(ugasoc_d_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg2}
ugasoc_d_ipd_crg2 <- arrange(ugasoc_d_ipd_crg2, Centile)

ugasoc_d_opd_crg2 <- aggregate(ugasoc_d_opd$all_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_crg2_amount <- aggregate(ugasoc_d_opd$all_caregiver, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_crg2 <- full_join_NA(x = ugasoc_d_opd_crg2, y = ugasoc_d_opd_crg2_amount, by = "Centile")
ugasoc_d_opd_crg2 <- if(1 %in% ugasoc_d_opd_crg2$Centile == F) {add_row(ugasoc_d_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg2}
ugasoc_d_opd_crg2 <- arrange(ugasoc_d_opd_crg2, Centile)

ugasoc_m_ipd_crg2 <- aggregate(ugasoc_m_ipd$all_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_crg2_amount <- aggregate(ugasoc_m_ipd$all_caregiver, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_crg2 <- full_join_NA(x = ugasoc_m_ipd_crg2, y = ugasoc_m_ipd_crg2_amount, by = "Centile")
ugasoc_m_ipd_crg2 <- if(1 %in% ugasoc_m_ipd_crg2$Centile == F) {add_row(ugasoc_m_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg2}
ugasoc_m_ipd_crg2 <- arrange(ugasoc_m_ipd_crg2, Centile)

ugasoc_m_opd_crg2 <- aggregate(ugasoc_m_opd$all_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_crg2_amount <- aggregate(ugasoc_m_opd$all_caregiver, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_crg2 <- full_join_NA(x = ugasoc_m_opd_crg2, y = ugasoc_m_opd_crg2_amount, by = "Centile")
ugasoc_m_opd_crg2 <- if(1 %in% ugasoc_m_opd_crg2$Centile == F) {add_row(ugasoc_m_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg2}
ugasoc_m_opd_crg2 <- arrange(ugasoc_m_opd_crg2, Centile)


# Rename variables
ugasoc_p_ipd_inc <- rename(ugasoc_p_ipd_inc, inc_prop = x.x)
ugasoc_p_ipd_inc <- rename(ugasoc_p_ipd_inc, inc = x.y)
ugasoc_p_ipd_exp <- rename(ugasoc_p_ipd_exp, exp_prop = x.x)
ugasoc_p_ipd_exp <- rename(ugasoc_p_ipd_exp, exp = x.y)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2_prop = x.x)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2 = x.y)
ugasoc_p_opd_inc <- rename(ugasoc_p_opd_inc, inc_prop = x.x)
ugasoc_p_opd_inc <- rename(ugasoc_p_opd_inc, inc = x.y)
ugasoc_p_opd_exp <- rename(ugasoc_p_opd_exp, exp_prop = x.x)
ugasoc_p_opd_exp <- rename(ugasoc_p_opd_exp, exp = x.y)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2_prop = x.x)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2 = x.y)

ugasoc_d_ipd_inc <- rename(ugasoc_d_ipd_inc, inc_prop = x.x)
ugasoc_d_ipd_inc <- rename(ugasoc_d_ipd_inc, inc = x.y)
ugasoc_d_ipd_exp <- rename(ugasoc_d_ipd_exp, exp_prop = x.x)
ugasoc_d_ipd_exp <- rename(ugasoc_d_ipd_exp, exp = x.y)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2_prop = x.x)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2 = x.y)
ugasoc_d_opd_inc <- rename(ugasoc_d_opd_inc, inc_prop = x.x)
ugasoc_d_opd_inc <- rename(ugasoc_d_opd_inc, inc = x.y)
ugasoc_d_opd_exp <- rename(ugasoc_d_opd_exp, exp_prop = x.x)
ugasoc_d_opd_exp <- rename(ugasoc_d_opd_exp, exp = x.y)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2_prop = x.x)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2 = x.y)

ugasoc_m_ipd_inc <- rename(ugasoc_m_ipd_inc, inc_prop = x.x)
ugasoc_m_ipd_inc <- rename(ugasoc_m_ipd_inc, inc = x.y)
ugasoc_m_ipd_exp <- rename(ugasoc_m_ipd_exp, exp_prop = x.x)
ugasoc_m_ipd_exp <- rename(ugasoc_m_ipd_exp, exp = x.y)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2_prop = x.x)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2 = x.y)
ugasoc_m_opd_inc <- rename(ugasoc_m_opd_inc, inc_prop = x.x)
ugasoc_m_opd_inc <- rename(ugasoc_m_opd_inc, inc = x.y)
ugasoc_m_opd_exp <- rename(ugasoc_m_opd_exp, exp_prop = x.x)
ugasoc_m_opd_exp <- rename(ugasoc_m_opd_exp, exp = x.y)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2_prop = x.x)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2 = x.y)


# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(ugasoc_p_ipd_inc, ugasoc_p_ipd_exp, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg2, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)
uga_p_opd <- full_join_NA(ugasoc_p_opd_inc, ugasoc_p_opd_exp, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg2, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(ugasoc_d_ipd_inc, ugasoc_d_ipd_exp, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg2, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)
uga_d_opd <- full_join_NA(ugasoc_d_opd_inc, ugasoc_d_opd_exp, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg2, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(ugasoc_m_ipd_inc, ugasoc_m_ipd_exp, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg2, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)
uga_m_opd <- full_join_NA(ugasoc_m_opd_inc, ugasoc_m_opd_exp, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg2, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)


# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, inc_prop_cumul = cumsum(inc_prop))
uga_p_ipd <- mutate(uga_p_ipd, inc_cumul = cumsum(inc))
uga_p_ipd <- mutate(uga_p_ipd, exp_prop_cumul = cumsum(exp_prop))
uga_p_ipd <- mutate(uga_p_ipd, exp_cumul = cumsum(exp))
uga_p_ipd <- mutate(uga_p_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg2_cumul = cumsum(crg2))
uga_p_opd <- mutate(uga_p_opd, inc_prop_cumul = cumsum(inc_prop))
uga_p_opd <- mutate(uga_p_opd, inc_cumul = cumsum(inc))
uga_p_opd <- mutate(uga_p_opd, exp_prop_cumul = cumsum(exp_prop))
uga_p_opd <- mutate(uga_p_opd, exp_cumul = cumsum(exp))
uga_p_opd <- mutate(uga_p_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_opd <- mutate(uga_p_opd, crg2_cumul = cumsum(crg2))

uga_d_ipd <- mutate(uga_d_ipd, inc_prop_cumul = cumsum(inc_prop))
uga_d_ipd <- mutate(uga_d_ipd, inc_cumul = cumsum(inc))
uga_d_ipd <- mutate(uga_d_ipd, exp_prop_cumul = cumsum(exp_prop))
uga_d_ipd <- mutate(uga_d_ipd, exp_cumul = cumsum(exp))
uga_d_ipd <- mutate(uga_d_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg2_cumul = cumsum(crg2))
uga_d_opd <- mutate(uga_d_opd, inc_prop_cumul = cumsum(inc_prop))
uga_d_opd <- mutate(uga_d_opd, inc_cumul = cumsum(inc))
uga_d_opd <- mutate(uga_d_opd, exp_prop_cumul = cumsum(exp_prop))
uga_d_opd <- mutate(uga_d_opd, exp_cumul = cumsum(exp))
uga_d_opd <- mutate(uga_d_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_opd <- mutate(uga_d_opd, crg2_cumul = cumsum(crg2))

uga_m_ipd <- mutate(uga_m_ipd, inc_prop_cumul = cumsum(inc_prop))
uga_m_ipd <- mutate(uga_m_ipd, inc_cumul = cumsum(inc))
uga_m_ipd <- mutate(uga_m_ipd, exp_prop_cumul = cumsum(exp_prop))
uga_m_ipd <- mutate(uga_m_ipd, exp_cumul = cumsum(exp))
uga_m_ipd <- mutate(uga_m_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg2_cumul = cumsum(crg2))
uga_m_opd <- mutate(uga_m_opd, inc_prop_cumul = cumsum(inc_prop))
uga_m_opd <- mutate(uga_m_opd, inc_cumul = cumsum(inc))
uga_m_opd <- mutate(uga_m_opd, exp_prop_cumul = cumsum(exp_prop))
uga_m_opd <- mutate(uga_m_opd, exp_cumul = cumsum(exp))
uga_m_opd <- mutate(uga_m_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_opd <- mutate(uga_m_opd, crg2_cumul = cumsum(crg2))



# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)


# Calculate
concentration_uga_inc_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$inc)) * cov(df_concentration_uga_p_ipd$inc, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_inc_p_opd <- round((2/mean(df_concentration_uga_p_opd$inc)) * cov(df_concentration_uga_p_opd$inc, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_inc_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$inc)) * cov(df_concentration_uga_d_ipd$inc, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_inc_d_opd <- round((2/mean(df_concentration_uga_d_opd$inc)) * cov(df_concentration_uga_d_opd$inc, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_inc_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$inc)) * cov(df_concentration_uga_m_ipd$inc, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_inc_m_opd <- round((2/mean(df_concentration_uga_m_opd$inc)) * cov(df_concentration_uga_m_opd$inc, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_exp_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$exp)) * cov(df_concentration_uga_p_ipd$exp, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_exp_p_opd <- round((2/mean(df_concentration_uga_p_opd$exp)) * cov(df_concentration_uga_p_opd$exp, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_exp_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$exp)) * cov(df_concentration_uga_d_ipd$exp, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_exp_d_opd <- round((2/mean(df_concentration_uga_d_opd$exp)) * cov(df_concentration_uga_d_opd$exp, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_exp_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$exp)) * cov(df_concentration_uga_m_ipd$exp, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_exp_m_opd <- round((2/mean(df_concentration_uga_m_opd$exp)) * cov(df_concentration_uga_m_opd$exp, df_concentration_uga_m_opd$Centile), digits = 4)


concentration_uga_crg2_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg2)) * cov(df_concentration_uga_p_ipd$crg2, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg2_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg2)) * cov(df_concentration_uga_p_opd$crg2, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg2_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg2)) * cov(df_concentration_uga_d_ipd$crg2, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg2_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg2)) * cov(df_concentration_uga_d_opd$crg2, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg2_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg2)) * cov(df_concentration_uga_m_ipd$crg2, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg2_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg2)) * cov(df_concentration_uga_m_opd$crg2, df_concentration_uga_m_opd$Centile), digits = 4)


#####

# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
income <- c(concentration_uga_inc_p_ipd, concentration_uga_inc_p_opd, concentration_uga_inc_d_ipd, concentration_uga_inc_d_opd, concentration_uga_inc_m_ipd, concentration_uga_inc_m_opd)
#income_wb <- c(concentration_uga_hhincexp_inc,concentration_uga_hhincexp_inc,concentration_uga_hhincexp_inc,concentration_uga_hhincexp_inc,concentration_uga_hhincexp_inc,concentration_uga_hhincexp_inc)
consumption <- c(concentration_uga_exp_p_ipd, concentration_uga_exp_p_opd, concentration_uga_exp_d_ipd, concentration_uga_exp_d_opd, concentration_uga_exp_m_ipd, concentration_uga_exp_m_opd)
consumption_wb <- c(concentration_uga_hhincexp_exp,concentration_uga_hhincexp_exp,concentration_uga_hhincexp_exp,concentration_uga_hhincexp_exp,concentration_uga_hhincexp_exp,concentration_uga_hhincexp_exp)
oop_all <- c(concentration_uga_crg2_p_ipd, concentration_uga_crg2_p_opd, concentration_uga_crg2_d_ipd, concentration_uga_crg2_d_opd, concentration_uga_crg2_m_ipd, concentration_uga_crg2_m_opd)
wealth_distrib <- c('consumption', 'consumption', 'consumption', 'consumption', 'consumption', 'consumption')
wealth_source <- c('wb', 'wb', 'wb', 'wb', 'wb', 'wb')
gini_uga <- data.frame(disease, visittype, income, consumption, consumption_wb, oop_all, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
write.estimate <- "gini_"
write.graph1 <- "Consumption"
write.graph2 <- "(National)"


# Graph GINI
graph_p_ipd_gini <- graph_gini(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_exp_p_ipd)
graph_p_opd_gini <- graph_gini(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_exp_p_opd)
graph_d_ipd_gini <- graph_gini(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_exp_d_ipd)
graph_d_opd_gini <- graph_gini(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_exp_d_opd)
graph_m_ipd_gini <- graph_gini(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_exp_m_ipd)
graph_m_opd_gini <- graph_gini(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_exp_m_opd)


jpeg(filename = paste0("graph_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gini,graph_d_ipd_gini,graph_m_ipd_gini,graph_p_opd_gini,graph_d_opd_gini,graph_m_opd_gini)
dev.off()


# Meta data
write.estimate <- "gini_oop_"
write.graph1 <- "Consumption"
write.graph2 <- "(National)"


# Graph GINI + OOP
graph_p_ipd_gini_crg <- graph_gini_crg(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_crg2_p_ipd, ci2 = )
graph_p_opd_gini_crg <- graph_gini_crg(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_crg2_p_opd, ci2 = )
graph_d_ipd_gini_crg <- graph_gini_crg(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_crg2_d_ipd, ci2 = )
graph_d_opd_gini_crg <- graph_gini_crg(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_crg2_d_opd, ci2 = )
graph_m_ipd_gini_crg <- graph_gini_crg(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_crg2_m_ipd, ci2 = )
graph_m_opd_gini_crg <- graph_gini_crg(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_crg2_m_opd, ci2 = )


jpeg(filename = paste0("graph_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gini_crg,graph_d_ipd_gini_crg,graph_m_ipd_gini_crg,graph_p_opd_gini_crg,graph_d_opd_gini_crg,graph_m_opd_gini_crg)
dev.off()


# Show both income and consumption over consumption SES
ci = concentration_uga_inc_p_ipd
ci2 = concentration_uga_exp_p_ipd
ggplot(uga_p_ipd, aes(x=Centile,y=inc_prop_cumul)) +
  geom_line(aes(x=Centile,y=inc_prop_cumul), color="lightslateblue") +
  geom_point(aes(x=Centile,y=inc_prop_cumul), color="lightslateblue") +
  geom_line(aes(x=Centile,y=exp_prop_cumul), color="olivedrab") +
  geom_point(aes(x=Centile,y=exp_prop_cumul), color="olivedrab") +
  labs(title = "title",
       subtitle = "subtitle",
       y = "label_y",
       x = "label_x") +
  geom_abline(intercept = 0, slope = 1) +
  annotate("text", x = 0.25, y = 1, label = paste0("C = ",ci," (income)"), color="lightslateblue") +
  annotate("text", x = 0.25, y = 0.9, label = paste0("C = ",ci2," (consumption)"), color="olivedrab")



### Regress disease and World Bank estimates

# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- left_join(uga_p_ipd, uga_hhincexp, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)
uga_p_opd <- left_join(uga_p_opd, uga_hhincexp, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)
uga_d_ipd <- left_join(uga_d_ipd, uga_hhincexp, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)
uga_d_opd <- left_join(uga_d_opd, uga_hhincexp, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)
uga_m_ipd <- left_join(uga_m_ipd, uga_hhincexp, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)
uga_m_opd <- left_join(uga_m_opd, uga_hhincexp, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)

# Regress
lm_exp_p_ipd <- lm(formula = log(wb_exp) ~ exp, data = uga_p_ipd)
summary(lm_exp_p_ipd)
AIC(lm_exp_p_ipd)
lm_exp_p_opd <- lm(formula = log(wb_exp) ~ exp, data = uga_p_opd)
summary(lm_exp_p_opd)
AIC(lm_exp_p_opd)
lm_exp_d_ipd <- lm(formula = log(wb_exp) ~ exp, data = uga_d_ipd)
summary(lm_exp_d_ipd)
AIC(lm_exp_d_ipd)
lm_exp_d_opd <- lm(formula = log(wb_exp) ~ exp, data = uga_d_opd)
summary(lm_exp_d_opd)
AIC(lm_exp_d_opd)
lm_exp_m_ipd <- lm(formula = log(wb_exp) ~ exp, data = uga_m_ipd)
summary(lm_exp_m_ipd)
AIC(lm_exp_m_ipd)
lm_exp_m_opd <- lm(formula = log(wb_exp) ~ exp, data = uga_m_opd)
summary(lm_exp_m_opd)
AIC(lm_exp_m_opd)


################################################################################################
# National estimates of public health expenditures (PHE) and out-of-pocket (OOP)
################################################

################################################
# PHE & OOP based on INCOME (sample)
################################################

print("Using sample estimates")
user0 <- 1
user1 <- 0


# Sum proportion and cumulative amounts of healthcare spendings by nationally representative (WB) quantile/decile/centile
public_p_ipd_gov <- aggregate(public_p_ipd$dmc_facility_prop, by=list(Centile=public_p_ipd$hhhead_income), FUN=sum)
public_p_ipd_gov_amount <- aggregate((public_p_ipd$dmc_facility + public_p_ipd$dmc_caregiver_c), by=list(Centile=public_p_ipd$hhhead_income), FUN=sum)
public_p_ipd_gov <- full_join_NA(x = public_p_ipd_gov, y = public_p_ipd_gov_amount, by = "Centile")
public_p_ipd_gov <- if(1 %in% public_p_ipd_gov$Centile == F) {add_row(public_p_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_gov}
public_p_ipd_net <- aggregate(public_p_ipd$dmc_facility_net_prop, by=list(Centile=public_p_ipd$hhhead_income), FUN=sum)
public_p_ipd_net_amount <- aggregate(public_p_ipd$dmc_facility, by=list(Centile=public_p_ipd$hhhead_income), FUN=sum)
public_p_ipd_net <- full_join_NA(x = public_p_ipd_net, y = public_p_ipd_net_amount, by = "Centile")
public_p_ipd_net <- if(1 %in% public_p_ipd_net$Centile == F) {add_row(public_p_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net}
public_p_ipd_net2 <- aggregate(public_p_ipd$dmc_facility_net2_prop, by=list(Centile=public_p_ipd$hhhead_income), FUN=sum)
public_p_ipd_net2_amount <- aggregate((public_p_ipd$dmc_facility - public_p_ipd$dmc_caregiver_ba), by=list(Centile=public_p_ipd$hhhead_income), FUN=sum)
public_p_ipd_net2 <- full_join_NA(x = public_p_ipd_net2, y = public_p_ipd_net2_amount, by = "Centile")
public_p_ipd_net2 <- if(1 %in% public_p_ipd_net2$Centile == F) {add_row(public_p_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net2}
ugasoc_p_ipd_crg <- aggregate(ugasoc_p_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhhead_income), FUN=sum)
ugasoc_p_ipd_crg_amount <- aggregate(ugasoc_p_ipd$dmc_caregiver, by=list(Centile=ugasoc_p_ipd$hhhead_income), FUN=sum)
ugasoc_p_ipd_crg <- full_join_NA(x = ugasoc_p_ipd_crg, y = ugasoc_p_ipd_crg_amount, by = "Centile")
ugasoc_p_ipd_crg <- if(1 %in% ugasoc_p_ipd_crg$Centile == F) {add_row(ugasoc_p_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg}
ugasoc_p_ipd_crg2 <- aggregate(ugasoc_p_ipd$all_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhhead_income), FUN=sum)
ugasoc_p_ipd_crg2_amount <- aggregate(ugasoc_p_ipd$all_caregiver, by=list(Centile=ugasoc_p_ipd$hhhead_income), FUN=sum)
ugasoc_p_ipd_crg2 <- full_join_NA(x = ugasoc_p_ipd_crg2, y = ugasoc_p_ipd_crg2_amount, by = "Centile")
ugasoc_p_ipd_crg2 <- if(1 %in% ugasoc_p_ipd_crg2$Centile == F) {add_row(ugasoc_p_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg2}
public_p_ipd_gov <- arrange(public_p_ipd_gov, Centile)
public_p_ipd_net <- arrange(public_p_ipd_net, Centile)
public_p_ipd_net2 <- arrange(public_p_ipd_net2, Centile)
ugasoc_p_ipd_crg <- arrange(ugasoc_p_ipd_crg, Centile)
ugasoc_p_ipd_crg2 <- arrange(ugasoc_p_ipd_crg2, Centile)

public_p_opd_gov <- aggregate(public_p_opd$dmc_facility_prop, by=list(Centile=public_p_opd$hhhead_income), FUN=sum)
public_p_opd_gov_amount <- aggregate((public_p_opd$dmc_facility + public_p_opd$dmc_caregiver_c), by=list(Centile=public_p_opd$hhhead_income), FUN=sum)
public_p_opd_gov <- full_join_NA(x = public_p_opd_gov, y = public_p_opd_gov_amount, by = "Centile")
public_p_opd_gov <- if(1 %in% public_p_opd_gov$Centile == F) {add_row(public_p_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_gov}
public_p_opd_net <- aggregate(public_p_opd$dmc_facility_net_prop, by=list(Centile=public_p_opd$hhhead_income), FUN=sum)
public_p_opd_net_amount <- aggregate(public_p_opd$dmc_facility, by=list(Centile=public_p_opd$hhhead_income), FUN=sum)
public_p_opd_net <- full_join_NA(x = public_p_opd_net, y = public_p_opd_net_amount, by = "Centile")
public_p_opd_net <- if(1 %in% public_p_opd_net$Centile == F) {add_row(public_p_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net}
public_p_opd_net2 <- aggregate(public_p_opd$dmc_facility_net2_prop, by=list(Centile=public_p_opd$hhhead_income), FUN=sum)
public_p_opd_net2_amount <- aggregate((public_p_opd$dmc_facility - public_p_opd$dmc_caregiver_ba), by=list(Centile=public_p_opd$hhhead_income), FUN=sum)
public_p_opd_net2 <- full_join_NA(x = public_p_opd_net2, y = public_p_opd_net2_amount, by = "Centile")
public_p_opd_net2 <- if(1 %in% public_p_opd_net2$Centile == F) {add_row(public_p_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net2}
ugasoc_p_opd_crg <- aggregate(ugasoc_p_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhhead_income), FUN=sum)
ugasoc_p_opd_crg_amount <- aggregate(ugasoc_p_opd$dmc_caregiver, by=list(Centile=ugasoc_p_opd$hhhead_income), FUN=sum)
ugasoc_p_opd_crg <- full_join_NA(x = ugasoc_p_opd_crg, y = ugasoc_p_opd_crg_amount, by = "Centile")
ugasoc_p_opd_crg <- if(1 %in% ugasoc_p_opd_crg$Centile == F) {add_row(ugasoc_p_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg}
ugasoc_p_opd_crg2 <- aggregate(ugasoc_p_opd$all_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhhead_income), FUN=sum)
ugasoc_p_opd_crg2_amount <- aggregate(ugasoc_p_opd$all_caregiver, by=list(Centile=ugasoc_p_opd$hhhead_income), FUN=sum)
ugasoc_p_opd_crg2 <- full_join_NA(x = ugasoc_p_opd_crg2, y = ugasoc_p_opd_crg2_amount, by = "Centile")
ugasoc_p_opd_crg2 <- if(1 %in% ugasoc_p_opd_crg2$Centile == F) {add_row(ugasoc_p_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg2}
public_p_opd_gov <- arrange(public_p_opd_gov, Centile)
public_p_opd_net <- arrange(public_p_opd_net, Centile)
public_p_opd_net2 <- arrange(public_p_opd_net2, Centile)
ugasoc_p_opd_crg <- arrange(ugasoc_p_opd_crg, Centile)
ugasoc_p_opd_crg2 <- arrange(ugasoc_p_opd_crg2, Centile)

public_d_ipd_gov <- aggregate(public_d_ipd$dmc_facility_prop, by=list(Centile=public_d_ipd$hhhead_income), FUN=sum)
public_d_ipd_gov_amount <- aggregate((public_d_ipd$dmc_facility + public_d_ipd$dmc_caregiver_c), by=list(Centile=public_d_ipd$hhhead_income), FUN=sum)
public_d_ipd_gov <- full_join_NA(x = public_d_ipd_gov, y = public_d_ipd_gov_amount, by = "Centile")
public_d_ipd_gov <- if(1 %in% public_d_ipd_gov$Centile == F) {add_row(public_d_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_gov}
public_d_ipd_net <- aggregate(public_d_ipd$dmc_facility_net_prop, by=list(Centile=public_d_ipd$hhhead_income), FUN=sum)
public_d_ipd_net_amount <- aggregate(public_d_ipd$dmc_facility, by=list(Centile=public_d_ipd$hhhead_income), FUN=sum)
public_d_ipd_net <- full_join_NA(x = public_d_ipd_net, y = public_d_ipd_net_amount, by = "Centile")
public_d_ipd_net <- if(1 %in% public_d_ipd_net$Centile == F) {add_row(public_d_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net}
public_d_ipd_net2 <- aggregate(public_d_ipd$dmc_facility_net2_prop, by=list(Centile=public_d_ipd$hhhead_income), FUN=sum)
public_d_ipd_net2_amount <- aggregate((public_d_ipd$dmc_facility - public_d_ipd$dmc_caregiver_ba), by=list(Centile=public_d_ipd$hhhead_income), FUN=sum)
public_d_ipd_net2 <- full_join_NA(x = public_d_ipd_net2, y = public_d_ipd_net2_amount, by = "Centile")
public_d_ipd_net2 <- if(1 %in% public_d_ipd_net2$Centile == F) {add_row(public_d_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net2}
ugasoc_d_ipd_crg <- aggregate(ugasoc_d_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_crg_amount <- aggregate(ugasoc_d_ipd$dmc_caregiver, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_crg <- full_join_NA(x = ugasoc_d_ipd_crg, y = ugasoc_d_ipd_crg_amount, by = "Centile")
ugasoc_d_ipd_crg <- if(1 %in% ugasoc_d_ipd_crg$Centile == F) {add_row(ugasoc_d_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg}
ugasoc_d_ipd_crg2 <- aggregate(ugasoc_d_ipd$all_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_crg2_amount <- aggregate(ugasoc_d_ipd$all_caregiver, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_crg2 <- full_join_NA(x = ugasoc_d_ipd_crg2, y = ugasoc_d_ipd_crg2_amount, by = "Centile")
ugasoc_d_ipd_crg2 <- if(1 %in% ugasoc_d_ipd_crg2$Centile == F) {add_row(ugasoc_d_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg2}
public_d_ipd_gov <- arrange(public_d_ipd_gov, Centile)
public_d_ipd_net <- arrange(public_d_ipd_net, Centile)
public_d_ipd_net2 <- arrange(public_d_ipd_net2, Centile)
ugasoc_d_ipd_crg <- arrange(ugasoc_d_ipd_crg, Centile)
ugasoc_d_ipd_crg2 <- arrange(ugasoc_d_ipd_crg2, Centile)

public_d_opd_gov <- aggregate(public_d_opd$dmc_facility_prop, by=list(Centile=public_d_opd$hhhead_income), FUN=sum)
public_d_opd_gov_amount <- aggregate((public_d_opd$dmc_facility + public_d_opd$dmc_caregiver_c), by=list(Centile=public_d_opd$hhhead_income), FUN=sum)
public_d_opd_gov <- full_join_NA(x = public_d_opd_gov, y = public_d_opd_gov_amount, by = "Centile")
public_d_opd_gov <- if(1 %in% public_d_opd_gov$Centile == F) {add_row(public_d_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_gov}
public_d_opd_net <- aggregate(public_d_opd$dmc_facility_net_prop, by=list(Centile=public_d_opd$hhhead_income), FUN=sum)
public_d_opd_net_amount <- aggregate(public_d_opd$dmc_facility, by=list(Centile=public_d_opd$hhhead_income), FUN=sum)
public_d_opd_net <- full_join_NA(x = public_d_opd_net, y = public_d_opd_net_amount, by = "Centile")
public_d_opd_net <- if(1 %in% public_d_opd_net$Centile == F) {add_row(public_d_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net}
public_d_opd_net2 <- aggregate(public_d_opd$dmc_facility_net2_prop, by=list(Centile=public_d_opd$hhhead_income), FUN=sum)
public_d_opd_net2_amount <- aggregate((public_d_opd$dmc_facility - public_d_opd$dmc_caregiver_ba), by=list(Centile=public_d_opd$hhhead_income), FUN=sum)
public_d_opd_net2 <- full_join_NA(x = public_d_opd_net2, y = public_d_opd_net2_amount, by = "Centile")
public_d_opd_net2 <- if(1 %in% public_d_opd_net2$Centile == F) {add_row(public_d_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net2}
ugasoc_d_opd_crg <- aggregate(ugasoc_d_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_crg_amount <- aggregate(ugasoc_d_opd$dmc_caregiver, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_crg <- full_join_NA(x = ugasoc_d_opd_crg, y = ugasoc_d_opd_crg_amount, by = "Centile")
ugasoc_d_opd_crg <- if(1 %in% ugasoc_d_opd_crg$Centile == F) {add_row(ugasoc_d_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg}
ugasoc_d_opd_crg2 <- aggregate(ugasoc_d_opd$all_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_crg2_amount <- aggregate(ugasoc_d_opd$all_caregiver, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_crg2 <- full_join_NA(x = ugasoc_d_opd_crg2, y = ugasoc_d_opd_crg2_amount, by = "Centile")
ugasoc_d_opd_crg2 <- if(1 %in% ugasoc_d_opd_crg2$Centile == F) {add_row(ugasoc_d_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg2}
public_d_opd_gov <- arrange(public_d_opd_gov, Centile)
public_d_opd_net <- arrange(public_d_opd_net, Centile)
public_d_opd_net2 <- arrange(public_d_opd_net2, Centile)
ugasoc_d_opd_crg <- arrange(ugasoc_d_opd_crg, Centile)
ugasoc_d_opd_crg2 <- arrange(ugasoc_d_opd_crg2, Centile)

public_m_ipd_gov <- aggregate(public_m_ipd$dmc_facility_prop, by=list(Centile=public_m_ipd$hhhead_income), FUN=sum)
public_m_ipd_gov_amount <- aggregate((public_m_ipd$dmc_facility + public_m_ipd$dmc_caregiver_c), by=list(Centile=public_m_ipd$hhhead_income), FUN=sum)
public_m_ipd_gov <- full_join_NA(x = public_m_ipd_gov, y = public_m_ipd_gov_amount, by = "Centile")
public_m_ipd_gov <- if(1 %in% public_m_ipd_gov$Centile == F) {add_row(public_m_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_gov}
public_m_ipd_net <- aggregate(public_m_ipd$dmc_facility_net_prop, by=list(Centile=public_m_ipd$hhhead_income), FUN=sum)
public_m_ipd_net_amount <- aggregate(public_m_ipd$dmc_facility, by=list(Centile=public_m_ipd$hhhead_income), FUN=sum)
public_m_ipd_net <- full_join_NA(x = public_m_ipd_net, y = public_m_ipd_net_amount, by = "Centile")
public_m_ipd_net <- if(1 %in% public_m_ipd_net$Centile == F) {add_row(public_m_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net}
public_m_ipd_net2 <- aggregate(public_m_ipd$dmc_facility_net2_prop, by=list(Centile=public_m_ipd$hhhead_income), FUN=sum)
public_m_ipd_net2_amount <- aggregate((public_m_ipd$dmc_facility - public_m_ipd$dmc_caregiver_ba), by=list(Centile=public_m_ipd$hhhead_income), FUN=sum)
public_m_ipd_net2 <- full_join_NA(x = public_m_ipd_net2, y = public_m_ipd_net2_amount, by = "Centile")
public_m_ipd_net2 <- if(1 %in% public_m_ipd_net2$Centile == F) {add_row(public_m_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net2}
ugasoc_m_ipd_crg <- aggregate(ugasoc_m_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_crg_amount <- aggregate(ugasoc_m_ipd$dmc_caregiver, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_crg <- full_join_NA(x = ugasoc_m_ipd_crg, y = ugasoc_m_ipd_crg_amount, by = "Centile")
ugasoc_m_ipd_crg <- if(1 %in% ugasoc_m_ipd_crg$Centile == F) {add_row(ugasoc_m_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg}
ugasoc_m_ipd_crg2 <- aggregate(ugasoc_m_ipd$all_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_crg2_amount <- aggregate(ugasoc_m_ipd$all_caregiver, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_crg2 <- full_join_NA(x = ugasoc_m_ipd_crg2, y = ugasoc_m_ipd_crg2_amount, by = "Centile")
ugasoc_m_ipd_crg2 <- if(1 %in% ugasoc_m_ipd_crg2$Centile == F) {add_row(ugasoc_m_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg2}
public_m_ipd_gov <- arrange(public_m_ipd_gov, Centile)
public_m_ipd_net <- arrange(public_m_ipd_net, Centile)
public_m_ipd_net2 <- arrange(public_m_ipd_net2, Centile)
ugasoc_m_ipd_crg <- arrange(ugasoc_m_ipd_crg, Centile)
ugasoc_m_ipd_crg2 <- arrange(ugasoc_m_ipd_crg2, Centile)

public_m_opd_gov <- aggregate(public_m_opd$dmc_facility_prop, by=list(Centile=public_m_opd$hhhead_income), FUN=sum)
public_m_opd_gov_amount <- aggregate((public_m_opd$dmc_facility + public_m_opd$dmc_caregiver_c), by=list(Centile=public_m_opd$hhhead_income), FUN=sum)
public_m_opd_gov <- full_join_NA(x = public_m_opd_gov, y = public_m_opd_gov_amount, by = "Centile")
public_m_opd_gov <- if(1 %in% public_m_opd_gov$Centile == F) {add_row(public_m_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_gov}
public_m_opd_net <- aggregate(public_m_opd$dmc_facility_net_prop, by=list(Centile=public_m_opd$hhhead_income), FUN=sum)
public_m_opd_net_amount <- aggregate(public_m_opd$dmc_facility, by=list(Centile=public_m_opd$hhhead_income), FUN=sum)
public_m_opd_net <- full_join_NA(x = public_m_opd_net, y = public_m_opd_net_amount, by = "Centile")
public_m_opd_net <- if(1 %in% public_m_opd_net$Centile == F) {add_row(public_m_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net}
public_m_opd_net2 <- aggregate(public_m_opd$dmc_facility_net2_prop, by=list(Centile=public_m_opd$hhhead_income), FUN=sum)
public_m_opd_net2_amount <- aggregate((public_m_opd$dmc_facility - public_m_opd$dmc_caregiver_ba), by=list(Centile=public_m_opd$hhhead_income), FUN=sum)
public_m_opd_net2 <- full_join_NA(x = public_m_opd_net2, y = public_m_opd_net2_amount, by = "Centile")
public_m_opd_net2 <- if(1 %in% public_m_opd_net2$Centile == F) {add_row(public_m_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net2}
ugasoc_m_opd_crg <- aggregate(ugasoc_m_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_crg_amount <- aggregate(ugasoc_m_opd$dmc_caregiver, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_crg <- full_join_NA(x = ugasoc_m_opd_crg, y = ugasoc_m_opd_crg_amount, by = "Centile")
ugasoc_m_opd_crg <- if(1 %in% ugasoc_m_opd_crg$Centile == F) {add_row(ugasoc_m_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg}
ugasoc_m_opd_crg2 <- aggregate(ugasoc_m_opd$all_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_crg2_amount <- aggregate(ugasoc_m_opd$all_caregiver, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_crg2 <- full_join_NA(x = ugasoc_m_opd_crg2, y = ugasoc_m_opd_crg2_amount, by = "Centile")
ugasoc_m_opd_crg2 <- if(1 %in% ugasoc_m_opd_crg2$Centile == F) {add_row(ugasoc_m_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg2}
public_m_opd_gov <- arrange(public_m_opd_gov, Centile)
public_m_opd_net <- arrange(public_m_opd_net, Centile)
public_m_opd_net2 <- arrange(public_m_opd_net2, Centile)
ugasoc_m_opd_crg <- arrange(ugasoc_m_opd_crg, Centile)
ugasoc_m_opd_crg2 <- arrange(ugasoc_m_opd_crg, Centile)


# Rename variables
public_p_ipd_gov <- rename(public_p_ipd_gov, gov_prop = x.x)
public_p_ipd_gov <- rename(public_p_ipd_gov, gov = x.y)
public_p_ipd_net <- rename(public_p_ipd_net, net_prop = x.x)
public_p_ipd_net <- rename(public_p_ipd_net, net = x.y)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2_prop = x.x)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2 = x.y)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg_prop = x.x)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg = x.y)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2_prop = x.x)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2 = x.y)

public_p_opd_gov <- rename(public_p_opd_gov, gov_prop = x.x)
public_p_opd_gov <- rename(public_p_opd_gov, gov = x.y)
public_p_opd_net <- rename(public_p_opd_net, net_prop = x.x)
public_p_opd_net <- rename(public_p_opd_net, net = x.y)
public_p_opd_net2 <- rename(public_p_opd_net2, net2_prop = x.x)
public_p_opd_net2 <- rename(public_p_opd_net2, net2 = x.y)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg_prop = x.x)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg = x.y)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2_prop = x.x)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2 = x.y)

public_d_ipd_gov <- rename(public_d_ipd_gov, gov_prop = x.x)
public_d_ipd_gov <- rename(public_d_ipd_gov, gov = x.y)
public_d_ipd_net <- rename(public_d_ipd_net, net_prop = x.x)
public_d_ipd_net <- rename(public_d_ipd_net, net = x.y)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2_prop = x.x)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2 = x.y)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg_prop = x.x)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg = x.y)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2_prop = x.x)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2 = x.y)

public_d_opd_gov <- rename(public_d_opd_gov, gov_prop = x.x)
public_d_opd_gov <- rename(public_d_opd_gov, gov = x.y)
public_d_opd_net <- rename(public_d_opd_net, net_prop = x.x)
public_d_opd_net <- rename(public_d_opd_net, net = x.y)
public_d_opd_net2 <- rename(public_d_opd_net2, net2_prop = x.x)
public_d_opd_net2 <- rename(public_d_opd_net2, net2 = x.y)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg_prop = x.x)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg = x.y)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2_prop = x.x)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2 = x.y)

public_m_ipd_gov <- rename(public_m_ipd_gov, gov_prop = x.x)
public_m_ipd_gov <- rename(public_m_ipd_gov, gov = x.y)
public_m_ipd_net <- rename(public_m_ipd_net, net_prop = x.x)
public_m_ipd_net <- rename(public_m_ipd_net, net = x.y)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2_prop = x.x)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2 = x.y)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg_prop = x.x)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg = x.y)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2_prop = x.x)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2 = x.y)

public_m_opd_gov <- rename(public_m_opd_gov, gov_prop = x.x)
public_m_opd_gov <- rename(public_m_opd_gov, gov = x.y)
public_m_opd_net <- rename(public_m_opd_net, net_prop = x.x)
public_m_opd_net <- rename(public_m_opd_net, net = x.y)
public_m_opd_net2 <- rename(public_m_opd_net2, net2_prop = x.x)
public_m_opd_net2 <- rename(public_m_opd_net2, net2 = x.y)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg_prop = x.x)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg = x.y)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2_prop = x.x)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2 = x.y)


# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(public_p_ipd_gov, public_p_ipd_net, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, public_p_ipd_net2, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg2, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(public_p_opd_gov, public_p_opd_net, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, public_p_opd_net2, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg2, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(public_d_ipd_gov, public_d_ipd_net, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, public_d_ipd_net2, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg2, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(public_d_opd_gov, public_d_opd_net, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, public_d_opd_net2, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg2, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(public_m_ipd_gov, public_m_ipd_net, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, public_m_ipd_net2, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg2, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(public_m_opd_gov, public_m_opd_net, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, public_m_opd_net2, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg2, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)

# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_p_ipd <- mutate(uga_p_ipd, gov_cumul = cumsum(gov))
uga_p_ipd <- mutate(uga_p_ipd, net_prop_cumul = cumsum(net_prop))
uga_p_ipd <- mutate(uga_p_ipd, net_cumul = cumsum(net))
uga_p_ipd <- mutate(uga_p_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_p_ipd <- mutate(uga_p_ipd, net2_cumul = cumsum(net2))
uga_p_ipd <- mutate(uga_p_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg_cumul = cumsum(crg))
uga_p_ipd <- mutate(uga_p_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg2_cumul = cumsum(crg2))

uga_p_opd <- mutate(uga_p_opd, gov_prop_cumul = cumsum(gov_prop))
uga_p_opd <- mutate(uga_p_opd, gov_cumul = cumsum(gov))
uga_p_opd <- mutate(uga_p_opd, net_prop_cumul = cumsum(net_prop))
uga_p_opd <- mutate(uga_p_opd, net_cumul = cumsum(net))
uga_p_opd <- mutate(uga_p_opd, net2_prop_cumul = cumsum(net2_prop))
uga_p_opd <- mutate(uga_p_opd, net2_cumul = cumsum(net2))
uga_p_opd <- mutate(uga_p_opd, crg_prop_cumul = cumsum(crg_prop))
uga_p_opd <- mutate(uga_p_opd, crg_cumul = cumsum(crg))
uga_p_opd <- mutate(uga_p_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_opd <- mutate(uga_p_opd, crg2_cumul = cumsum(crg2))

uga_d_ipd <- mutate(uga_d_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_d_ipd <- mutate(uga_d_ipd, gov_cumul = cumsum(gov))
uga_d_ipd <- mutate(uga_d_ipd, net_prop_cumul = cumsum(net_prop))
uga_d_ipd <- mutate(uga_d_ipd, net_cumul = cumsum(net))
uga_d_ipd <- mutate(uga_d_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_d_ipd <- mutate(uga_d_ipd, net2_cumul = cumsum(net2))
uga_d_ipd <- mutate(uga_d_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg_cumul = cumsum(crg))
uga_d_ipd <- mutate(uga_d_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg2_cumul = cumsum(crg2))

uga_d_opd <- mutate(uga_d_opd, gov_prop_cumul = cumsum(gov_prop))
uga_d_opd <- mutate(uga_d_opd, gov_cumul = cumsum(gov))
uga_d_opd <- mutate(uga_d_opd, net_prop_cumul = cumsum(net_prop))
uga_d_opd <- mutate(uga_d_opd, net_cumul = cumsum(net))
uga_d_opd <- mutate(uga_d_opd, net2_prop_cumul = cumsum(net2_prop))
uga_d_opd <- mutate(uga_d_opd, net2_cumul = cumsum(net2))
uga_d_opd <- mutate(uga_d_opd, crg_prop_cumul = cumsum(crg_prop))
uga_d_opd <- mutate(uga_d_opd, crg_cumul = cumsum(crg))
uga_d_opd <- mutate(uga_d_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_opd <- mutate(uga_d_opd, crg2_cumul = cumsum(crg2))

uga_m_ipd <- mutate(uga_m_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_m_ipd <- mutate(uga_m_ipd, gov_cumul = cumsum(gov))
uga_m_ipd <- mutate(uga_m_ipd, net_prop_cumul = cumsum(net_prop))
uga_m_ipd <- mutate(uga_m_ipd, net_cumul = cumsum(net))
uga_m_ipd <- mutate(uga_m_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_m_ipd <- mutate(uga_m_ipd, net2_cumul = cumsum(net2))
uga_m_ipd <- mutate(uga_m_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg_cumul = cumsum(crg))
uga_m_ipd <- mutate(uga_m_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg2_cumul = cumsum(crg2))

uga_m_opd <- mutate(uga_m_opd, gov_prop_cumul = cumsum(gov_prop))
uga_m_opd <- mutate(uga_m_opd, gov_cumul = cumsum(gov))
uga_m_opd <- mutate(uga_m_opd, net_prop_cumul = cumsum(net_prop))
uga_m_opd <- mutate(uga_m_opd, net_cumul = cumsum(net))
uga_m_opd <- mutate(uga_m_opd, net2_prop_cumul = cumsum(net2_prop))
uga_m_opd <- mutate(uga_m_opd, net2_cumul = cumsum(net2))
uga_m_opd <- mutate(uga_m_opd, crg_prop_cumul = cumsum(crg_prop))
uga_m_opd <- mutate(uga_m_opd, crg_cumul = cumsum(crg))
uga_m_opd <- mutate(uga_m_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_opd <- mutate(uga_m_opd, crg2_cumul = cumsum(crg2))



# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)


# Calculate
concentration_uga_gov_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$gov)) * cov(df_concentration_uga_p_ipd$gov, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_gov_p_opd <- round((2/mean(df_concentration_uga_p_opd$gov)) * cov(df_concentration_uga_p_opd$gov, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_gov_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$gov)) * cov(df_concentration_uga_d_ipd$gov, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_gov_d_opd <- round((2/mean(df_concentration_uga_d_opd$gov)) * cov(df_concentration_uga_d_opd$gov, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_gov_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$gov)) * cov(df_concentration_uga_m_ipd$gov, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_gov_m_opd <- round((2/mean(df_concentration_uga_m_opd$gov)) * cov(df_concentration_uga_m_opd$gov, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_net_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$net)) * cov(df_concentration_uga_p_ipd$net, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_net_p_opd <- round((2/mean(df_concentration_uga_p_opd$net)) * cov(df_concentration_uga_p_opd$net, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_net_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$net)) * cov(df_concentration_uga_d_ipd$net, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_net_d_opd <- round((2/mean(df_concentration_uga_d_opd$net)) * cov(df_concentration_uga_d_opd$net, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_net_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$net)) * cov(df_concentration_uga_m_ipd$net, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_net_m_opd <- round((2/mean(df_concentration_uga_m_opd$net)) * cov(df_concentration_uga_m_opd$net, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg)) * cov(df_concentration_uga_p_ipd$crg, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg)) * cov(df_concentration_uga_p_opd$crg, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg)) * cov(df_concentration_uga_d_ipd$crg, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg)) * cov(df_concentration_uga_d_opd$crg, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg)) * cov(df_concentration_uga_m_ipd$crg, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg)) * cov(df_concentration_uga_m_opd$crg, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg2_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg2)) * cov(df_concentration_uga_p_ipd$crg2, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg2_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg2)) * cov(df_concentration_uga_p_opd$crg2, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg2_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg2)) * cov(df_concentration_uga_d_ipd$crg2, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg2_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg2)) * cov(df_concentration_uga_d_opd$crg2, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg2_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg2)) * cov(df_concentration_uga_m_ipd$crg2, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg2_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg2)) * cov(df_concentration_uga_m_opd$crg2, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
public_gov <- c(concentration_uga_gov_p_ipd, concentration_uga_gov_p_opd, concentration_uga_gov_d_ipd, concentration_uga_gov_d_opd, concentration_uga_gov_m_ipd, concentration_uga_gov_m_opd)
public_net <- c(concentration_uga_net_p_ipd, concentration_uga_net_p_opd, concentration_uga_net_d_ipd, concentration_uga_net_d_opd, concentration_uga_net_m_ipd, concentration_uga_net_m_opd)
oop <- c(concentration_uga_crg_p_ipd, concentration_uga_crg_p_opd, concentration_uga_crg_d_ipd, concentration_uga_crg_d_opd, concentration_uga_crg_m_ipd, concentration_uga_crg_m_opd)
oop_all <- c(concentration_uga_crg2_p_ipd, concentration_uga_crg2_p_opd, concentration_uga_crg2_d_ipd, concentration_uga_crg2_d_opd, concentration_uga_crg2_m_ipd, concentration_uga_crg2_m_opd)
wealth_distrib <- c('income', 'income', 'income', 'income', 'income', 'income')
wealth_source <- c('sample', 'sample', 'sample', 'sample', 'sample', 'sample')
concentration_uga_inc <- data.frame(disease, visittype, public_gov, public_net, oop, oop_all, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_gov <- graph_gov(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_gov_p_ipd, ci2 = concentration_uga_net_p_ipd)
graph_p_ipd_crg <- graph_crg(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_crg_p_ipd, ci2 = concentration_uga_crg2_p_ipd)
graph_p_ipd_gov_freq <- graph_freq(dataset = public_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_ipd_crg_freq <- graph_freq(dataset = ugasoc_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_gov <- graph_gov(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_gov_p_opd, ci2 = concentration_uga_net_p_opd)
graph_p_opd_crg <- graph_crg(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_crg_p_opd, ci2 = concentration_uga_crg2_p_opd)
graph_p_opd_gov_freq <- graph_freq(dataset = public_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_opd_crg_freq <- graph_freq(dataset = ugasoc_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov,graph_p_ipd_crg,graph_p_opd_gov,graph_p_opd_crg)
dev.off()
jpeg(filename = paste0("graph_p_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov_freq,graph_p_ipd_crg_freq,graph_p_opd_gov_freq,graph_p_opd_crg_freq)
dev.off()


# Diarrhea
graph_d_ipd_gov <- graph_gov(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_gov_d_ipd, ci2 = concentration_uga_net_d_ipd)
graph_d_ipd_crg <- graph_crg(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_crg_d_ipd, ci2 = concentration_uga_crg2_d_ipd)
graph_d_ipd_gov_freq <- graph_freq(dataset = public_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_ipd_crg_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_gov <- graph_gov(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_gov_d_opd, ci2 = concentration_uga_net_d_opd)
graph_d_opd_crg <- graph_crg(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_crg_d_opd, ci2 = concentration_uga_crg2_d_opd)
graph_d_opd_gov_freq <- graph_freq(dataset = public_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_opd_crg_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov,graph_d_ipd_crg,graph_d_opd_gov,graph_d_opd_crg)
dev.off()
jpeg(filename = paste0("graph_d_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov_freq,graph_d_ipd_crg_freq,graph_d_opd_gov_freq,graph_d_opd_crg_freq)
dev.off()

# Measles 
graph_m_ipd_gov <- graph_gov(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_gov_m_ipd, ci2 = concentration_uga_net_m_ipd)
graph_m_ipd_crg <- graph_crg(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_crg_m_ipd, ci2 = concentration_uga_crg2_m_ipd)
graph_m_ipd_gov_freq <- graph_freq(dataset = public_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_ipd_crg_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_gov <- graph_gov(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_gov_m_opd, ci2 = concentration_uga_net_m_opd)
graph_m_opd_crg <- graph_crg(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_crg_m_opd, ci2 = concentration_uga_crg2_m_opd)
graph_m_opd_gov_freq <- graph_freq(dataset = public_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_opd_crg_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov,graph_m_ipd_crg,graph_m_opd_gov,graph_m_opd_crg)
dev.off()
jpeg(filename = paste0("graph_m_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov_freq,graph_m_ipd_crg_freq,graph_m_opd_gov_freq,graph_m_opd_crg_freq)
dev.off()


# Separate income distribution for comparison
hhhead_income_p_ipd <- ugasoc_p_ipd %>% select(c3_caretakerid, hhhead_income)
hhhead_income_p_opd <- ugasoc_p_opd %>% select(c3_caretakerid, hhhead_income)
hhhead_income_d_ipd <- ugasoc_d_ipd %>% select(c3_caretakerid, hhhead_income)
hhhead_income_d_opd <- ugasoc_d_opd %>% select(c3_caretakerid, hhhead_income)
hhhead_income_m_ipd <- ugasoc_m_ipd %>% select(c3_caretakerid, hhhead_income)
hhhead_income_m_opd <- ugasoc_m_opd %>% select(c3_caretakerid, hhhead_income)



################################################
# PHE & OOP based on INCOME (World Bank)
################################################
user0 <- 1
user1 <- 1


# Sum proportion of healthcare spendings by nationally representative (WB) quantile/decile/centile
public_p_ipd_gov <- aggregate(public_p_ipd$dmc_facility_prop, by=list(Centile=public_p_ipd$hhhead_income_wb), FUN=sum)
public_p_ipd_gov_amount <- aggregate((public_p_ipd$dmc_facility + public_p_ipd$dmc_caregiver_c), by=list(Centile=public_p_ipd$hhhead_income_wb), FUN=sum)
public_p_ipd_gov <- full_join_NA(x = public_p_ipd_gov, y = public_p_ipd_gov_amount, by = "Centile")
public_p_ipd_gov <- if(1 %in% public_p_ipd_gov$Centile == F) {add_row(public_p_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_gov}
public_p_ipd_net <- aggregate(public_p_ipd$dmc_facility_net_prop, by=list(Centile=public_p_ipd$hhhead_income_wb), FUN=sum)
public_p_ipd_net_amount <- aggregate(public_p_ipd$dmc_facility, by=list(Centile=public_p_ipd$hhhead_income_wb), FUN=sum)
public_p_ipd_net <- full_join_NA(x = public_p_ipd_net, y = public_p_ipd_net_amount, by = "Centile")
public_p_ipd_net <- if(1 %in% public_p_ipd_net$Centile == F) {add_row(public_p_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net}
public_p_ipd_net2 <- aggregate(public_p_ipd$dmc_facility_net2_prop, by=list(Centile=public_p_ipd$hhhead_income_wb), FUN=sum)
public_p_ipd_net2_amount <- aggregate((public_p_ipd$dmc_facility - public_p_ipd$dmc_caregiver_ba), by=list(Centile=public_p_ipd$hhhead_income_wb), FUN=sum)
public_p_ipd_net2 <- full_join_NA(x = public_p_ipd_net2, y = public_p_ipd_net2_amount, by = "Centile")
public_p_ipd_net2 <- if(1 %in% public_p_ipd_net2$Centile == F) {add_row(public_p_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net2}
ugasoc_p_ipd_crg <- aggregate(ugasoc_p_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_crg_amount <- aggregate(ugasoc_p_ipd$dmc_caregiver, by=list(Centile=ugasoc_p_ipd$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_crg <- full_join_NA(x = ugasoc_p_ipd_crg, y = ugasoc_p_ipd_crg_amount, by = "Centile")
ugasoc_p_ipd_crg <- if(1 %in% ugasoc_p_ipd_crg$Centile == F) {add_row(ugasoc_p_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg}
ugasoc_p_ipd_crg2 <- aggregate(ugasoc_p_ipd$all_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_crg2_amount <- aggregate(ugasoc_p_ipd$all_caregiver, by=list(Centile=ugasoc_p_ipd$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_crg2 <- full_join_NA(x = ugasoc_p_ipd_crg2, y = ugasoc_p_ipd_crg2_amount, by = "Centile")
ugasoc_p_ipd_crg2 <- if(1 %in% ugasoc_p_ipd_crg2$Centile == F) {add_row(ugasoc_p_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg2}
public_p_ipd_gov <- arrange(public_p_ipd_gov, Centile)
public_p_ipd_net <- arrange(public_p_ipd_net, Centile)
public_p_ipd_net2 <- arrange(public_p_ipd_net2, Centile)
ugasoc_p_ipd_crg <- arrange(ugasoc_p_ipd_crg, Centile)
ugasoc_p_ipd_crg2 <- arrange(ugasoc_p_ipd_crg2, Centile)

public_p_opd_gov <- aggregate(public_p_opd$dmc_facility_prop, by=list(Centile=public_p_opd$hhhead_income_wb), FUN=sum)
public_p_opd_gov_amount <- aggregate((public_p_opd$dmc_facility + public_p_opd$dmc_caregiver_c), by=list(Centile=public_p_opd$hhhead_income_wb), FUN=sum)
public_p_opd_gov <- full_join_NA(x = public_p_opd_gov, y = public_p_opd_gov_amount, by = "Centile")
public_p_opd_gov <- if(1 %in% public_p_opd_gov$Centile == F) {add_row(public_p_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_gov}
public_p_opd_net <- aggregate(public_p_opd$dmc_facility_net_prop, by=list(Centile=public_p_opd$hhhead_income_wb), FUN=sum)
public_p_opd_net_amount <- aggregate(public_p_opd$dmc_facility, by=list(Centile=public_p_opd$hhhead_income_wb), FUN=sum)
public_p_opd_net <- full_join_NA(x = public_p_opd_net, y = public_p_opd_net_amount, by = "Centile")
public_p_opd_net <- if(1 %in% public_p_opd_net$Centile == F) {add_row(public_p_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net}
public_p_opd_net2 <- aggregate(public_p_opd$dmc_facility_net2_prop, by=list(Centile=public_p_opd$hhhead_income_wb), FUN=sum)
public_p_opd_net2_amount <- aggregate((public_p_opd$dmc_facility - public_p_opd$dmc_caregiver_ba), by=list(Centile=public_p_opd$hhhead_income_wb), FUN=sum)
public_p_opd_net2 <- full_join_NA(x = public_p_opd_net2, y = public_p_opd_net2_amount, by = "Centile")
public_p_opd_net2 <- if(1 %in% public_p_opd_net2$Centile == F) {add_row(public_p_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net2}
ugasoc_p_opd_crg <- aggregate(ugasoc_p_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_crg_amount <- aggregate(ugasoc_p_opd$dmc_caregiver, by=list(Centile=ugasoc_p_opd$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_crg <- full_join_NA(x = ugasoc_p_opd_crg, y = ugasoc_p_opd_crg_amount, by = "Centile")
ugasoc_p_opd_crg <- if(1 %in% ugasoc_p_opd_crg$Centile == F) {add_row(ugasoc_p_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg}
ugasoc_p_opd_crg2 <- aggregate(ugasoc_p_opd$all_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_crg2_amount <- aggregate(ugasoc_p_opd$all_caregiver, by=list(Centile=ugasoc_p_opd$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_crg2 <- full_join_NA(x = ugasoc_p_opd_crg2, y = ugasoc_p_opd_crg2_amount, by = "Centile")
ugasoc_p_opd_crg2 <- if(1 %in% ugasoc_p_opd_crg2$Centile == F) {add_row(ugasoc_p_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg2}
public_p_opd_gov <- arrange(public_p_opd_gov, Centile)
public_p_opd_net <- arrange(public_p_opd_net, Centile)
public_p_opd_net2 <- arrange(public_p_opd_net2, Centile)
ugasoc_p_opd_crg <- arrange(ugasoc_p_opd_crg, Centile)
ugasoc_p_opd_crg2 <- arrange(ugasoc_p_opd_crg2, Centile)

public_d_ipd_gov <- aggregate(public_d_ipd$dmc_facility_prop, by=list(Centile=public_d_ipd$hhhead_income_wb), FUN=sum)
public_d_ipd_gov_amount <- aggregate((public_d_ipd$dmc_facility + public_d_ipd$dmc_caregiver_c), by=list(Centile=public_d_ipd$hhhead_income_wb), FUN=sum)
public_d_ipd_gov <- full_join_NA(x = public_d_ipd_gov, y = public_d_ipd_gov_amount, by = "Centile")
public_d_ipd_gov <- if(1 %in% public_d_ipd_gov$Centile == F) {add_row(public_d_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_gov}
public_d_ipd_net <- aggregate(public_d_ipd$dmc_facility_net_prop, by=list(Centile=public_d_ipd$hhhead_income_wb), FUN=sum)
public_d_ipd_net_amount <- aggregate(public_d_ipd$dmc_facility, by=list(Centile=public_d_ipd$hhhead_income_wb), FUN=sum)
public_d_ipd_net <- full_join_NA(x = public_d_ipd_net, y = public_d_ipd_net_amount, by = "Centile")
public_d_ipd_net <- if(1 %in% public_d_ipd_net$Centile == F) {add_row(public_d_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net}
public_d_ipd_net2 <- aggregate(public_d_ipd$dmc_facility_net2_prop, by=list(Centile=public_d_ipd$hhhead_income_wb), FUN=sum)
public_d_ipd_net2_amount <- aggregate((public_d_ipd$dmc_facility - public_d_ipd$dmc_caregiver_ba), by=list(Centile=public_d_ipd$hhhead_income_wb), FUN=sum)
public_d_ipd_net2 <- full_join_NA(x = public_d_ipd_net2, y = public_d_ipd_net2_amount, by = "Centile")
public_d_ipd_net2 <- if(1 %in% public_d_ipd_net2$Centile == F) {add_row(public_d_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net2}
ugasoc_d_ipd_crg <- aggregate(ugasoc_d_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_crg_amount <- aggregate(ugasoc_d_ipd$dmc_caregiver, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_crg <- full_join_NA(x = ugasoc_d_ipd_crg, y = ugasoc_d_ipd_crg_amount, by = "Centile")
ugasoc_d_ipd_crg <- if(1 %in% ugasoc_d_ipd_crg$Centile == F) {add_row(ugasoc_d_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg}
ugasoc_d_ipd_crg2 <- aggregate(ugasoc_d_ipd$all_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_crg2_amount <- aggregate(ugasoc_d_ipd$all_caregiver, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_crg2 <- full_join_NA(x = ugasoc_d_ipd_crg2, y = ugasoc_d_ipd_crg2_amount, by = "Centile")
ugasoc_d_ipd_crg2 <- if(1 %in% ugasoc_d_ipd_crg2$Centile == F) {add_row(ugasoc_d_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg2}
public_d_ipd_gov <- arrange(public_d_ipd_gov, Centile)
public_d_ipd_net <- arrange(public_d_ipd_net, Centile)
public_d_ipd_net2 <- arrange(public_d_ipd_net2, Centile)
ugasoc_d_ipd_crg <- arrange(ugasoc_d_ipd_crg, Centile)
ugasoc_d_ipd_crg2 <- arrange(ugasoc_d_ipd_crg2, Centile)

public_d_opd_gov <- aggregate(public_d_opd$dmc_facility_prop, by=list(Centile=public_d_opd$hhhead_income_wb), FUN=sum)
public_d_opd_gov_amount <- aggregate((public_d_opd$dmc_facility + public_d_opd$dmc_caregiver_c), by=list(Centile=public_d_opd$hhhead_income_wb), FUN=sum)
public_d_opd_gov <- full_join_NA(x = public_d_opd_gov, y = public_d_opd_gov_amount, by = "Centile")
public_d_opd_gov <- if(1 %in% public_d_opd_gov$Centile == F) {add_row(public_d_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_gov}
public_d_opd_net <- aggregate(public_d_opd$dmc_facility_net_prop, by=list(Centile=public_d_opd$hhhead_income_wb), FUN=sum)
public_d_opd_net_amount <- aggregate(public_d_opd$dmc_facility, by=list(Centile=public_d_opd$hhhead_income_wb), FUN=sum)
public_d_opd_net <- full_join_NA(x = public_d_opd_net, y = public_d_opd_net_amount, by = "Centile")
public_d_opd_net <- if(1 %in% public_d_opd_net$Centile == F) {add_row(public_d_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net}
public_d_opd_net2 <- aggregate(public_d_opd$dmc_facility_net2_prop, by=list(Centile=public_d_opd$hhhead_income_wb), FUN=sum)
public_d_opd_net2_amount <- aggregate((public_d_opd$dmc_facility - public_d_opd$dmc_caregiver_ba), by=list(Centile=public_d_opd$hhhead_income_wb), FUN=sum)
public_d_opd_net2 <- full_join_NA(x = public_d_opd_net2, y = public_d_opd_net2_amount, by = "Centile")
public_d_opd_net2 <- if(1 %in% public_d_opd_net2$Centile == F) {add_row(public_d_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net2}
ugasoc_d_opd_crg <- aggregate(ugasoc_d_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_crg_amount <- aggregate(ugasoc_d_opd$dmc_caregiver, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_crg <- full_join_NA(x = ugasoc_d_opd_crg, y = ugasoc_d_opd_crg_amount, by = "Centile")
ugasoc_d_opd_crg <- if(1 %in% ugasoc_d_opd_crg$Centile == F) {add_row(ugasoc_d_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg}
ugasoc_d_opd_crg2 <- aggregate(ugasoc_d_opd$all_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_crg2_amount <- aggregate(ugasoc_d_opd$all_caregiver, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_crg2 <- full_join_NA(x = ugasoc_d_opd_crg2, y = ugasoc_d_opd_crg2_amount, by = "Centile")
ugasoc_d_opd_crg2 <- if(1 %in% ugasoc_d_opd_crg2$Centile == F) {add_row(ugasoc_d_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg2}
public_d_opd_gov <- arrange(public_d_opd_gov, Centile)
public_d_opd_net <- arrange(public_d_opd_net, Centile)
public_d_opd_net2 <- arrange(public_d_opd_net2, Centile)
ugasoc_d_opd_crg <- arrange(ugasoc_d_opd_crg, Centile)
ugasoc_d_opd_crg2 <- arrange(ugasoc_d_opd_crg2, Centile)

public_m_ipd_gov <- aggregate(public_m_ipd$dmc_facility_prop, by=list(Centile=public_m_ipd$hhhead_income_wb), FUN=sum)
public_m_ipd_gov_amount <- aggregate((public_m_ipd$dmc_facility + public_m_ipd$dmc_caregiver_c), by=list(Centile=public_m_ipd$hhhead_income_wb), FUN=sum)
public_m_ipd_gov <- full_join_NA(x = public_m_ipd_gov, y = public_m_ipd_gov_amount, by = "Centile")
public_m_ipd_gov <- if(1 %in% public_m_ipd_gov$Centile == F) {add_row(public_m_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_gov}
public_m_ipd_net <- aggregate(public_m_ipd$dmc_facility_net_prop, by=list(Centile=public_m_ipd$hhhead_income_wb), FUN=sum)
public_m_ipd_net_amount <- aggregate(public_m_ipd$dmc_facility, by=list(Centile=public_m_ipd$hhhead_income_wb), FUN=sum)
public_m_ipd_net <- full_join_NA(x = public_m_ipd_net, y = public_m_ipd_net_amount, by = "Centile")
public_m_ipd_net <- if(1 %in% public_m_ipd_net$Centile == F) {add_row(public_m_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net}
public_m_ipd_net2 <- aggregate(public_m_ipd$dmc_facility_net2_prop, by=list(Centile=public_m_ipd$hhhead_income_wb), FUN=sum)
public_m_ipd_net2_amount <- aggregate((public_m_ipd$dmc_facility - public_m_ipd$dmc_caregiver_ba), by=list(Centile=public_m_ipd$hhhead_income_wb), FUN=sum)
public_m_ipd_net2 <- full_join_NA(x = public_m_ipd_net2, y = public_m_ipd_net2_amount, by = "Centile")
public_m_ipd_net2 <- if(1 %in% public_m_ipd_net2$Centile == F) {add_row(public_m_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net2}
ugasoc_m_ipd_crg <- aggregate(ugasoc_m_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_crg_amount <- aggregate(ugasoc_m_ipd$dmc_caregiver, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_crg <- full_join_NA(x = ugasoc_m_ipd_crg, y = ugasoc_m_ipd_crg_amount, by = "Centile")
ugasoc_m_ipd_crg <- if(1 %in% ugasoc_m_ipd_crg$Centile == F) {add_row(ugasoc_m_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg}
ugasoc_m_ipd_crg2 <- aggregate(ugasoc_m_ipd$all_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_crg2_amount <- aggregate(ugasoc_m_ipd$all_caregiver, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_crg2 <- full_join_NA(x = ugasoc_m_ipd_crg2, y = ugasoc_m_ipd_crg2_amount, by = "Centile")
ugasoc_m_ipd_crg2 <- if(1 %in% ugasoc_m_ipd_crg2$Centile == F) {add_row(ugasoc_m_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg2}
public_m_ipd_gov <- arrange(public_m_ipd_gov, Centile)
public_m_ipd_net <- arrange(public_m_ipd_net, Centile)
public_m_ipd_net2 <- arrange(public_m_ipd_net2, Centile)
ugasoc_m_ipd_crg <- arrange(ugasoc_m_ipd_crg, Centile)
ugasoc_m_ipd_crg2 <- arrange(ugasoc_m_ipd_crg2, Centile)

public_m_opd_gov <- aggregate(public_m_opd$dmc_facility_prop, by=list(Centile=public_m_opd$hhhead_income_wb), FUN=sum)
public_m_opd_gov_amount <- aggregate((public_m_opd$dmc_facility + public_m_opd$dmc_caregiver_c), by=list(Centile=public_m_opd$hhhead_income_wb), FUN=sum)
public_m_opd_gov <- full_join_NA(x = public_m_opd_gov, y = public_m_opd_gov_amount, by = "Centile")
public_m_opd_gov <- if(1 %in% public_m_opd_gov$Centile == F) {add_row(public_m_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_gov}
public_m_opd_net <- aggregate(public_m_opd$dmc_facility_net_prop, by=list(Centile=public_m_opd$hhhead_income_wb), FUN=sum)
public_m_opd_net_amount <- aggregate(public_m_opd$dmc_facility, by=list(Centile=public_m_opd$hhhead_income_wb), FUN=sum)
public_m_opd_net <- full_join_NA(x = public_m_opd_net, y = public_m_opd_net_amount, by = "Centile")
public_m_opd_net <- if(1 %in% public_m_opd_net$Centile == F) {add_row(public_m_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net}
public_m_opd_net2 <- aggregate(public_m_opd$dmc_facility_net2_prop, by=list(Centile=public_m_opd$hhhead_income_wb), FUN=sum)
public_m_opd_net2_amount <- aggregate((public_m_opd$dmc_facility - public_m_opd$dmc_caregiver_ba), by=list(Centile=public_m_opd$hhhead_income_wb), FUN=sum)
public_m_opd_net2 <- full_join_NA(x = public_m_opd_net2, y = public_m_opd_net2_amount, by = "Centile")
public_m_opd_net2 <- if(1 %in% public_m_opd_net2$Centile == F) {add_row(public_m_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net2}
ugasoc_m_opd_crg <- aggregate(ugasoc_m_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_crg_amount <- aggregate(ugasoc_m_opd$dmc_caregiver, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_crg <- full_join_NA(x = ugasoc_m_opd_crg, y = ugasoc_m_opd_crg_amount, by = "Centile")
ugasoc_m_opd_crg <- if(1 %in% ugasoc_m_opd_crg$Centile == F) {add_row(ugasoc_m_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg}
ugasoc_m_opd_crg2 <- aggregate(ugasoc_m_opd$all_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_crg2_amount <- aggregate(ugasoc_m_opd$all_caregiver, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_crg2 <- full_join_NA(x = ugasoc_m_opd_crg2, y = ugasoc_m_opd_crg2_amount, by = "Centile")
ugasoc_m_opd_crg2 <- if(1 %in% ugasoc_m_opd_crg2$Centile == F) {add_row(ugasoc_m_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg2}
public_m_opd_gov <- arrange(public_m_opd_gov, Centile)
public_m_opd_net <- arrange(public_m_opd_net, Centile)
public_m_opd_net2 <- arrange(public_m_opd_net2, Centile)
ugasoc_m_opd_crg <- arrange(ugasoc_m_opd_crg, Centile)
ugasoc_m_opd_crg2 <- arrange(ugasoc_m_opd_crg, Centile)



# Rename variables
public_p_ipd_gov <- rename(public_p_ipd_gov, gov_prop = x.x)
public_p_ipd_gov <- rename(public_p_ipd_gov, gov = x.y)
public_p_ipd_net <- rename(public_p_ipd_net, net_prop = x.x)
public_p_ipd_net <- rename(public_p_ipd_net, net = x.y)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2_prop = x.x)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2 = x.y)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg_prop = x.x)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg = x.y)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2_prop = x.x)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2 = x.y)

public_p_opd_gov <- rename(public_p_opd_gov, gov_prop = x.x)
public_p_opd_gov <- rename(public_p_opd_gov, gov = x.y)
public_p_opd_net <- rename(public_p_opd_net, net_prop = x.x)
public_p_opd_net <- rename(public_p_opd_net, net = x.y)
public_p_opd_net2 <- rename(public_p_opd_net2, net2_prop = x.x)
public_p_opd_net2 <- rename(public_p_opd_net2, net2 = x.y)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg_prop = x.x)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg = x.y)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2_prop = x.x)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2 = x.y)

public_d_ipd_gov <- rename(public_d_ipd_gov, gov_prop = x.x)
public_d_ipd_gov <- rename(public_d_ipd_gov, gov = x.y)
public_d_ipd_net <- rename(public_d_ipd_net, net_prop = x.x)
public_d_ipd_net <- rename(public_d_ipd_net, net = x.y)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2_prop = x.x)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2 = x.y)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg_prop = x.x)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg = x.y)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2_prop = x.x)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2 = x.y)

public_d_opd_gov <- rename(public_d_opd_gov, gov_prop = x.x)
public_d_opd_gov <- rename(public_d_opd_gov, gov = x.y)
public_d_opd_net <- rename(public_d_opd_net, net_prop = x.x)
public_d_opd_net <- rename(public_d_opd_net, net = x.y)
public_d_opd_net2 <- rename(public_d_opd_net2, net2_prop = x.x)
public_d_opd_net2 <- rename(public_d_opd_net2, net2 = x.y)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg_prop = x.x)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg = x.y)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2_prop = x.x)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2 = x.y)

public_m_ipd_gov <- rename(public_m_ipd_gov, gov_prop = x.x)
public_m_ipd_gov <- rename(public_m_ipd_gov, gov = x.y)
public_m_ipd_net <- rename(public_m_ipd_net, net_prop = x.x)
public_m_ipd_net <- rename(public_m_ipd_net, net = x.y)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2_prop = x.x)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2 = x.y)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg_prop = x.x)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg = x.y)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2_prop = x.x)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2 = x.y)

public_m_opd_gov <- rename(public_m_opd_gov, gov_prop = x.x)
public_m_opd_gov <- rename(public_m_opd_gov, gov = x.y)
public_m_opd_net <- rename(public_m_opd_net, net_prop = x.x)
public_m_opd_net <- rename(public_m_opd_net, net = x.y)
public_m_opd_net2 <- rename(public_m_opd_net2, net2_prop = x.x)
public_m_opd_net2 <- rename(public_m_opd_net2, net2 = x.y)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg_prop = x.x)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg = x.y)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2_prop = x.x)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2 = x.y)


# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(public_p_ipd_gov, public_p_ipd_net, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, public_p_ipd_net2, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg2, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(public_p_opd_gov, public_p_opd_net, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, public_p_opd_net2, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg2, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(public_d_ipd_gov, public_d_ipd_net, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, public_d_ipd_net2, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg2, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(public_d_opd_gov, public_d_opd_net, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, public_d_opd_net2, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg2, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(public_m_ipd_gov, public_m_ipd_net, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, public_m_ipd_net2, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg2, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(public_m_opd_gov, public_m_opd_net, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, public_m_opd_net2, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg2, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)

# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_p_ipd <- mutate(uga_p_ipd, gov_cumul = cumsum(gov))
uga_p_ipd <- mutate(uga_p_ipd, net_prop_cumul = cumsum(net_prop))
uga_p_ipd <- mutate(uga_p_ipd, net_cumul = cumsum(net))
uga_p_ipd <- mutate(uga_p_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_p_ipd <- mutate(uga_p_ipd, net2_cumul = cumsum(net2))
uga_p_ipd <- mutate(uga_p_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg_cumul = cumsum(crg))
uga_p_ipd <- mutate(uga_p_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg2_cumul = cumsum(crg2))

uga_p_opd <- mutate(uga_p_opd, gov_prop_cumul = cumsum(gov_prop))
uga_p_opd <- mutate(uga_p_opd, gov_cumul = cumsum(gov))
uga_p_opd <- mutate(uga_p_opd, net_prop_cumul = cumsum(net_prop))
uga_p_opd <- mutate(uga_p_opd, net_cumul = cumsum(net))
uga_p_opd <- mutate(uga_p_opd, net2_prop_cumul = cumsum(net2_prop))
uga_p_opd <- mutate(uga_p_opd, net2_cumul = cumsum(net2))
uga_p_opd <- mutate(uga_p_opd, crg_prop_cumul = cumsum(crg_prop))
uga_p_opd <- mutate(uga_p_opd, crg_cumul = cumsum(crg))
uga_p_opd <- mutate(uga_p_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_opd <- mutate(uga_p_opd, crg2_cumul = cumsum(crg2))

uga_d_ipd <- mutate(uga_d_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_d_ipd <- mutate(uga_d_ipd, gov_cumul = cumsum(gov))
uga_d_ipd <- mutate(uga_d_ipd, net_prop_cumul = cumsum(net_prop))
uga_d_ipd <- mutate(uga_d_ipd, net_cumul = cumsum(net))
uga_d_ipd <- mutate(uga_d_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_d_ipd <- mutate(uga_d_ipd, net2_cumul = cumsum(net2))
uga_d_ipd <- mutate(uga_d_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg_cumul = cumsum(crg))
uga_d_ipd <- mutate(uga_d_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg2_cumul = cumsum(crg2))

uga_d_opd <- mutate(uga_d_opd, gov_prop_cumul = cumsum(gov_prop))
uga_d_opd <- mutate(uga_d_opd, gov_cumul = cumsum(gov))
uga_d_opd <- mutate(uga_d_opd, net_prop_cumul = cumsum(net_prop))
uga_d_opd <- mutate(uga_d_opd, net_cumul = cumsum(net))
uga_d_opd <- mutate(uga_d_opd, net2_prop_cumul = cumsum(net2_prop))
uga_d_opd <- mutate(uga_d_opd, net2_cumul = cumsum(net2))
uga_d_opd <- mutate(uga_d_opd, crg_prop_cumul = cumsum(crg_prop))
uga_d_opd <- mutate(uga_d_opd, crg_cumul = cumsum(crg))
uga_d_opd <- mutate(uga_d_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_opd <- mutate(uga_d_opd, crg2_cumul = cumsum(crg2))

uga_m_ipd <- mutate(uga_m_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_m_ipd <- mutate(uga_m_ipd, gov_cumul = cumsum(gov))
uga_m_ipd <- mutate(uga_m_ipd, net_prop_cumul = cumsum(net_prop))
uga_m_ipd <- mutate(uga_m_ipd, net_cumul = cumsum(net))
uga_m_ipd <- mutate(uga_m_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_m_ipd <- mutate(uga_m_ipd, net2_cumul = cumsum(net2))
uga_m_ipd <- mutate(uga_m_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg_cumul = cumsum(crg))
uga_m_ipd <- mutate(uga_m_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg2_cumul = cumsum(crg2))

uga_m_opd <- mutate(uga_m_opd, gov_prop_cumul = cumsum(gov_prop))
uga_m_opd <- mutate(uga_m_opd, gov_cumul = cumsum(gov))
uga_m_opd <- mutate(uga_m_opd, net_prop_cumul = cumsum(net_prop))
uga_m_opd <- mutate(uga_m_opd, net_cumul = cumsum(net))
uga_m_opd <- mutate(uga_m_opd, net2_prop_cumul = cumsum(net2_prop))
uga_m_opd <- mutate(uga_m_opd, net2_cumul = cumsum(net2))
uga_m_opd <- mutate(uga_m_opd, crg_prop_cumul = cumsum(crg_prop))
uga_m_opd <- mutate(uga_m_opd, crg_cumul = cumsum(crg))
uga_m_opd <- mutate(uga_m_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_opd <- mutate(uga_m_opd, crg2_cumul = cumsum(crg2))



# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)


# Calculate
concentration_uga_gov_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$gov)) * cov(df_concentration_uga_p_ipd$gov, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_gov_p_opd <- round((2/mean(df_concentration_uga_p_opd$gov)) * cov(df_concentration_uga_p_opd$gov, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_gov_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$gov)) * cov(df_concentration_uga_d_ipd$gov, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_gov_d_opd <- round((2/mean(df_concentration_uga_d_opd$gov)) * cov(df_concentration_uga_d_opd$gov, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_gov_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$gov)) * cov(df_concentration_uga_m_ipd$gov, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_gov_m_opd <- round((2/mean(df_concentration_uga_m_opd$gov)) * cov(df_concentration_uga_m_opd$gov, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_net_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$net)) * cov(df_concentration_uga_p_ipd$net, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_net_p_opd <- round((2/mean(df_concentration_uga_p_opd$net)) * cov(df_concentration_uga_p_opd$net, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_net_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$net)) * cov(df_concentration_uga_d_ipd$net, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_net_d_opd <- round((2/mean(df_concentration_uga_d_opd$net)) * cov(df_concentration_uga_d_opd$net, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_net_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$net)) * cov(df_concentration_uga_m_ipd$net, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_net_m_opd <- round((2/mean(df_concentration_uga_m_opd$net)) * cov(df_concentration_uga_m_opd$net, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg)) * cov(df_concentration_uga_p_ipd$crg, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg)) * cov(df_concentration_uga_p_opd$crg, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg)) * cov(df_concentration_uga_d_ipd$crg, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg)) * cov(df_concentration_uga_d_opd$crg, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg)) * cov(df_concentration_uga_m_ipd$crg, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg)) * cov(df_concentration_uga_m_opd$crg, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg2_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg2)) * cov(df_concentration_uga_p_ipd$crg2, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg2_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg2)) * cov(df_concentration_uga_p_opd$crg2, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg2_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg2)) * cov(df_concentration_uga_d_ipd$crg2, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg2_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg2)) * cov(df_concentration_uga_d_opd$crg2, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg2_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg2)) * cov(df_concentration_uga_m_ipd$crg2, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg2_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg2)) * cov(df_concentration_uga_m_opd$crg2, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
public_gov <- c(concentration_uga_gov_p_ipd, concentration_uga_gov_p_opd, concentration_uga_gov_d_ipd, concentration_uga_gov_d_opd, concentration_uga_gov_m_ipd, concentration_uga_gov_m_opd)
public_net <- c(concentration_uga_net_p_ipd, concentration_uga_net_p_opd, concentration_uga_net_d_ipd, concentration_uga_net_d_opd, concentration_uga_net_m_ipd, concentration_uga_net_m_opd)
oop <- c(concentration_uga_crg_p_ipd, concentration_uga_crg_p_opd, concentration_uga_crg_d_ipd, concentration_uga_crg_d_opd, concentration_uga_crg_m_ipd, concentration_uga_crg_m_opd)
oop_all <- c(concentration_uga_crg2_p_ipd, concentration_uga_crg2_p_opd, concentration_uga_crg2_d_ipd, concentration_uga_crg2_d_opd, concentration_uga_crg2_m_ipd, concentration_uga_crg2_m_opd)
wealth_distrib <- c('income', 'income', 'income', 'income', 'income', 'income')
wealth_source <- c('wb', 'wb', 'wb', 'wb', 'wb', 'wb')
concentration_uga_inc_wb <- data.frame(disease, visittype, public_gov, public_net, oop, oop_all, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_gov <- graph_gov(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_gov_p_ipd, ci2 = concentration_uga_net_p_ipd)
graph_p_ipd_crg <- graph_crg(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_crg_p_ipd, ci2 = concentration_uga_crg2_p_ipd)
graph_p_ipd_gov_freq <- graph_freq(dataset = public_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_ipd_crg_freq <- graph_freq(dataset = ugasoc_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_gov <- graph_gov(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_gov_p_opd, ci2 = concentration_uga_net_p_opd)
graph_p_opd_crg <- graph_crg(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_crg_p_opd, ci2 = concentration_uga_crg2_p_opd)
graph_p_opd_gov_freq <- graph_freq(dataset = public_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_opd_crg_freq <- graph_freq(dataset = ugasoc_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov,graph_p_ipd_crg,graph_p_opd_gov,graph_p_opd_crg)
dev.off()
jpeg(filename = paste0("graph_p_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov_freq,graph_p_ipd_crg_freq,graph_p_opd_gov_freq,graph_p_opd_crg_freq)
dev.off()


# Diarrhea
graph_d_ipd_gov <- graph_gov(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_gov_d_ipd, ci2 = concentration_uga_net_d_ipd)
graph_d_ipd_crg <- graph_crg(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_crg_d_ipd, ci2 = concentration_uga_crg2_d_ipd)
graph_d_ipd_gov_freq <- graph_freq(dataset = public_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_ipd_crg_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_gov <- graph_gov(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_gov_d_opd, ci2 = concentration_uga_net_d_opd)
graph_d_opd_crg <- graph_crg(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_crg_d_opd, ci2 = concentration_uga_crg2_d_opd)
graph_d_opd_gov_freq <- graph_freq(dataset = public_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_opd_crg_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov,graph_d_ipd_crg,graph_d_opd_gov,graph_d_opd_crg)
dev.off()
jpeg(filename = paste0("graph_d_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov_freq,graph_d_ipd_crg_freq,graph_d_opd_gov_freq,graph_d_opd_crg_freq)
dev.off()

# Measles 
graph_m_ipd_gov <- graph_gov(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_gov_m_ipd, ci2 = concentration_uga_net_m_ipd)
graph_m_ipd_crg <- graph_crg(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_crg_m_ipd, ci2 = concentration_uga_crg2_m_ipd)
graph_m_ipd_gov_freq <- graph_freq(dataset = public_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_ipd_crg_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_gov <- graph_gov(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_gov_m_opd, ci2 = concentration_uga_net_m_opd)
graph_m_opd_crg <- graph_crg(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_crg_m_opd, ci2 = concentration_uga_crg2_m_opd)
graph_m_opd_gov_freq <- graph_freq(dataset = public_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_opd_crg_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov,graph_m_ipd_crg,graph_m_opd_gov,graph_m_opd_crg)
dev.off()
jpeg(filename = paste0("graph_m_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov_freq,graph_m_ipd_crg_freq,graph_m_opd_gov_freq,graph_m_opd_crg_freq)
dev.off()



# Separate income distribution for comparison
hhhead_income_p_ipd_wb <- ugasoc_p_ipd %>% select(c3_caretakerid, hhhead_income_wb)
hhhead_income_p_opd_wb <- ugasoc_p_opd %>% select(c3_caretakerid, hhhead_income_wb)
hhhead_income_d_ipd_wb <- ugasoc_d_ipd %>% select(c3_caretakerid, hhhead_income_wb)
hhhead_income_d_opd_wb <- ugasoc_d_opd %>% select(c3_caretakerid, hhhead_income_wb)
hhhead_income_m_ipd_wb <- ugasoc_m_ipd %>% select(c3_caretakerid, hhhead_income_wb)
hhhead_income_m_opd_wb <- ugasoc_m_opd %>% select(c3_caretakerid, hhhead_income_wb)



################################################
# PHE & OOP based on CONSUMPTION (sample)
################################################

print("Using sample expenditure estimates")
user0 <- 2
user1 <- 0


# Sum proportion and cumulative amounts of healthcare spendings quantile/decile/centile
public_p_ipd_gov <- aggregate(public_p_ipd$dmc_facility_prop, by=list(Centile=public_p_ipd$hhexp), FUN=sum)
public_p_ipd_gov_amount <- aggregate((public_p_ipd$dmc_facility + public_p_ipd$dmc_caregiver_c), by=list(Centile=public_p_ipd$hhexp), FUN=sum)
public_p_ipd_gov <- full_join_NA(x = public_p_ipd_gov, y = public_p_ipd_gov_amount, by = "Centile")
public_p_ipd_gov <- if(1 %in% public_p_ipd_gov$Centile == F) {add_row(public_p_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_gov}
public_p_ipd_net <- aggregate(public_p_ipd$dmc_facility_net_prop, by=list(Centile=public_p_ipd$hhexp), FUN=sum)
public_p_ipd_net_amount <- aggregate(public_p_ipd$dmc_facility, by=list(Centile=public_p_ipd$hhexp), FUN=sum)
public_p_ipd_net <- full_join_NA(x = public_p_ipd_net, y = public_p_ipd_net_amount, by = "Centile")
public_p_ipd_net <- if(1 %in% public_p_ipd_net$Centile == F) {add_row(public_p_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net}
public_p_ipd_net2 <- aggregate(public_p_ipd$dmc_facility_net2_prop, by=list(Centile=public_p_ipd$hhexp), FUN=sum)
public_p_ipd_net2_amount <- aggregate((public_p_ipd$dmc_facility - public_p_ipd$dmc_caregiver_ba), by=list(Centile=public_p_ipd$hhexp), FUN=sum)
public_p_ipd_net2 <- full_join_NA(x = public_p_ipd_net2, y = public_p_ipd_net2_amount, by = "Centile")
public_p_ipd_net2 <- if(1 %in% public_p_ipd_net2$Centile == F) {add_row(public_p_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net2}
ugasoc_p_ipd_crg <- aggregate(ugasoc_p_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhexp), FUN=sum)
ugasoc_p_ipd_crg_amount <- aggregate(ugasoc_p_ipd$dmc_caregiver, by=list(Centile=ugasoc_p_ipd$hhexp), FUN=sum)
ugasoc_p_ipd_crg <- full_join_NA(x = ugasoc_p_ipd_crg, y = ugasoc_p_ipd_crg_amount, by = "Centile")
ugasoc_p_ipd_crg <- if(1 %in% ugasoc_p_ipd_crg$Centile == F) {add_row(ugasoc_p_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg}
ugasoc_p_ipd_crg2 <- aggregate(ugasoc_p_ipd$all_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhexp), FUN=sum)
ugasoc_p_ipd_crg2_amount <- aggregate(ugasoc_p_ipd$all_caregiver, by=list(Centile=ugasoc_p_ipd$hhexp), FUN=sum)
ugasoc_p_ipd_crg2 <- full_join_NA(x = ugasoc_p_ipd_crg2, y = ugasoc_p_ipd_crg2_amount, by = "Centile")
ugasoc_p_ipd_crg2 <- if(1 %in% ugasoc_p_ipd_crg2$Centile == F) {add_row(ugasoc_p_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg2}
public_p_ipd_gov <- arrange(public_p_ipd_gov, Centile)
public_p_ipd_net <- arrange(public_p_ipd_net, Centile)
public_p_ipd_net2 <- arrange(public_p_ipd_net2, Centile)
ugasoc_p_ipd_crg <- arrange(ugasoc_p_ipd_crg, Centile)
ugasoc_p_ipd_crg2 <- arrange(ugasoc_p_ipd_crg2, Centile)

public_p_opd_gov <- aggregate(public_p_opd$dmc_facility_prop, by=list(Centile=public_p_opd$hhexp), FUN=sum)
public_p_opd_gov_amount <- aggregate((public_p_opd$dmc_facility + public_p_opd$dmc_caregiver_c), by=list(Centile=public_p_opd$hhexp), FUN=sum)
public_p_opd_gov <- full_join_NA(x = public_p_opd_gov, y = public_p_opd_gov_amount, by = "Centile")
public_p_opd_gov <- if(1 %in% public_p_opd_gov$Centile == F) {add_row(public_p_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_gov}
public_p_opd_net <- aggregate(public_p_opd$dmc_facility_net_prop, by=list(Centile=public_p_opd$hhexp), FUN=sum)
public_p_opd_net_amount <- aggregate(public_p_opd$dmc_facility, by=list(Centile=public_p_opd$hhexp), FUN=sum)
public_p_opd_net <- full_join_NA(x = public_p_opd_net, y = public_p_opd_net_amount, by = "Centile")
public_p_opd_net <- if(1 %in% public_p_opd_net$Centile == F) {add_row(public_p_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net}
public_p_opd_net2 <- aggregate(public_p_opd$dmc_facility_net2_prop, by=list(Centile=public_p_opd$hhexp), FUN=sum)
public_p_opd_net2_amount <- aggregate((public_p_opd$dmc_facility - public_p_opd$dmc_caregiver_ba), by=list(Centile=public_p_opd$hhexp), FUN=sum)
public_p_opd_net2 <- full_join_NA(x = public_p_opd_net2, y = public_p_opd_net2_amount, by = "Centile")
public_p_opd_net2 <- if(1 %in% public_p_opd_net2$Centile == F) {add_row(public_p_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net2}
ugasoc_p_opd_crg <- aggregate(ugasoc_p_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhexp), FUN=sum)
ugasoc_p_opd_crg_amount <- aggregate(ugasoc_p_opd$dmc_caregiver, by=list(Centile=ugasoc_p_opd$hhexp), FUN=sum)
ugasoc_p_opd_crg <- full_join_NA(x = ugasoc_p_opd_crg, y = ugasoc_p_opd_crg_amount, by = "Centile")
ugasoc_p_opd_crg <- if(1 %in% ugasoc_p_opd_crg$Centile == F) {add_row(ugasoc_p_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg}
ugasoc_p_opd_crg2 <- aggregate(ugasoc_p_opd$all_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhexp), FUN=sum)
ugasoc_p_opd_crg2_amount <- aggregate(ugasoc_p_opd$all_caregiver, by=list(Centile=ugasoc_p_opd$hhexp), FUN=sum)
ugasoc_p_opd_crg2 <- full_join_NA(x = ugasoc_p_opd_crg2, y = ugasoc_p_opd_crg2_amount, by = "Centile")
ugasoc_p_opd_crg2 <- if(1 %in% ugasoc_p_opd_crg2$Centile == F) {add_row(ugasoc_p_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg2}
public_p_opd_gov <- arrange(public_p_opd_gov, Centile)
public_p_opd_net <- arrange(public_p_opd_net, Centile)
public_p_opd_net2 <- arrange(public_p_opd_net2, Centile)
ugasoc_p_opd_crg <- arrange(ugasoc_p_opd_crg, Centile)
ugasoc_p_opd_crg2 <- arrange(ugasoc_p_opd_crg2, Centile)

public_d_ipd_gov <- aggregate(public_d_ipd$dmc_facility_prop, by=list(Centile=public_d_ipd$hhexp), FUN=sum)
public_d_ipd_gov_amount <- aggregate((public_d_ipd$dmc_facility + public_d_ipd$dmc_caregiver_c), by=list(Centile=public_d_ipd$hhexp), FUN=sum)
public_d_ipd_gov <- full_join_NA(x = public_d_ipd_gov, y = public_d_ipd_gov_amount, by = "Centile")
public_d_ipd_gov <- if(1 %in% public_d_ipd_gov$Centile == F) {add_row(public_d_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_gov}
public_d_ipd_net <- aggregate(public_d_ipd$dmc_facility_net_prop, by=list(Centile=public_d_ipd$hhexp), FUN=sum)
public_d_ipd_net_amount <- aggregate(public_d_ipd$dmc_facility, by=list(Centile=public_d_ipd$hhexp), FUN=sum)
public_d_ipd_net <- full_join_NA(x = public_d_ipd_net, y = public_d_ipd_net_amount, by = "Centile")
public_d_ipd_net <- if(1 %in% public_d_ipd_net$Centile == F) {add_row(public_d_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net}
public_d_ipd_net2 <- aggregate(public_d_ipd$dmc_facility_net2_prop, by=list(Centile=public_d_ipd$hhexp), FUN=sum)
public_d_ipd_net2_amount <- aggregate((public_d_ipd$dmc_facility - public_d_ipd$dmc_caregiver_ba), by=list(Centile=public_d_ipd$hhexp), FUN=sum)
public_d_ipd_net2 <- full_join_NA(x = public_d_ipd_net2, y = public_d_ipd_net2_amount, by = "Centile")
public_d_ipd_net2 <- if(1 %in% public_d_ipd_net2$Centile == F) {add_row(public_d_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net2}
ugasoc_d_ipd_crg <- aggregate(ugasoc_d_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_crg_amount <- aggregate(ugasoc_d_ipd$dmc_caregiver, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_crg <- full_join_NA(x = ugasoc_d_ipd_crg, y = ugasoc_d_ipd_crg_amount, by = "Centile")
ugasoc_d_ipd_crg <- if(1 %in% ugasoc_d_ipd_crg$Centile == F) {add_row(ugasoc_d_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg}
ugasoc_d_ipd_crg2 <- aggregate(ugasoc_d_ipd$all_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_crg2_amount <- aggregate(ugasoc_d_ipd$all_caregiver, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_crg2 <- full_join_NA(x = ugasoc_d_ipd_crg2, y = ugasoc_d_ipd_crg2_amount, by = "Centile")
ugasoc_d_ipd_crg2 <- if(1 %in% ugasoc_d_ipd_crg2$Centile == F) {add_row(ugasoc_d_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg2}
public_d_ipd_gov <- arrange(public_d_ipd_gov, Centile)
public_d_ipd_net <- arrange(public_d_ipd_net, Centile)
public_d_ipd_net2 <- arrange(public_d_ipd_net2, Centile)
ugasoc_d_ipd_crg <- arrange(ugasoc_d_ipd_crg, Centile)
ugasoc_d_ipd_crg2 <- arrange(ugasoc_d_ipd_crg2, Centile)

public_d_opd_gov <- aggregate(public_d_opd$dmc_facility_prop, by=list(Centile=public_d_opd$hhexp), FUN=sum)
public_d_opd_gov_amount <- aggregate((public_d_opd$dmc_facility + public_d_opd$dmc_caregiver_c), by=list(Centile=public_d_opd$hhexp), FUN=sum)
public_d_opd_gov <- full_join_NA(x = public_d_opd_gov, y = public_d_opd_gov_amount, by = "Centile")
public_d_opd_gov <- if(1 %in% public_d_opd_gov$Centile == F) {add_row(public_d_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_gov}
public_d_opd_net <- aggregate(public_d_opd$dmc_facility_net_prop, by=list(Centile=public_d_opd$hhexp), FUN=sum)
public_d_opd_net_amount <- aggregate(public_d_opd$dmc_facility, by=list(Centile=public_d_opd$hhexp), FUN=sum)
public_d_opd_net <- full_join_NA(x = public_d_opd_net, y = public_d_opd_net_amount, by = "Centile")
public_d_opd_net <- if(1 %in% public_d_opd_net$Centile == F) {add_row(public_d_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net}
public_d_opd_net2 <- aggregate(public_d_opd$dmc_facility_net2_prop, by=list(Centile=public_d_opd$hhexp), FUN=sum)
public_d_opd_net2_amount <- aggregate((public_d_opd$dmc_facility - public_d_opd$dmc_caregiver_ba), by=list(Centile=public_d_opd$hhexp), FUN=sum)
public_d_opd_net2 <- full_join_NA(x = public_d_opd_net2, y = public_d_opd_net2_amount, by = "Centile")
public_d_opd_net2 <- if(1 %in% public_d_opd_net2$Centile == F) {add_row(public_d_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net2}
ugasoc_d_opd_crg <- aggregate(ugasoc_d_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_crg_amount <- aggregate(ugasoc_d_opd$dmc_caregiver, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_crg <- full_join_NA(x = ugasoc_d_opd_crg, y = ugasoc_d_opd_crg_amount, by = "Centile")
ugasoc_d_opd_crg <- if(1 %in% ugasoc_d_opd_crg$Centile == F) {add_row(ugasoc_d_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg}
ugasoc_d_opd_crg2 <- aggregate(ugasoc_d_opd$all_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_crg2_amount <- aggregate(ugasoc_d_opd$all_caregiver, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_crg2 <- full_join_NA(x = ugasoc_d_opd_crg2, y = ugasoc_d_opd_crg2_amount, by = "Centile")
ugasoc_d_opd_crg2 <- if(1 %in% ugasoc_d_opd_crg2$Centile == F) {add_row(ugasoc_d_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg2}
public_d_opd_gov <- arrange(public_d_opd_gov, Centile)
public_d_opd_net <- arrange(public_d_opd_net, Centile)
public_d_opd_net2 <- arrange(public_d_opd_net2, Centile)
ugasoc_d_opd_crg <- arrange(ugasoc_d_opd_crg, Centile)
ugasoc_d_opd_crg2 <- arrange(ugasoc_d_opd_crg2, Centile)

public_m_ipd_gov <- aggregate(public_m_ipd$dmc_facility_prop, by=list(Centile=public_m_ipd$hhexp), FUN=sum)
public_m_ipd_gov_amount <- aggregate((public_m_ipd$dmc_facility + public_m_ipd$dmc_caregiver_c), by=list(Centile=public_m_ipd$hhexp), FUN=sum)
public_m_ipd_gov <- full_join_NA(x = public_m_ipd_gov, y = public_m_ipd_gov_amount, by = "Centile")
public_m_ipd_gov <- if(1 %in% public_m_ipd_gov$Centile == F) {add_row(public_m_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_gov}
public_m_ipd_net <- aggregate(public_m_ipd$dmc_facility_net_prop, by=list(Centile=public_m_ipd$hhexp), FUN=sum)
public_m_ipd_net_amount <- aggregate(public_m_ipd$dmc_facility, by=list(Centile=public_m_ipd$hhexp), FUN=sum)
public_m_ipd_net <- full_join_NA(x = public_m_ipd_net, y = public_m_ipd_net_amount, by = "Centile")
public_m_ipd_net <- if(1 %in% public_m_ipd_net$Centile == F) {add_row(public_m_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net}
public_m_ipd_net2 <- aggregate(public_m_ipd$dmc_facility_net2_prop, by=list(Centile=public_m_ipd$hhexp), FUN=sum)
public_m_ipd_net2_amount <- aggregate((public_m_ipd$dmc_facility - public_m_ipd$dmc_caregiver_ba), by=list(Centile=public_m_ipd$hhexp), FUN=sum)
public_m_ipd_net2 <- full_join_NA(x = public_m_ipd_net2, y = public_m_ipd_net2_amount, by = "Centile")
public_m_ipd_net2 <- if(1 %in% public_m_ipd_net2$Centile == F) {add_row(public_m_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net2}
ugasoc_m_ipd_crg <- aggregate(ugasoc_m_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_crg_amount <- aggregate(ugasoc_m_ipd$dmc_caregiver, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_crg <- full_join_NA(x = ugasoc_m_ipd_crg, y = ugasoc_m_ipd_crg_amount, by = "Centile")
ugasoc_m_ipd_crg <- if(1 %in% ugasoc_m_ipd_crg$Centile == F) {add_row(ugasoc_m_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg}
ugasoc_m_ipd_crg2 <- aggregate(ugasoc_m_ipd$all_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_crg2_amount <- aggregate(ugasoc_m_ipd$all_caregiver, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_crg2 <- full_join_NA(x = ugasoc_m_ipd_crg2, y = ugasoc_m_ipd_crg2_amount, by = "Centile")
ugasoc_m_ipd_crg2 <- if(1 %in% ugasoc_m_ipd_crg2$Centile == F) {add_row(ugasoc_m_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg2}
public_m_ipd_gov <- arrange(public_m_ipd_gov, Centile)
public_m_ipd_net <- arrange(public_m_ipd_net, Centile)
public_m_ipd_net2 <- arrange(public_m_ipd_net2, Centile)
ugasoc_m_ipd_crg <- arrange(ugasoc_m_ipd_crg, Centile)
ugasoc_m_ipd_crg2 <- arrange(ugasoc_m_ipd_crg2, Centile)

public_m_opd_gov <- aggregate(public_m_opd$dmc_facility_prop, by=list(Centile=public_m_opd$hhexp), FUN=sum)
public_m_opd_gov_amount <- aggregate((public_m_opd$dmc_facility + public_m_opd$dmc_caregiver_c), by=list(Centile=public_m_opd$hhexp), FUN=sum)
public_m_opd_gov <- full_join_NA(x = public_m_opd_gov, y = public_m_opd_gov_amount, by = "Centile")
public_m_opd_gov <- if(1 %in% public_m_opd_gov$Centile == F) {add_row(public_m_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_gov}
public_m_opd_net <- aggregate(public_m_opd$dmc_facility_net_prop, by=list(Centile=public_m_opd$hhexp), FUN=sum)
public_m_opd_net_amount <- aggregate(public_m_opd$dmc_facility, by=list(Centile=public_m_opd$hhexp), FUN=sum)
public_m_opd_net <- full_join_NA(x = public_m_opd_net, y = public_m_opd_net_amount, by = "Centile")
public_m_opd_net <- if(1 %in% public_m_opd_net$Centile == F) {add_row(public_m_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net}
public_m_opd_net2 <- aggregate(public_m_opd$dmc_facility_net2_prop, by=list(Centile=public_m_opd$hhexp), FUN=sum)
public_m_opd_net2_amount <- aggregate((public_m_opd$dmc_facility - public_m_opd$dmc_caregiver_ba), by=list(Centile=public_m_opd$hhexp), FUN=sum)
public_m_opd_net2 <- full_join_NA(x = public_m_opd_net2, y = public_m_opd_net2_amount, by = "Centile")
public_m_opd_net2 <- if(1 %in% public_m_opd_net2$Centile == F) {add_row(public_m_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net2}
ugasoc_m_opd_crg <- aggregate(ugasoc_m_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_crg_amount <- aggregate(ugasoc_m_opd$dmc_caregiver, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_crg <- full_join_NA(x = ugasoc_m_opd_crg, y = ugasoc_m_opd_crg_amount, by = "Centile")
ugasoc_m_opd_crg <- if(1 %in% ugasoc_m_opd_crg$Centile == F) {add_row(ugasoc_m_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg}
ugasoc_m_opd_crg2 <- aggregate(ugasoc_m_opd$all_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_crg2_amount <- aggregate(ugasoc_m_opd$all_caregiver, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_crg2 <- full_join_NA(x = ugasoc_m_opd_crg2, y = ugasoc_m_opd_crg2_amount, by = "Centile")
ugasoc_m_opd_crg2 <- if(1 %in% ugasoc_m_opd_crg2$Centile == F) {add_row(ugasoc_m_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg2}
public_m_opd_gov <- arrange(public_m_opd_gov, Centile)
public_m_opd_net <- arrange(public_m_opd_net, Centile)
public_m_opd_net2 <- arrange(public_m_opd_net2, Centile)
ugasoc_m_opd_crg <- arrange(ugasoc_m_opd_crg, Centile)
ugasoc_m_opd_crg2 <- arrange(ugasoc_m_opd_crg, Centile)



# Rename variables
public_p_ipd_gov <- rename(public_p_ipd_gov, gov_prop = x.x)
public_p_ipd_gov <- rename(public_p_ipd_gov, gov = x.y)
public_p_ipd_net <- rename(public_p_ipd_net, net_prop = x.x)
public_p_ipd_net <- rename(public_p_ipd_net, net = x.y)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2_prop = x.x)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2 = x.y)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg_prop = x.x)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg = x.y)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2_prop = x.x)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2 = x.y)

public_p_opd_gov <- rename(public_p_opd_gov, gov_prop = x.x)
public_p_opd_gov <- rename(public_p_opd_gov, gov = x.y)
public_p_opd_net <- rename(public_p_opd_net, net_prop = x.x)
public_p_opd_net <- rename(public_p_opd_net, net = x.y)
public_p_opd_net2 <- rename(public_p_opd_net2, net2_prop = x.x)
public_p_opd_net2 <- rename(public_p_opd_net2, net2 = x.y)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg_prop = x.x)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg = x.y)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2_prop = x.x)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2 = x.y)

public_d_ipd_gov <- rename(public_d_ipd_gov, gov_prop = x.x)
public_d_ipd_gov <- rename(public_d_ipd_gov, gov = x.y)
public_d_ipd_net <- rename(public_d_ipd_net, net_prop = x.x)
public_d_ipd_net <- rename(public_d_ipd_net, net = x.y)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2_prop = x.x)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2 = x.y)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg_prop = x.x)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg = x.y)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2_prop = x.x)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2 = x.y)

public_d_opd_gov <- rename(public_d_opd_gov, gov_prop = x.x)
public_d_opd_gov <- rename(public_d_opd_gov, gov = x.y)
public_d_opd_net <- rename(public_d_opd_net, net_prop = x.x)
public_d_opd_net <- rename(public_d_opd_net, net = x.y)
public_d_opd_net2 <- rename(public_d_opd_net2, net2_prop = x.x)
public_d_opd_net2 <- rename(public_d_opd_net2, net2 = x.y)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg_prop = x.x)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg = x.y)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2_prop = x.x)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2 = x.y)

public_m_ipd_gov <- rename(public_m_ipd_gov, gov_prop = x.x)
public_m_ipd_gov <- rename(public_m_ipd_gov, gov = x.y)
public_m_ipd_net <- rename(public_m_ipd_net, net_prop = x.x)
public_m_ipd_net <- rename(public_m_ipd_net, net = x.y)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2_prop = x.x)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2 = x.y)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg_prop = x.x)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg = x.y)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2_prop = x.x)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2 = x.y)

public_m_opd_gov <- rename(public_m_opd_gov, gov_prop = x.x)
public_m_opd_gov <- rename(public_m_opd_gov, gov = x.y)
public_m_opd_net <- rename(public_m_opd_net, net_prop = x.x)
public_m_opd_net <- rename(public_m_opd_net, net = x.y)
public_m_opd_net2 <- rename(public_m_opd_net2, net2_prop = x.x)
public_m_opd_net2 <- rename(public_m_opd_net2, net2 = x.y)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg_prop = x.x)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg = x.y)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2_prop = x.x)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2 = x.y)


# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(public_p_ipd_gov, public_p_ipd_net, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, public_p_ipd_net2, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg2, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(public_p_opd_gov, public_p_opd_net, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, public_p_opd_net2, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg2, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(public_d_ipd_gov, public_d_ipd_net, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, public_d_ipd_net2, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg2, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(public_d_opd_gov, public_d_opd_net, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, public_d_opd_net2, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg2, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(public_m_ipd_gov, public_m_ipd_net, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, public_m_ipd_net2, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg2, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(public_m_opd_gov, public_m_opd_net, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, public_m_opd_net2, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg2, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)

# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_p_ipd <- mutate(uga_p_ipd, gov_cumul = cumsum(gov))
uga_p_ipd <- mutate(uga_p_ipd, net_prop_cumul = cumsum(net_prop))
uga_p_ipd <- mutate(uga_p_ipd, net_cumul = cumsum(net))
uga_p_ipd <- mutate(uga_p_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_p_ipd <- mutate(uga_p_ipd, net2_cumul = cumsum(net2))
uga_p_ipd <- mutate(uga_p_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg_cumul = cumsum(crg))
uga_p_ipd <- mutate(uga_p_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg2_cumul = cumsum(crg2))

uga_p_opd <- mutate(uga_p_opd, gov_prop_cumul = cumsum(gov_prop))
uga_p_opd <- mutate(uga_p_opd, gov_cumul = cumsum(gov))
uga_p_opd <- mutate(uga_p_opd, net_prop_cumul = cumsum(net_prop))
uga_p_opd <- mutate(uga_p_opd, net_cumul = cumsum(net))
uga_p_opd <- mutate(uga_p_opd, net2_prop_cumul = cumsum(net2_prop))
uga_p_opd <- mutate(uga_p_opd, net2_cumul = cumsum(net2))
uga_p_opd <- mutate(uga_p_opd, crg_prop_cumul = cumsum(crg_prop))
uga_p_opd <- mutate(uga_p_opd, crg_cumul = cumsum(crg))
uga_p_opd <- mutate(uga_p_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_opd <- mutate(uga_p_opd, crg2_cumul = cumsum(crg2))

uga_d_ipd <- mutate(uga_d_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_d_ipd <- mutate(uga_d_ipd, gov_cumul = cumsum(gov))
uga_d_ipd <- mutate(uga_d_ipd, net_prop_cumul = cumsum(net_prop))
uga_d_ipd <- mutate(uga_d_ipd, net_cumul = cumsum(net))
uga_d_ipd <- mutate(uga_d_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_d_ipd <- mutate(uga_d_ipd, net2_cumul = cumsum(net2))
uga_d_ipd <- mutate(uga_d_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg_cumul = cumsum(crg))
uga_d_ipd <- mutate(uga_d_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg2_cumul = cumsum(crg2))

uga_d_opd <- mutate(uga_d_opd, gov_prop_cumul = cumsum(gov_prop))
uga_d_opd <- mutate(uga_d_opd, gov_cumul = cumsum(gov))
uga_d_opd <- mutate(uga_d_opd, net_prop_cumul = cumsum(net_prop))
uga_d_opd <- mutate(uga_d_opd, net_cumul = cumsum(net))
uga_d_opd <- mutate(uga_d_opd, net2_prop_cumul = cumsum(net2_prop))
uga_d_opd <- mutate(uga_d_opd, net2_cumul = cumsum(net2))
uga_d_opd <- mutate(uga_d_opd, crg_prop_cumul = cumsum(crg_prop))
uga_d_opd <- mutate(uga_d_opd, crg_cumul = cumsum(crg))
uga_d_opd <- mutate(uga_d_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_opd <- mutate(uga_d_opd, crg2_cumul = cumsum(crg2))

uga_m_ipd <- mutate(uga_m_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_m_ipd <- mutate(uga_m_ipd, gov_cumul = cumsum(gov))
uga_m_ipd <- mutate(uga_m_ipd, net_prop_cumul = cumsum(net_prop))
uga_m_ipd <- mutate(uga_m_ipd, net_cumul = cumsum(net))
uga_m_ipd <- mutate(uga_m_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_m_ipd <- mutate(uga_m_ipd, net2_cumul = cumsum(net2))
uga_m_ipd <- mutate(uga_m_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg_cumul = cumsum(crg))
uga_m_ipd <- mutate(uga_m_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg2_cumul = cumsum(crg2))

uga_m_opd <- mutate(uga_m_opd, gov_prop_cumul = cumsum(gov_prop))
uga_m_opd <- mutate(uga_m_opd, gov_cumul = cumsum(gov))
uga_m_opd <- mutate(uga_m_opd, net_prop_cumul = cumsum(net_prop))
uga_m_opd <- mutate(uga_m_opd, net_cumul = cumsum(net))
uga_m_opd <- mutate(uga_m_opd, net2_prop_cumul = cumsum(net2_prop))
uga_m_opd <- mutate(uga_m_opd, net2_cumul = cumsum(net2))
uga_m_opd <- mutate(uga_m_opd, crg_prop_cumul = cumsum(crg_prop))
uga_m_opd <- mutate(uga_m_opd, crg_cumul = cumsum(crg))
uga_m_opd <- mutate(uga_m_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_opd <- mutate(uga_m_opd, crg2_cumul = cumsum(crg2))



# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)


# Calculate
concentration_uga_gov_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$gov)) * cov(df_concentration_uga_p_ipd$gov, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_gov_p_opd <- round((2/mean(df_concentration_uga_p_opd$gov)) * cov(df_concentration_uga_p_opd$gov, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_gov_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$gov)) * cov(df_concentration_uga_d_ipd$gov, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_gov_d_opd <- round((2/mean(df_concentration_uga_d_opd$gov)) * cov(df_concentration_uga_d_opd$gov, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_gov_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$gov)) * cov(df_concentration_uga_m_ipd$gov, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_gov_m_opd <- round((2/mean(df_concentration_uga_m_opd$gov)) * cov(df_concentration_uga_m_opd$gov, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_net_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$net)) * cov(df_concentration_uga_p_ipd$net, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_net_p_opd <- round((2/mean(df_concentration_uga_p_opd$net)) * cov(df_concentration_uga_p_opd$net, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_net_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$net)) * cov(df_concentration_uga_d_ipd$net, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_net_d_opd <- round((2/mean(df_concentration_uga_d_opd$net)) * cov(df_concentration_uga_d_opd$net, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_net_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$net)) * cov(df_concentration_uga_m_ipd$net, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_net_m_opd <- round((2/mean(df_concentration_uga_m_opd$net)) * cov(df_concentration_uga_m_opd$net, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg)) * cov(df_concentration_uga_p_ipd$crg, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg)) * cov(df_concentration_uga_p_opd$crg, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg)) * cov(df_concentration_uga_d_ipd$crg, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg)) * cov(df_concentration_uga_d_opd$crg, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg)) * cov(df_concentration_uga_m_ipd$crg, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg)) * cov(df_concentration_uga_m_opd$crg, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg2_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg2)) * cov(df_concentration_uga_p_ipd$crg2, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg2_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg2)) * cov(df_concentration_uga_p_opd$crg2, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg2_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg2)) * cov(df_concentration_uga_d_ipd$crg2, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg2_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg2)) * cov(df_concentration_uga_d_opd$crg2, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg2_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg2)) * cov(df_concentration_uga_m_ipd$crg2, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg2_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg2)) * cov(df_concentration_uga_m_opd$crg2, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
public_gov <- c(concentration_uga_gov_p_ipd, concentration_uga_gov_p_opd, concentration_uga_gov_d_ipd, concentration_uga_gov_d_opd, concentration_uga_gov_m_ipd, concentration_uga_gov_m_opd)
public_net <- c(concentration_uga_net_p_ipd, concentration_uga_net_p_opd, concentration_uga_net_d_ipd, concentration_uga_net_d_opd, concentration_uga_net_m_ipd, concentration_uga_net_m_opd)
oop <- c(concentration_uga_crg_p_ipd, concentration_uga_crg_p_opd, concentration_uga_crg_d_ipd, concentration_uga_crg_d_opd, concentration_uga_crg_m_ipd, concentration_uga_crg_m_opd)
oop_all <- c(concentration_uga_crg2_p_ipd, concentration_uga_crg2_p_opd, concentration_uga_crg2_d_ipd, concentration_uga_crg2_d_opd, concentration_uga_crg2_m_ipd, concentration_uga_crg2_m_opd)
wealth_distrib <- c('consumption', 'consumption', 'consumption', 'consumption', 'consumption', 'consumption')
wealth_source <- c('sample', 'sample', 'sample', 'sample', 'sample', 'sample')
concentration_uga_exp <- data.frame(disease, visittype, public_gov, public_net, oop, oop_all, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_gov <- graph_gov(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_gov_p_ipd, ci2 = concentration_uga_net_p_ipd)
graph_p_ipd_crg <- graph_crg(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_crg_p_ipd, ci2 = concentration_uga_crg2_p_ipd)
graph_p_ipd_gov_freq <- graph_freq(dataset = public_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_ipd_crg_freq <- graph_freq(dataset = ugasoc_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_gov <- graph_gov(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_gov_p_opd, ci2 = concentration_uga_net_p_opd)
graph_p_opd_crg <- graph_crg(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_crg_p_opd, ci2 = concentration_uga_crg2_p_opd)
graph_p_opd_gov_freq <- graph_freq(dataset = public_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_opd_crg_freq <- graph_freq(dataset = ugasoc_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov,graph_p_ipd_crg,graph_p_opd_gov,graph_p_opd_crg)
dev.off()
jpeg(filename = paste0("graph_p_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov_freq,graph_p_ipd_crg_freq,graph_p_opd_gov_freq,graph_p_opd_crg_freq)
dev.off()


# Diarrhea
graph_d_ipd_gov <- graph_gov(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_gov_d_ipd, ci2 = concentration_uga_net_d_ipd)
graph_d_ipd_crg <- graph_crg(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_crg_d_ipd, ci2 = concentration_uga_crg2_d_ipd)
graph_d_ipd_gov_freq <- graph_freq(dataset = public_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_ipd_crg_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_gov <- graph_gov(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_gov_d_opd, ci2 = concentration_uga_net_d_opd)
graph_d_opd_crg <- graph_crg(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_crg_d_opd, ci2 = concentration_uga_crg2_d_opd)
graph_d_opd_gov_freq <- graph_freq(dataset = public_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_opd_crg_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov,graph_d_ipd_crg,graph_d_opd_gov,graph_d_opd_crg)
dev.off()
jpeg(filename = paste0("graph_d_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov_freq,graph_d_ipd_crg_freq,graph_d_opd_gov_freq,graph_d_opd_crg_freq)
dev.off()

# Measles 
graph_m_ipd_gov <- graph_gov(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_gov_m_ipd, ci2 = concentration_uga_net_m_ipd)
graph_m_ipd_crg <- graph_crg(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_crg_m_ipd, ci2 = concentration_uga_crg2_m_ipd)
graph_m_ipd_gov_freq <- graph_freq(dataset = public_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_ipd_crg_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_gov <- graph_gov(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_gov_m_opd, ci2 = concentration_uga_net_m_opd)
graph_m_opd_crg <- graph_crg(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_crg_m_opd, ci2 = concentration_uga_crg2_m_opd)
graph_m_opd_gov_freq <- graph_freq(dataset = public_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_opd_crg_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov,graph_m_ipd_crg,graph_m_opd_gov,graph_m_opd_crg)
dev.off()
jpeg(filename = paste0("graph_m_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov_freq,graph_m_ipd_crg_freq,graph_m_opd_gov_freq,graph_m_opd_crg_freq)
dev.off()




# Separate income distribution for comparison
hhexp_p_ipd <- ugasoc_p_ipd %>% select(c3_caretakerid, hhexp)
hhexp_p_opd <- ugasoc_p_opd %>% select(c3_caretakerid, hhexp)
hhexp_d_ipd <- ugasoc_d_ipd %>% select(c3_caretakerid, hhexp)
hhexp_d_opd <- ugasoc_d_opd %>% select(c3_caretakerid, hhexp)
hhexp_m_ipd <- ugasoc_m_ipd %>% select(c3_caretakerid, hhexp)
hhexp_m_opd <- ugasoc_m_opd %>% select(c3_caretakerid, hhexp)



################################################
# PHE & OOP based on CONSUMPTION (World Bank)
################################################
user0 <- 2
user1 <- 1


# Sum proportion and cumulative amounts of healthcare spendings quantile/decile/centile
public_p_ipd_gov <- aggregate(public_p_ipd$dmc_facility_prop, by=list(Centile=public_p_ipd$hhexp_wb), FUN=sum)
public_p_ipd_gov_amount <- aggregate((public_p_ipd$dmc_facility + public_p_ipd$dmc_caregiver_c), by=list(Centile=public_p_ipd$hhexp_wb), FUN=sum)
public_p_ipd_gov <- full_join_NA(x = public_p_ipd_gov, y = public_p_ipd_gov_amount, by = "Centile")
public_p_ipd_gov <- if(1 %in% public_p_ipd_gov$Centile == F) {add_row(public_p_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_gov}
public_p_ipd_net <- aggregate(public_p_ipd$dmc_facility_net_prop, by=list(Centile=public_p_ipd$hhexp_wb), FUN=sum)
public_p_ipd_net_amount <- aggregate(public_p_ipd$dmc_facility, by=list(Centile=public_p_ipd$hhexp_wb), FUN=sum)
public_p_ipd_net <- full_join_NA(x = public_p_ipd_net, y = public_p_ipd_net_amount, by = "Centile")
public_p_ipd_net <- if(1 %in% public_p_ipd_net$Centile == F) {add_row(public_p_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net}
public_p_ipd_net2 <- aggregate(public_p_ipd$dmc_facility_net2_prop, by=list(Centile=public_p_ipd$hhexp_wb), FUN=sum)
public_p_ipd_net2_amount <- aggregate((public_p_ipd$dmc_facility - public_p_ipd$dmc_caregiver_ba), by=list(Centile=public_p_ipd$hhexp_wb), FUN=sum)
public_p_ipd_net2 <- full_join_NA(x = public_p_ipd_net2, y = public_p_ipd_net2_amount, by = "Centile")
public_p_ipd_net2 <- if(1 %in% public_p_ipd_net2$Centile == F) {add_row(public_p_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_ipd_net2}
ugasoc_p_ipd_crg <- aggregate(ugasoc_p_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_crg_amount <- aggregate(ugasoc_p_ipd$dmc_caregiver, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_crg <- full_join_NA(x = ugasoc_p_ipd_crg, y = ugasoc_p_ipd_crg_amount, by = "Centile")
ugasoc_p_ipd_crg <- if(1 %in% ugasoc_p_ipd_crg$Centile == F) {add_row(ugasoc_p_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg}
ugasoc_p_ipd_crg2 <- aggregate(ugasoc_p_ipd$all_caregiver_prop, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_crg2_amount <- aggregate(ugasoc_p_ipd$all_caregiver, by=list(Centile=ugasoc_p_ipd$hhexp_wb), FUN=sum)
ugasoc_p_ipd_crg2 <- full_join_NA(x = ugasoc_p_ipd_crg2, y = ugasoc_p_ipd_crg2_amount, by = "Centile")
ugasoc_p_ipd_crg2 <- if(1 %in% ugasoc_p_ipd_crg2$Centile == F) {add_row(ugasoc_p_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_crg2}
public_p_ipd_gov <- arrange(public_p_ipd_gov, Centile)
public_p_ipd_net <- arrange(public_p_ipd_net, Centile)
public_p_ipd_net2 <- arrange(public_p_ipd_net2, Centile)
ugasoc_p_ipd_crg <- arrange(ugasoc_p_ipd_crg, Centile)
ugasoc_p_ipd_crg2 <- arrange(ugasoc_p_ipd_crg2, Centile)

public_p_opd_gov <- aggregate(public_p_opd$dmc_facility_prop, by=list(Centile=public_p_opd$hhexp_wb), FUN=sum)
public_p_opd_gov_amount <- aggregate((public_p_opd$dmc_facility + public_p_opd$dmc_caregiver_c), by=list(Centile=public_p_opd$hhexp_wb), FUN=sum)
public_p_opd_gov <- full_join_NA(x = public_p_opd_gov, y = public_p_opd_gov_amount, by = "Centile")
public_p_opd_gov <- if(1 %in% public_p_opd_gov$Centile == F) {add_row(public_p_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_gov}
public_p_opd_net <- aggregate(public_p_opd$dmc_facility_net_prop, by=list(Centile=public_p_opd$hhexp_wb), FUN=sum)
public_p_opd_net_amount <- aggregate(public_p_opd$dmc_facility, by=list(Centile=public_p_opd$hhexp_wb), FUN=sum)
public_p_opd_net <- full_join_NA(x = public_p_opd_net, y = public_p_opd_net_amount, by = "Centile")
public_p_opd_net <- if(1 %in% public_p_opd_net$Centile == F) {add_row(public_p_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net}
public_p_opd_net2 <- aggregate(public_p_opd$dmc_facility_net2_prop, by=list(Centile=public_p_opd$hhexp_wb), FUN=sum)
public_p_opd_net2_amount <- aggregate((public_p_opd$dmc_facility - public_p_opd$dmc_caregiver_ba), by=list(Centile=public_p_opd$hhexp_wb), FUN=sum)
public_p_opd_net2 <- full_join_NA(x = public_p_opd_net2, y = public_p_opd_net2_amount, by = "Centile")
public_p_opd_net2 <- if(1 %in% public_p_opd_net2$Centile == F) {add_row(public_p_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_p_opd_net2}
ugasoc_p_opd_crg <- aggregate(ugasoc_p_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_crg_amount <- aggregate(ugasoc_p_opd$dmc_caregiver, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_crg <- full_join_NA(x = ugasoc_p_opd_crg, y = ugasoc_p_opd_crg_amount, by = "Centile")
ugasoc_p_opd_crg <- if(1 %in% ugasoc_p_opd_crg$Centile == F) {add_row(ugasoc_p_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg}
ugasoc_p_opd_crg2 <- aggregate(ugasoc_p_opd$all_caregiver_prop, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_crg2_amount <- aggregate(ugasoc_p_opd$all_caregiver, by=list(Centile=ugasoc_p_opd$hhexp_wb), FUN=sum)
ugasoc_p_opd_crg2 <- full_join_NA(x = ugasoc_p_opd_crg2, y = ugasoc_p_opd_crg2_amount, by = "Centile")
ugasoc_p_opd_crg2 <- if(1 %in% ugasoc_p_opd_crg2$Centile == F) {add_row(ugasoc_p_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_crg2}
public_p_opd_gov <- arrange(public_p_opd_gov, Centile)
public_p_opd_net <- arrange(public_p_opd_net, Centile)
public_p_opd_net2 <- arrange(public_p_opd_net2, Centile)
ugasoc_p_opd_crg <- arrange(ugasoc_p_opd_crg, Centile)
ugasoc_p_opd_crg2 <- arrange(ugasoc_p_opd_crg2, Centile)

public_d_ipd_gov <- aggregate(public_d_ipd$dmc_facility_prop, by=list(Centile=public_d_ipd$hhexp_wb), FUN=sum)
public_d_ipd_gov_amount <- aggregate((public_d_ipd$dmc_facility + public_d_ipd$dmc_caregiver_c), by=list(Centile=public_d_ipd$hhexp_wb), FUN=sum)
public_d_ipd_gov <- full_join_NA(x = public_d_ipd_gov, y = public_d_ipd_gov_amount, by = "Centile")
public_d_ipd_gov <- if(1 %in% public_d_ipd_gov$Centile == F) {add_row(public_d_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_gov}
public_d_ipd_net <- aggregate(public_d_ipd$dmc_facility_net_prop, by=list(Centile=public_d_ipd$hhexp_wb), FUN=sum)
public_d_ipd_net_amount <- aggregate(public_d_ipd$dmc_facility, by=list(Centile=public_d_ipd$hhexp_wb), FUN=sum)
public_d_ipd_net <- full_join_NA(x = public_d_ipd_net, y = public_d_ipd_net_amount, by = "Centile")
public_d_ipd_net <- if(1 %in% public_d_ipd_net$Centile == F) {add_row(public_d_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net}
public_d_ipd_net2 <- aggregate(public_d_ipd$dmc_facility_net2_prop, by=list(Centile=public_d_ipd$hhexp_wb), FUN=sum)
public_d_ipd_net2_amount <- aggregate((public_d_ipd$dmc_facility - public_d_ipd$dmc_caregiver_ba), by=list(Centile=public_d_ipd$hhexp_wb), FUN=sum)
public_d_ipd_net2 <- full_join_NA(x = public_d_ipd_net2, y = public_d_ipd_net2_amount, by = "Centile")
public_d_ipd_net2 <- if(1 %in% public_d_ipd_net2$Centile == F) {add_row(public_d_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_ipd_net2}
ugasoc_d_ipd_crg <- aggregate(ugasoc_d_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_crg_amount <- aggregate(ugasoc_d_ipd$dmc_caregiver, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_crg <- full_join_NA(x = ugasoc_d_ipd_crg, y = ugasoc_d_ipd_crg_amount, by = "Centile")
ugasoc_d_ipd_crg <- if(1 %in% ugasoc_d_ipd_crg$Centile == F) {add_row(ugasoc_d_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg}
ugasoc_d_ipd_crg2 <- aggregate(ugasoc_d_ipd$all_caregiver_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_crg2_amount <- aggregate(ugasoc_d_ipd$all_caregiver, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_crg2 <- full_join_NA(x = ugasoc_d_ipd_crg2, y = ugasoc_d_ipd_crg2_amount, by = "Centile")
ugasoc_d_ipd_crg2 <- if(1 %in% ugasoc_d_ipd_crg2$Centile == F) {add_row(ugasoc_d_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_crg2}
public_d_ipd_gov <- arrange(public_d_ipd_gov, Centile)
public_d_ipd_net <- arrange(public_d_ipd_net, Centile)
public_d_ipd_net2 <- arrange(public_d_ipd_net2, Centile)
ugasoc_d_ipd_crg <- arrange(ugasoc_d_ipd_crg, Centile)
ugasoc_d_ipd_crg2 <- arrange(ugasoc_d_ipd_crg2, Centile)

public_d_opd_gov <- aggregate(public_d_opd$dmc_facility_prop, by=list(Centile=public_d_opd$hhexp_wb), FUN=sum)
public_d_opd_gov_amount <- aggregate((public_d_opd$dmc_facility + public_d_opd$dmc_caregiver_c), by=list(Centile=public_d_opd$hhexp_wb), FUN=sum)
public_d_opd_gov <- full_join_NA(x = public_d_opd_gov, y = public_d_opd_gov_amount, by = "Centile")
public_d_opd_gov <- if(1 %in% public_d_opd_gov$Centile == F) {add_row(public_d_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_gov}
public_d_opd_net <- aggregate(public_d_opd$dmc_facility_net_prop, by=list(Centile=public_d_opd$hhexp_wb), FUN=sum)
public_d_opd_net_amount <- aggregate(public_d_opd$dmc_facility, by=list(Centile=public_d_opd$hhexp_wb), FUN=sum)
public_d_opd_net <- full_join_NA(x = public_d_opd_net, y = public_d_opd_net_amount, by = "Centile")
public_d_opd_net <- if(1 %in% public_d_opd_net$Centile == F) {add_row(public_d_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net}
public_d_opd_net2 <- aggregate(public_d_opd$dmc_facility_net2_prop, by=list(Centile=public_d_opd$hhexp_wb), FUN=sum)
public_d_opd_net2_amount <- aggregate((public_d_opd$dmc_facility - public_d_opd$dmc_caregiver_ba), by=list(Centile=public_d_opd$hhexp_wb), FUN=sum)
public_d_opd_net2 <- full_join_NA(x = public_d_opd_net2, y = public_d_opd_net2_amount, by = "Centile")
public_d_opd_net2 <- if(1 %in% public_d_opd_net2$Centile == F) {add_row(public_d_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_d_opd_net2}
ugasoc_d_opd_crg <- aggregate(ugasoc_d_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_crg_amount <- aggregate(ugasoc_d_opd$dmc_caregiver, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_crg <- full_join_NA(x = ugasoc_d_opd_crg, y = ugasoc_d_opd_crg_amount, by = "Centile")
ugasoc_d_opd_crg <- if(1 %in% ugasoc_d_opd_crg$Centile == F) {add_row(ugasoc_d_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg}
ugasoc_d_opd_crg2 <- aggregate(ugasoc_d_opd$all_caregiver_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_crg2_amount <- aggregate(ugasoc_d_opd$all_caregiver, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_crg2 <- full_join_NA(x = ugasoc_d_opd_crg2, y = ugasoc_d_opd_crg2_amount, by = "Centile")
ugasoc_d_opd_crg2 <- if(1 %in% ugasoc_d_opd_crg2$Centile == F) {add_row(ugasoc_d_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_crg2}
public_d_opd_gov <- arrange(public_d_opd_gov, Centile)
public_d_opd_net <- arrange(public_d_opd_net, Centile)
public_d_opd_net2 <- arrange(public_d_opd_net2, Centile)
ugasoc_d_opd_crg <- arrange(ugasoc_d_opd_crg, Centile)
ugasoc_d_opd_crg2 <- arrange(ugasoc_d_opd_crg2, Centile)

public_m_ipd_gov <- aggregate(public_m_ipd$dmc_facility_prop, by=list(Centile=public_m_ipd$hhexp_wb), FUN=sum)
public_m_ipd_gov_amount <- aggregate((public_m_ipd$dmc_facility + public_m_ipd$dmc_caregiver_c), by=list(Centile=public_m_ipd$hhexp_wb), FUN=sum)
public_m_ipd_gov <- full_join_NA(x = public_m_ipd_gov, y = public_m_ipd_gov_amount, by = "Centile")
public_m_ipd_gov <- if(1 %in% public_m_ipd_gov$Centile == F) {add_row(public_m_ipd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_gov}
public_m_ipd_net <- aggregate(public_m_ipd$dmc_facility_net_prop, by=list(Centile=public_m_ipd$hhexp_wb), FUN=sum)
public_m_ipd_net_amount <- aggregate(public_m_ipd$dmc_facility, by=list(Centile=public_m_ipd$hhexp_wb), FUN=sum)
public_m_ipd_net <- full_join_NA(x = public_m_ipd_net, y = public_m_ipd_net_amount, by = "Centile")
public_m_ipd_net <- if(1 %in% public_m_ipd_net$Centile == F) {add_row(public_m_ipd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net}
public_m_ipd_net2 <- aggregate(public_m_ipd$dmc_facility_net2_prop, by=list(Centile=public_m_ipd$hhexp_wb), FUN=sum)
public_m_ipd_net2_amount <- aggregate((public_m_ipd$dmc_facility - public_m_ipd$dmc_caregiver_ba), by=list(Centile=public_m_ipd$hhexp_wb), FUN=sum)
public_m_ipd_net2 <- full_join_NA(x = public_m_ipd_net2, y = public_m_ipd_net2_amount, by = "Centile")
public_m_ipd_net2 <- if(1 %in% public_m_ipd_net2$Centile == F) {add_row(public_m_ipd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_ipd_net2}
ugasoc_m_ipd_crg <- aggregate(ugasoc_m_ipd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_crg_amount <- aggregate(ugasoc_m_ipd$dmc_caregiver, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_crg <- full_join_NA(x = ugasoc_m_ipd_crg, y = ugasoc_m_ipd_crg_amount, by = "Centile")
ugasoc_m_ipd_crg <- if(1 %in% ugasoc_m_ipd_crg$Centile == F) {add_row(ugasoc_m_ipd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg}
ugasoc_m_ipd_crg2 <- aggregate(ugasoc_m_ipd$all_caregiver_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_crg2_amount <- aggregate(ugasoc_m_ipd$all_caregiver, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_crg2 <- full_join_NA(x = ugasoc_m_ipd_crg2, y = ugasoc_m_ipd_crg2_amount, by = "Centile")
ugasoc_m_ipd_crg2 <- if(1 %in% ugasoc_m_ipd_crg2$Centile == F) {add_row(ugasoc_m_ipd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_crg2}
public_m_ipd_gov <- arrange(public_m_ipd_gov, Centile)
public_m_ipd_net <- arrange(public_m_ipd_net, Centile)
public_m_ipd_net2 <- arrange(public_m_ipd_net2, Centile)
ugasoc_m_ipd_crg <- arrange(ugasoc_m_ipd_crg, Centile)
ugasoc_m_ipd_crg2 <- arrange(ugasoc_m_ipd_crg2, Centile)

public_m_opd_gov <- aggregate(public_m_opd$dmc_facility_prop, by=list(Centile=public_m_opd$hhexp_wb), FUN=sum)
public_m_opd_gov_amount <- aggregate((public_m_opd$dmc_facility + public_m_opd$dmc_caregiver_c), by=list(Centile=public_m_opd$hhexp_wb), FUN=sum)
public_m_opd_gov <- full_join_NA(x = public_m_opd_gov, y = public_m_opd_gov_amount, by = "Centile")
public_m_opd_gov <- if(1 %in% public_m_opd_gov$Centile == F) {add_row(public_m_opd_gov, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_gov}
public_m_opd_net <- aggregate(public_m_opd$dmc_facility_net_prop, by=list(Centile=public_m_opd$hhexp_wb), FUN=sum)
public_m_opd_net_amount <- aggregate(public_m_opd$dmc_facility, by=list(Centile=public_m_opd$hhexp_wb), FUN=sum)
public_m_opd_net <- full_join_NA(x = public_m_opd_net, y = public_m_opd_net_amount, by = "Centile")
public_m_opd_net <- if(1 %in% public_m_opd_net$Centile == F) {add_row(public_m_opd_net, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net}
public_m_opd_net2 <- aggregate(public_m_opd$dmc_facility_net2_prop, by=list(Centile=public_m_opd$hhexp_wb), FUN=sum)
public_m_opd_net2_amount <- aggregate((public_m_opd$dmc_facility - public_m_opd$dmc_caregiver_ba), by=list(Centile=public_m_opd$hhexp_wb), FUN=sum)
public_m_opd_net2 <- full_join_NA(x = public_m_opd_net2, y = public_m_opd_net2_amount, by = "Centile")
public_m_opd_net2 <- if(1 %in% public_m_opd_net2$Centile == F) {add_row(public_m_opd_net2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {public_m_opd_net2}
ugasoc_m_opd_crg <- aggregate(ugasoc_m_opd$dmc_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_crg_amount <- aggregate(ugasoc_m_opd$dmc_caregiver, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_crg <- full_join_NA(x = ugasoc_m_opd_crg, y = ugasoc_m_opd_crg_amount, by = "Centile")
ugasoc_m_opd_crg <- if(1 %in% ugasoc_m_opd_crg$Centile == F) {add_row(ugasoc_m_opd_crg, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg}
ugasoc_m_opd_crg2 <- aggregate(ugasoc_m_opd$all_caregiver_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_crg2_amount <- aggregate(ugasoc_m_opd$all_caregiver, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_crg2 <- full_join_NA(x = ugasoc_m_opd_crg2, y = ugasoc_m_opd_crg2_amount, by = "Centile")
ugasoc_m_opd_crg2 <- if(1 %in% ugasoc_m_opd_crg2$Centile == F) {add_row(ugasoc_m_opd_crg2, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_crg2}
public_m_opd_gov <- arrange(public_m_opd_gov, Centile)
public_m_opd_net <- arrange(public_m_opd_net, Centile)
public_m_opd_net2 <- arrange(public_m_opd_net2, Centile)
ugasoc_m_opd_crg <- arrange(ugasoc_m_opd_crg, Centile)
ugasoc_m_opd_crg2 <- arrange(ugasoc_m_opd_crg2, Centile)



# Rename variables
public_p_ipd_gov <- rename(public_p_ipd_gov, gov_prop = x.x)
public_p_ipd_gov <- rename(public_p_ipd_gov, gov = x.y)
public_p_ipd_net <- rename(public_p_ipd_net, net_prop = x.x)
public_p_ipd_net <- rename(public_p_ipd_net, net = x.y)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2_prop = x.x)
public_p_ipd_net2 <- rename(public_p_ipd_net2, net2 = x.y)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg_prop = x.x)
ugasoc_p_ipd_crg <- rename(ugasoc_p_ipd_crg, crg = x.y)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2_prop = x.x)
ugasoc_p_ipd_crg2 <- rename(ugasoc_p_ipd_crg2, crg2 = x.y)

public_p_opd_gov <- rename(public_p_opd_gov, gov_prop = x.x)
public_p_opd_gov <- rename(public_p_opd_gov, gov = x.y)
public_p_opd_net <- rename(public_p_opd_net, net_prop = x.x)
public_p_opd_net <- rename(public_p_opd_net, net = x.y)
public_p_opd_net2 <- rename(public_p_opd_net2, net2_prop = x.x)
public_p_opd_net2 <- rename(public_p_opd_net2, net2 = x.y)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg_prop = x.x)
ugasoc_p_opd_crg <- rename(ugasoc_p_opd_crg, crg = x.y)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2_prop = x.x)
ugasoc_p_opd_crg2 <- rename(ugasoc_p_opd_crg2, crg2 = x.y)

public_d_ipd_gov <- rename(public_d_ipd_gov, gov_prop = x.x)
public_d_ipd_gov <- rename(public_d_ipd_gov, gov = x.y)
public_d_ipd_net <- rename(public_d_ipd_net, net_prop = x.x)
public_d_ipd_net <- rename(public_d_ipd_net, net = x.y)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2_prop = x.x)
public_d_ipd_net2 <- rename(public_d_ipd_net2, net2 = x.y)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg_prop = x.x)
ugasoc_d_ipd_crg <- rename(ugasoc_d_ipd_crg, crg = x.y)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2_prop = x.x)
ugasoc_d_ipd_crg2 <- rename(ugasoc_d_ipd_crg2, crg2 = x.y)

public_d_opd_gov <- rename(public_d_opd_gov, gov_prop = x.x)
public_d_opd_gov <- rename(public_d_opd_gov, gov = x.y)
public_d_opd_net <- rename(public_d_opd_net, net_prop = x.x)
public_d_opd_net <- rename(public_d_opd_net, net = x.y)
public_d_opd_net2 <- rename(public_d_opd_net2, net2_prop = x.x)
public_d_opd_net2 <- rename(public_d_opd_net2, net2 = x.y)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg_prop = x.x)
ugasoc_d_opd_crg <- rename(ugasoc_d_opd_crg, crg = x.y)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2_prop = x.x)
ugasoc_d_opd_crg2 <- rename(ugasoc_d_opd_crg2, crg2 = x.y)

public_m_ipd_gov <- rename(public_m_ipd_gov, gov_prop = x.x)
public_m_ipd_gov <- rename(public_m_ipd_gov, gov = x.y)
public_m_ipd_net <- rename(public_m_ipd_net, net_prop = x.x)
public_m_ipd_net <- rename(public_m_ipd_net, net = x.y)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2_prop = x.x)
public_m_ipd_net2 <- rename(public_m_ipd_net2, net2 = x.y)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg_prop = x.x)
ugasoc_m_ipd_crg <- rename(ugasoc_m_ipd_crg, crg = x.y)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2_prop = x.x)
ugasoc_m_ipd_crg2 <- rename(ugasoc_m_ipd_crg2, crg2 = x.y)

public_m_opd_gov <- rename(public_m_opd_gov, gov_prop = x.x)
public_m_opd_gov <- rename(public_m_opd_gov, gov = x.y)
public_m_opd_net <- rename(public_m_opd_net, net_prop = x.x)
public_m_opd_net <- rename(public_m_opd_net, net = x.y)
public_m_opd_net2 <- rename(public_m_opd_net2, net2_prop = x.x)
public_m_opd_net2 <- rename(public_m_opd_net2, net2 = x.y)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg_prop = x.x)
ugasoc_m_opd_crg <- rename(ugasoc_m_opd_crg, crg = x.y)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2_prop = x.x)
ugasoc_m_opd_crg2 <- rename(ugasoc_m_opd_crg2, crg2 = x.y)


# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(public_p_ipd_gov, public_p_ipd_net, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, public_p_ipd_net2, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg, by = "Centile")
uga_p_ipd <- full_join_NA(uga_p_ipd, ugasoc_p_ipd_crg2, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(public_p_opd_gov, public_p_opd_net, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, public_p_opd_net2, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg, by = "Centile")
uga_p_opd <- full_join_NA(uga_p_opd, ugasoc_p_opd_crg2, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(public_d_ipd_gov, public_d_ipd_net, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, public_d_ipd_net2, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg, by = "Centile")
uga_d_ipd <- full_join_NA(uga_d_ipd, ugasoc_d_ipd_crg2, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(public_d_opd_gov, public_d_opd_net, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, public_d_opd_net2, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg, by = "Centile")
uga_d_opd <- full_join_NA(uga_d_opd, ugasoc_d_opd_crg2, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(public_m_ipd_gov, public_m_ipd_net, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, public_m_ipd_net2, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg, by = "Centile")
uga_m_ipd <- full_join_NA(uga_m_ipd, ugasoc_m_ipd_crg2, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(public_m_opd_gov, public_m_opd_net, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, public_m_opd_net2, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg, by = "Centile")
uga_m_opd <- full_join_NA(uga_m_opd, ugasoc_m_opd_crg2, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)

# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_p_ipd <- mutate(uga_p_ipd, gov_cumul = cumsum(gov))
uga_p_ipd <- mutate(uga_p_ipd, net_prop_cumul = cumsum(net_prop))
uga_p_ipd <- mutate(uga_p_ipd, net_cumul = cumsum(net))
uga_p_ipd <- mutate(uga_p_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_p_ipd <- mutate(uga_p_ipd, net2_cumul = cumsum(net2))
uga_p_ipd <- mutate(uga_p_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg_cumul = cumsum(crg))
uga_p_ipd <- mutate(uga_p_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_ipd <- mutate(uga_p_ipd, crg2_cumul = cumsum(crg2))

uga_p_opd <- mutate(uga_p_opd, gov_prop_cumul = cumsum(gov_prop))
uga_p_opd <- mutate(uga_p_opd, gov_cumul = cumsum(gov))
uga_p_opd <- mutate(uga_p_opd, net_prop_cumul = cumsum(net_prop))
uga_p_opd <- mutate(uga_p_opd, net_cumul = cumsum(net))
uga_p_opd <- mutate(uga_p_opd, net2_prop_cumul = cumsum(net2_prop))
uga_p_opd <- mutate(uga_p_opd, net2_cumul = cumsum(net2))
uga_p_opd <- mutate(uga_p_opd, crg_prop_cumul = cumsum(crg_prop))
uga_p_opd <- mutate(uga_p_opd, crg_cumul = cumsum(crg))
uga_p_opd <- mutate(uga_p_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_p_opd <- mutate(uga_p_opd, crg2_cumul = cumsum(crg2))

uga_d_ipd <- mutate(uga_d_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_d_ipd <- mutate(uga_d_ipd, gov_cumul = cumsum(gov))
uga_d_ipd <- mutate(uga_d_ipd, net_prop_cumul = cumsum(net_prop))
uga_d_ipd <- mutate(uga_d_ipd, net_cumul = cumsum(net))
uga_d_ipd <- mutate(uga_d_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_d_ipd <- mutate(uga_d_ipd, net2_cumul = cumsum(net2))
uga_d_ipd <- mutate(uga_d_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg_cumul = cumsum(crg))
uga_d_ipd <- mutate(uga_d_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_ipd <- mutate(uga_d_ipd, crg2_cumul = cumsum(crg2))

uga_d_opd <- mutate(uga_d_opd, gov_prop_cumul = cumsum(gov_prop))
uga_d_opd <- mutate(uga_d_opd, gov_cumul = cumsum(gov))
uga_d_opd <- mutate(uga_d_opd, net_prop_cumul = cumsum(net_prop))
uga_d_opd <- mutate(uga_d_opd, net_cumul = cumsum(net))
uga_d_opd <- mutate(uga_d_opd, net2_prop_cumul = cumsum(net2_prop))
uga_d_opd <- mutate(uga_d_opd, net2_cumul = cumsum(net2))
uga_d_opd <- mutate(uga_d_opd, crg_prop_cumul = cumsum(crg_prop))
uga_d_opd <- mutate(uga_d_opd, crg_cumul = cumsum(crg))
uga_d_opd <- mutate(uga_d_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_d_opd <- mutate(uga_d_opd, crg2_cumul = cumsum(crg2))

uga_m_ipd <- mutate(uga_m_ipd, gov_prop_cumul = cumsum(gov_prop))
uga_m_ipd <- mutate(uga_m_ipd, gov_cumul = cumsum(gov))
uga_m_ipd <- mutate(uga_m_ipd, net_prop_cumul = cumsum(net_prop))
uga_m_ipd <- mutate(uga_m_ipd, net_cumul = cumsum(net))
uga_m_ipd <- mutate(uga_m_ipd, net2_prop_cumul = cumsum(net2_prop))
uga_m_ipd <- mutate(uga_m_ipd, net2_cumul = cumsum(net2))
uga_m_ipd <- mutate(uga_m_ipd, crg_prop_cumul = cumsum(crg_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg_cumul = cumsum(crg))
uga_m_ipd <- mutate(uga_m_ipd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_ipd <- mutate(uga_m_ipd, crg2_cumul = cumsum(crg2))

uga_m_opd <- mutate(uga_m_opd, gov_prop_cumul = cumsum(gov_prop))
uga_m_opd <- mutate(uga_m_opd, gov_cumul = cumsum(gov))
uga_m_opd <- mutate(uga_m_opd, net_prop_cumul = cumsum(net_prop))
uga_m_opd <- mutate(uga_m_opd, net_cumul = cumsum(net))
uga_m_opd <- mutate(uga_m_opd, net2_prop_cumul = cumsum(net2_prop))
uga_m_opd <- mutate(uga_m_opd, net2_cumul = cumsum(net2))
uga_m_opd <- mutate(uga_m_opd, crg_prop_cumul = cumsum(crg_prop))
uga_m_opd <- mutate(uga_m_opd, crg_cumul = cumsum(crg))
uga_m_opd <- mutate(uga_m_opd, crg2_prop_cumul = cumsum(crg2_prop))
uga_m_opd <- mutate(uga_m_opd, crg2_cumul = cumsum(crg2))



# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)


# Calculate
concentration_uga_gov_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$gov)) * cov(df_concentration_uga_p_ipd$gov, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_gov_p_opd <- round((2/mean(df_concentration_uga_p_opd$gov)) * cov(df_concentration_uga_p_opd$gov, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_gov_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$gov)) * cov(df_concentration_uga_d_ipd$gov, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_gov_d_opd <- round((2/mean(df_concentration_uga_d_opd$gov)) * cov(df_concentration_uga_d_opd$gov, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_gov_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$gov)) * cov(df_concentration_uga_m_ipd$gov, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_gov_m_opd <- round((2/mean(df_concentration_uga_m_opd$gov)) * cov(df_concentration_uga_m_opd$gov, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_net_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$net)) * cov(df_concentration_uga_p_ipd$net, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_net_p_opd <- round((2/mean(df_concentration_uga_p_opd$net)) * cov(df_concentration_uga_p_opd$net, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_net_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$net)) * cov(df_concentration_uga_d_ipd$net, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_net_d_opd <- round((2/mean(df_concentration_uga_d_opd$net)) * cov(df_concentration_uga_d_opd$net, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_net_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$net)) * cov(df_concentration_uga_m_ipd$net, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_net_m_opd <- round((2/mean(df_concentration_uga_m_opd$net)) * cov(df_concentration_uga_m_opd$net, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg)) * cov(df_concentration_uga_p_ipd$crg, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg)) * cov(df_concentration_uga_p_opd$crg, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg)) * cov(df_concentration_uga_d_ipd$crg, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg)) * cov(df_concentration_uga_d_opd$crg, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg)) * cov(df_concentration_uga_m_ipd$crg, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg)) * cov(df_concentration_uga_m_opd$crg, df_concentration_uga_m_opd$Centile), digits = 4)

concentration_uga_crg2_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$crg2)) * cov(df_concentration_uga_p_ipd$crg2, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_crg2_p_opd <- round((2/mean(df_concentration_uga_p_opd$crg2)) * cov(df_concentration_uga_p_opd$crg2, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_crg2_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$crg2)) * cov(df_concentration_uga_d_ipd$crg2, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_crg2_d_opd <- round((2/mean(df_concentration_uga_d_opd$crg2)) * cov(df_concentration_uga_d_opd$crg2, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_crg2_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$crg2)) * cov(df_concentration_uga_m_ipd$crg2, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_crg2_m_opd <- round((2/mean(df_concentration_uga_m_opd$crg2)) * cov(df_concentration_uga_m_opd$crg2, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
public_gov <- c(concentration_uga_gov_p_ipd, concentration_uga_gov_p_opd, concentration_uga_gov_d_ipd, concentration_uga_gov_d_opd, concentration_uga_gov_m_ipd, concentration_uga_gov_m_opd)
public_net <- c(concentration_uga_net_p_ipd, concentration_uga_net_p_opd, concentration_uga_net_d_ipd, concentration_uga_net_d_opd, concentration_uga_net_m_ipd, concentration_uga_net_m_opd)
oop <- c(concentration_uga_crg_p_ipd, concentration_uga_crg_p_opd, concentration_uga_crg_d_ipd, concentration_uga_crg_d_opd, concentration_uga_crg_m_ipd, concentration_uga_crg_m_opd)
oop_all <- c(concentration_uga_crg2_p_ipd, concentration_uga_crg2_p_opd, concentration_uga_crg2_d_ipd, concentration_uga_crg2_d_opd, concentration_uga_crg2_m_ipd, concentration_uga_crg2_m_opd)
wealth_distrib <- c('consumption', 'consumption', 'consumption', 'consumption', 'consumption', 'consumption')
wealth_source <- c('wb', 'wb', 'wb', 'wb', 'wb', 'wb')
concentration_uga_exp_wb <- data.frame(disease, visittype, public_gov, public_net, oop, oop_all, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_gov <- graph_gov(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_gov_p_ipd, ci2 = concentration_uga_net_p_ipd)
graph_p_ipd_crg <- graph_crg(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_crg_p_ipd, ci2 = concentration_uga_crg2_p_ipd)
graph_p_ipd_gov_freq <- graph_freq(dataset = public_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_ipd_crg_freq <- graph_freq(dataset = ugasoc_p_ipd, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_gov <- graph_gov(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_gov_p_opd, ci2 = concentration_uga_net_p_opd)
graph_p_opd_crg <- graph_crg(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_crg_p_opd, ci2 = concentration_uga_crg2_p_opd)
graph_p_opd_gov_freq <- graph_freq(dataset = public_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in public facilities")
graph_p_opd_crg_freq <- graph_freq(dataset = ugasoc_p_opd, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov,graph_p_ipd_crg,graph_p_opd_gov,graph_p_opd_crg)
dev.off()
jpeg(filename = paste0("graph_p_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_gov_freq,graph_p_ipd_crg_freq,graph_p_opd_gov_freq,graph_p_opd_crg_freq)
dev.off()


# Diarrhea
graph_d_ipd_gov <- graph_gov(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_gov_d_ipd, ci2 = concentration_uga_net_d_ipd)
graph_d_ipd_crg <- graph_crg(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_crg_d_ipd, ci2 = concentration_uga_crg2_d_ipd)
graph_d_ipd_gov_freq <- graph_freq(dataset = public_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_ipd_crg_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_gov <- graph_gov(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_gov_d_opd, ci2 = concentration_uga_net_d_opd)
graph_d_opd_crg <- graph_crg(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_crg_d_opd, ci2 = concentration_uga_crg2_d_opd)
graph_d_opd_gov_freq <- graph_freq(dataset = public_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in public facilities")
graph_d_opd_crg_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov,graph_d_ipd_crg,graph_d_opd_gov,graph_d_opd_crg)
dev.off()
jpeg(filename = paste0("graph_d_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_gov_freq,graph_d_ipd_crg_freq,graph_d_opd_gov_freq,graph_d_opd_crg_freq)
dev.off()

# Measles 
graph_m_ipd_gov <- graph_gov(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_gov_m_ipd, ci2 = concentration_uga_net_m_ipd)
graph_m_ipd_crg <- graph_crg(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_crg_m_ipd, ci2 = concentration_uga_crg2_m_ipd)
graph_m_ipd_gov_freq <- graph_freq(dataset = public_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_ipd_crg_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_gov <- graph_gov(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_gov_m_opd, ci2 = concentration_uga_net_m_opd)
graph_m_opd_crg <- graph_crg(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_crg_m_opd, ci2 = concentration_uga_crg2_m_opd)
graph_m_opd_gov_freq <- graph_freq(dataset = public_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in public facilities")
graph_m_opd_crg_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov,graph_m_ipd_crg,graph_m_opd_gov,graph_m_opd_crg)
dev.off()
jpeg(filename = paste0("graph_m_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_gov_freq,graph_m_ipd_crg_freq,graph_m_opd_gov_freq,graph_m_opd_crg_freq)
dev.off()




# Separate income distribution for comparison
hhexp_p_ipd_wb <- ugasoc_p_ipd %>% select(c3_caretakerid, hhexp_wb)
hhexp_p_opd_wb <- ugasoc_p_opd %>% select(c3_caretakerid, hhexp_wb)
hhexp_d_ipd_wb <- ugasoc_d_ipd %>% select(c3_caretakerid, hhexp_wb)
hhexp_d_opd_wb <- ugasoc_d_opd %>% select(c3_caretakerid, hhexp_wb)
hhexp_m_ipd_wb <- ugasoc_m_ipd %>% select(c3_caretakerid, hhexp_wb)
hhexp_m_opd_wb <- ugasoc_m_opd %>% select(c3_caretakerid, hhexp_wb)



################################################
# Generate concentration indexes CSV (PHE & OOP)
################################################

concentration_uga <- bind_rows(concentration_uga_inc, concentration_uga_inc_wb, concentration_uga_exp, concentration_uga_exp_wb)
write.csv(concentration_uga, file = paste0("concentration_uga_", write.time, ".csv"), col.names = TRUE, sep = ",")


################################################
# PHE & OOP regression decomposition analysis
################################################

# Merge IPD and OPD for each disease
ugasoc_p <- bind_rows(ugasoc_p_ipd, ugasoc_p_opd)
ugasoc_d <- bind_rows(ugasoc_d_ipd, ugasoc_d_opd)
ugasoc_m <- bind_rows(ugasoc_m_ipd, ugasoc_m_opd)
public_p <- bind_rows(public_p_ipd, public_p_opd)
public_d <- bind_rows(public_d_ipd, public_d_opd)
public_m <- bind_rows(public_m_ipd, public_m_opd)


# Check if centile or nearest
length(unique(ugasoc_p_ipd$hhhead_income_wb)) # n = 38
length(unique(ugasoc_p_opd$hhhead_income_wb)) # n = 37
length(unique(ugasoc_p_ipd$hhexp_wb)) # n = 94
length(unique(ugasoc_p_opd$hhexp_wb)) # n = 93
length(unique(ugasoc_d_ipd$hhhead_income_wb)) # n = 35
length(unique(ugasoc_d_opd$hhhead_income_wb)) # n = 46
length(unique(ugasoc_d_ipd$hhexp_wb)) # n = 91
length(unique(ugasoc_d_opd$hhexp_wb)) # n = 94
length(unique(ugasoc_m_ipd$hhhead_income_wb)) # n = 35
length(unique(ugasoc_m_opd$hhhead_income_wb)) # n = 14
length(unique(ugasoc_m_ipd$hhexp_wb)) # n = 59
length(unique(ugasoc_m_opd$hhexp_wb)) # n = 25
## Less than Centiles, use deciles instead

length(unique(ugasoc_p_ipd$hhhead_income_n10_wb)) # n = 12
length(unique(ugasoc_p_opd$hhhead_income_n10_wb)) # n = 12
length(unique(ugasoc_p_ipd$hhexp_n10_wb)) # n = 11
length(unique(ugasoc_p_opd$hhexp_n10_wb)) # n = 11
length(unique(ugasoc_d_ipd$hhhead_income_n10_wb)) # n = 11
length(unique(ugasoc_d_opd$hhhead_income_n10_wb)) # n = 12
length(unique(ugasoc_d_ipd$hhexp_n10_wb)) # n = 11
length(unique(ugasoc_d_opd$hhexp_n10_wb)) # n = 11
length(unique(ugasoc_m_ipd$hhhead_income_n10_wb)) # n = 12
length(unique(ugasoc_m_opd$hhhead_income_n10_wb)) # n = 9
length(unique(ugasoc_m_ipd$hhexp_n10_wb)) # n = 11
length(unique(ugasoc_m_opd$hhexp_n10_wb)) # n = 10
## OK


# MLR of PHE
## Includes public & private facilities


# Model fitting
## PHE (net benefit) - PNEUMONIA

lm_phe_p_ipd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_ipd)
summary(lm_phe_p_ipd)
AIC(lm_phe_p_ipd)


## OOP - PNEUMONIA
lm_oop_p_ipd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_ipd)
summary(lm_oop_p_ipd)
AIC(lm_oop_p_ipd)



# Generate models

# PHE
lm_phe_p_ipd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_ipd)
summary(lm_phe_p_ipd)
AIC(lm_phe_p_ipd)
lm_phe_p_opd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_opd)
summary(lm_phe_p_opd)
AIC(lm_phe_p_opd)
lm_phe_d_ipd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_ipd)
summary(lm_phe_d_ipd)
AIC(lm_phe_d_ipd)
lm_phe_d_opd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_opd)
summary(lm_phe_d_opd)
AIC(lm_phe_d_opd)
lm_phe_m_ipd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_ipd)
summary(lm_phe_m_ipd)
AIC(lm_phe_m_ipd)
lm_phe_m_opd <- lm(formula = dmc_facility ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_opd)
summary(lm_phe_m_opd)
AIC(lm_phe_m_opd)


# OOP
lm_oop_p_ipd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_ipd)
summary(lm_oop_p_ipd)
AIC(lm_oop_p_ipd)
lm_oop_p_opd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_opd)
summary(lm_oop_p_opd)
AIC(lm_oop_p_opd)
lm_oop_d_ipd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_ipd)
summary(lm_oop_d_ipd)
AIC(lm_oop_d_ipd)
lm_oop_d_opd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_opd)
summary(lm_oop_d_opd)
AIC(lm_oop_d_opd)
lm_oop_m_ipd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_ipd)
summary(lm_oop_m_ipd)
AIC(lm_oop_m_ipd)
lm_oop_m_opd <- lm(formula = all_caregiver ~ sector_public + time + distance + hhhead_income_n10_wb + hhexp_n10_wb + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_opd)
summary(lm_oop_m_opd)
AIC(lm_oop_m_opd)



# Show summary model stats
glance(lm_phe_p_ipd)
glance(lm_phe_p_opd)
glance(lm_phe_d_ipd)
glance(lm_phe_d_opd)
glance(lm_phe_m_ipd)
glance(lm_phe_m_opd)
glance(lm_oop_p_ipd)
glance(lm_oop_p_opd)
glance(lm_oop_d_ipd)
glance(lm_oop_d_opd)
glance(lm_oop_m_ipd)
glance(lm_oop_m_opd)


# n observations
nrow(ugasoc_p_ipd)
nrow(ugasoc_p_opd)
nrow(ugasoc_d_ipd)
nrow(ugasoc_d_opd)
nrow(ugasoc_m_ipd)
nrow(ugasoc_m_opd)


# Export regression results to CSV ("broom" package)
tidy_lm_phe_p_ipd <- tidy(lm_phe_p_ipd)
write.csv(tidy_lm_phe_p_ipd, file = paste0("lm_phe_p_ipd",write.time,".csv"))
tidy_lm_phe_p_opd <- tidy(lm_phe_p_opd)
write.csv(tidy_lm_phe_p_opd, file = paste0("lm_phe_p_opd",write.time,".csv"))
tidy_lm_phe_d_ipd <- tidy(lm_phe_d_ipd)
write.csv(tidy_lm_phe_d_ipd, file = paste0("lm_phe_d_ipd",write.time,".csv"))
tidy_lm_phe_d_opd <- tidy(lm_phe_d_opd)
write.csv(tidy_lm_phe_d_opd, file = paste0("lm_phe_d_opd",write.time,".csv"))
tidy_lm_phe_m_ipd <- tidy(lm_phe_m_ipd)
write.csv(tidy_lm_phe_m_ipd, file = paste0("lm_phe_m_ipd",write.time,".csv"))
tidy_lm_phe_m_opd <- tidy(lm_phe_m_opd)
write.csv(tidy_lm_phe_m_opd, file = paste0("lm_phe_m_opd",write.time,".csv"))
tidy_lm_oop_p_ipd <- tidy(lm_oop_p_ipd)
write.csv(tidy_lm_oop_p_ipd, file = paste0("lm_oop_p_ipd",write.time,".csv"))
tidy_lm_oop_p_opd <- tidy(lm_oop_p_opd)
write.csv(tidy_lm_oop_p_opd, file = paste0("lm_oop_p_opd",write.time,".csv"))
tidy_lm_oop_d_ipd <- tidy(lm_oop_d_ipd)
write.csv(tidy_lm_oop_d_ipd, file = paste0("lm_oop_d_ipd",write.time,".csv"))
tidy_lm_oop_d_opd <- tidy(lm_oop_d_opd)
write.csv(tidy_lm_oop_d_opd, file = paste0("lm_oop_d_opd",write.time,".csv"))
tidy_lm_oop_m_ipd <- tidy(lm_oop_m_ipd)
write.csv(tidy_lm_oop_m_ipd, file = paste0("lm_oop_m_ipd",write.time,".csv"))
tidy_lm_oop_m_opd <- tidy(lm_oop_m_opd)
write.csv(tidy_lm_oop_m_opd, file = paste0("lm_oop_m_opd",write.time,".csv"))



################################################################################################
# National estimates of time spent (TS) and distance travelled (DT) on healthcare
################################################

# Clean outliers > 100 days (1 outlier)
ugasoc_p_ipd_edittime <- ugasoc_p_ipd %>% filter(time < 100)
ugasoc_p_opd_edittime <- ugasoc_p_opd %>% filter(time < 100)

# Clean outliers > 400 km (2 outliers)
ugasoc_p_ipd_edittime <- ugasoc_p_ipd_edittime %>% filter(distance < 400)
ugasoc_p_opd_edittime <- ugasoc_p_opd_edittime %>% filter(distance < 400)

# Create proportions of time spent in healthcare (time)
ugasoc_p_ipd_edittime <- ugasoc_p_ipd_edittime %>% mutate(time_prop = time / sum(time, na.rm = T))
ugasoc_p_opd_edittime <- ugasoc_p_opd_edittime %>% mutate(time_prop = time / sum(time, na.rm = T))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(time_prop = time / sum(time, na.rm = T))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(time_prop = time / sum(time, na.rm = T))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(time_prop = time / sum(time, na.rm = T))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(time_prop = time / sum(time, na.rm = T))

# Create proportions of distance traveled to access healthcare (distance)
ugasoc_p_ipd_edittime <- ugasoc_p_ipd_edittime %>% mutate(distance_prop = distance / sum(distance, na.rm = T))
ugasoc_p_opd_edittime <- ugasoc_p_opd_edittime %>% mutate(distance_prop = distance / sum(distance, na.rm = T))
ugasoc_d_ipd <- ugasoc_d_ipd %>% mutate(distance_prop = distance / sum(distance, na.rm = T))
ugasoc_d_opd <- ugasoc_d_opd %>% mutate(distance_prop = distance / sum(distance, na.rm = T))
ugasoc_m_ipd <- ugasoc_m_ipd %>% mutate(distance_prop = distance / sum(distance, na.rm = T))
ugasoc_m_opd <- ugasoc_m_opd %>% mutate(distance_prop = distance / sum(distance, na.rm = T))

# Add row 0 for "No population" (origin of x-axis)
ugasoc_p_ipd_edittime <- add_row(ugasoc_p_ipd_edittime, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)
ugasoc_p_opd_edittime <- add_row(ugasoc_p_opd_edittime, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)
ugasoc_d_ipd <- add_row(ugasoc_d_ipd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)
ugasoc_d_opd <- add_row(ugasoc_d_opd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)
ugasoc_m_ipd <- add_row(ugasoc_m_ipd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)
ugasoc_m_opd <- add_row(ugasoc_m_opd, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_n5=0, hhhead_income_n10=0, hhhead_income=0, hhexp_n5=0, hhexp_n10=0, hhexp=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)
#ugasoc_p_ipd[nrow(ugasoc_p_ipd)+1,] <- 0
#ugasoc_p_opd[nrow(ugasoc_p_opd)+1,] <- 0
#ugasoc_d_ipd[nrow(ugasoc_d_ipd)+1,] <- 0
#ugasoc_d_opd[nrow(ugasoc_d_opd)+1,] <- 0
#ugasoc_m_ipd[nrow(ugasoc_m_ipd)+1,] <- 0
#ugasoc_m_opd[nrow(ugasoc_m_opd)+1,] <- 0


################################################
# Time & distance based on INCOME (sample)
################################################
user0 <- 1
user1 <- 0



# Sum proportion and cumulative amounts of time spent and distance travelled by sample quantile/decile/centile
ugasoc_p_ipd_time <- aggregate(ugasoc_p_ipd_edittime$time_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income), FUN=sum)
ugasoc_p_ipd_time_amount <- aggregate(ugasoc_p_ipd_edittime$time, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income), FUN=sum)
ugasoc_p_ipd_time <- full_join_NA(x = ugasoc_p_ipd_time, y = ugasoc_p_ipd_time_amount, by = "Centile")
ugasoc_p_ipd_time <- if(1 %in% ugasoc_p_ipd_time$Centile == F) {add_row(ugasoc_p_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_time}
ugasoc_p_ipd_time <- arrange(ugasoc_p_ipd_time, Centile)
ugasoc_p_ipd_distance <- aggregate(ugasoc_p_ipd_edittime$distance_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income), FUN=sum)
ugasoc_p_ipd_distance_amount <- aggregate(ugasoc_p_ipd_edittime$distance, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income), FUN=sum)
ugasoc_p_ipd_distance <- full_join_NA(x = ugasoc_p_ipd_distance, y = ugasoc_p_ipd_distance_amount, by = "Centile")
ugasoc_p_ipd_distance <- if(1 %in% ugasoc_p_ipd_distance$Centile == F) {add_row(ugasoc_p_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_distance}
ugasoc_p_ipd_distance <- arrange(ugasoc_p_ipd_distance, Centile)

ugasoc_p_opd_time <- aggregate(ugasoc_p_opd_edittime$time_prop, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income), FUN=sum)
ugasoc_p_opd_time_amount <- aggregate(ugasoc_p_opd_edittime$time, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income), FUN=sum)
ugasoc_p_opd_time <- full_join_NA(x = ugasoc_p_opd_time, y = ugasoc_p_opd_time_amount, by = "Centile")
ugasoc_p_opd_time <- if(1 %in% ugasoc_p_opd_time$Centile == F) {add_row(ugasoc_p_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_time}
ugasoc_p_opd_time <- arrange(ugasoc_p_opd_time, Centile)
ugasoc_p_opd_distance <- aggregate(ugasoc_p_opd_edittime$distance_prop, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income), FUN=sum)
ugasoc_p_opd_distance_amount <- aggregate(ugasoc_p_opd_edittime$distance, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income), FUN=sum)
ugasoc_p_opd_distance <- full_join_NA(x = ugasoc_p_opd_distance, y = ugasoc_p_opd_distance_amount, by = "Centile")
ugasoc_p_opd_distance <- if(1 %in% ugasoc_p_opd_distance$Centile == F) {add_row(ugasoc_p_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_distance}
ugasoc_p_opd_distance <- arrange(ugasoc_p_opd_distance, Centile)

ugasoc_d_ipd_time <- aggregate(ugasoc_d_ipd$time_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_time_amount <- aggregate(ugasoc_d_ipd$time, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_time <- full_join_NA(x = ugasoc_d_ipd_time, y = ugasoc_d_ipd_time_amount, by = "Centile")
ugasoc_d_ipd_time <- if(1 %in% ugasoc_d_ipd_time$Centile == F) {add_row(ugasoc_d_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_time}
ugasoc_d_ipd_time <- arrange(ugasoc_d_ipd_time, Centile)
ugasoc_d_ipd_distance <- aggregate(ugasoc_d_ipd$distance_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_distance_amount <- aggregate(ugasoc_d_ipd$distance, by=list(Centile=ugasoc_d_ipd$hhhead_income), FUN=sum)
ugasoc_d_ipd_distance <- full_join_NA(x = ugasoc_d_ipd_distance, y = ugasoc_d_ipd_distance_amount, by = "Centile")
ugasoc_d_ipd_distance <- if(1 %in% ugasoc_d_ipd_distance$Centile == F) {add_row(ugasoc_d_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_distance}
ugasoc_d_ipd_distance <- arrange(ugasoc_d_ipd_distance, Centile)

ugasoc_d_opd_time <- aggregate(ugasoc_d_opd$time_prop, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_time_amount <- aggregate(ugasoc_d_opd$time, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_time <- full_join_NA(x = ugasoc_d_opd_time, y = ugasoc_d_opd_time_amount, by = "Centile")
ugasoc_d_opd_time <- if(1 %in% ugasoc_d_opd_time$Centile == F) {add_row(ugasoc_d_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_time}
ugasoc_d_opd_time <- arrange(ugasoc_d_opd_time, Centile)
ugasoc_d_opd_distance <- aggregate(ugasoc_d_opd$distance_prop, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_distance_amount <- aggregate(ugasoc_d_opd$distance, by=list(Centile=ugasoc_d_opd$hhhead_income), FUN=sum)
ugasoc_d_opd_distance <- full_join_NA(x = ugasoc_d_opd_distance, y = ugasoc_d_opd_distance_amount, by = "Centile")
ugasoc_d_opd_distance <- if(1 %in% ugasoc_d_opd_distance$Centile == F) {add_row(ugasoc_d_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_distance}
ugasoc_d_opd_distance <- arrange(ugasoc_d_opd_distance, Centile)

ugasoc_m_ipd_time <- aggregate(ugasoc_m_ipd$time_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_time_amount <- aggregate(ugasoc_m_ipd$time, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_time <- full_join_NA(x = ugasoc_m_ipd_time, y = ugasoc_m_ipd_time_amount, by = "Centile")
ugasoc_m_ipd_time <- if(1 %in% ugasoc_m_ipd_time$Centile == F) {add_row(ugasoc_m_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_time}
ugasoc_m_ipd_time <- arrange(ugasoc_m_ipd_time, Centile)
ugasoc_m_ipd_distance <- aggregate(ugasoc_m_ipd$distance_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_distance_amount <- aggregate(ugasoc_m_ipd$distance, by=list(Centile=ugasoc_m_ipd$hhhead_income), FUN=sum)
ugasoc_m_ipd_distance <- full_join_NA(x = ugasoc_m_ipd_distance, y = ugasoc_m_ipd_distance_amount, by = "Centile")
ugasoc_m_ipd_distance <- if(1 %in% ugasoc_m_ipd_distance$Centile == F) {add_row(ugasoc_m_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_distance}
ugasoc_m_ipd_distance <- arrange(ugasoc_m_ipd_distance, Centile)

ugasoc_m_opd_time <- aggregate(ugasoc_m_opd$time_prop, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_time_amount <- aggregate(ugasoc_m_opd$time, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_time <- full_join_NA(x = ugasoc_m_opd_time, y = ugasoc_m_opd_time_amount, by = "Centile")
ugasoc_m_opd_time <- if(1 %in% ugasoc_m_opd_time$Centile == F) {add_row(ugasoc_m_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_time}
ugasoc_m_opd_time <- arrange(ugasoc_m_opd_time, Centile)
ugasoc_m_opd_distance <- aggregate(ugasoc_m_opd$distance_prop, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_distance_amount <- aggregate(ugasoc_m_opd$distance, by=list(Centile=ugasoc_m_opd$hhhead_income), FUN=sum)
ugasoc_m_opd_distance <- full_join_NA(x = ugasoc_m_opd_distance, y = ugasoc_m_opd_distance_amount, by = "Centile")
ugasoc_m_opd_distance <- if(1 %in% ugasoc_m_opd_distance$Centile == F) {add_row(ugasoc_m_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_distance}
ugasoc_m_opd_distance <- arrange(ugasoc_m_opd_distance, Centile)



# Rename variables
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time_prop = x.x)
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time = x.y)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance_prop = x.x)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance = x.y)

ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time_prop = x.x)
ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time = x.y)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance_prop = x.x)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance = x.y)

ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time_prop = x.x)
ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time = x.y)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance_prop = x.x)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance = x.y)

ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time_prop = x.x)
ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time = x.y)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance_prop = x.x)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance = x.y)

ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time_prop = x.x)
ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time = x.y)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance_prop = x.x)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance = x.y)

ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time_prop = x.x)
ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time = x.y)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance_prop = x.x)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance = x.y)




# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(ugasoc_p_ipd_time, ugasoc_p_ipd_distance, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(ugasoc_p_opd_time, ugasoc_p_opd_distance, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(ugasoc_d_ipd_time, ugasoc_d_ipd_distance, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(ugasoc_d_opd_time, ugasoc_d_opd_distance, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(ugasoc_m_ipd_time, ugasoc_m_ipd_distance, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(ugasoc_m_opd_time, ugasoc_m_opd_distance, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)



# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, time_prop_cumul = cumsum(time_prop))
uga_p_ipd <- mutate(uga_p_ipd, time_cumul = cumsum(time))
uga_p_ipd <- mutate(uga_p_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_p_ipd <- mutate(uga_p_ipd, distance_cumul = cumsum(distance))

uga_p_opd <- mutate(uga_p_opd, time_prop_cumul = cumsum(time_prop))
uga_p_opd <- mutate(uga_p_opd, time_cumul = cumsum(time))
uga_p_opd <- mutate(uga_p_opd, distance_prop_cumul = cumsum(distance_prop))
uga_p_opd <- mutate(uga_p_opd, distance_cumul = cumsum(distance))

uga_d_ipd <- mutate(uga_d_ipd, time_prop_cumul = cumsum(time_prop))
uga_d_ipd <- mutate(uga_d_ipd, time_cumul = cumsum(time))
uga_d_ipd <- mutate(uga_d_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_d_ipd <- mutate(uga_d_ipd, distance_cumul = cumsum(distance))

uga_d_opd <- mutate(uga_d_opd, time_prop_cumul = cumsum(time_prop))
uga_d_opd <- mutate(uga_d_opd, time_cumul = cumsum(time))
uga_d_opd <- mutate(uga_d_opd, distance_prop_cumul = cumsum(distance_prop))
uga_d_opd <- mutate(uga_d_opd, distance_cumul = cumsum(distance))

uga_m_ipd <- mutate(uga_m_ipd, time_prop_cumul = cumsum(time_prop))
uga_m_ipd <- mutate(uga_m_ipd, time_cumul = cumsum(time))
uga_m_ipd <- mutate(uga_m_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_m_ipd <- mutate(uga_m_ipd, distance_cumul = cumsum(distance))

uga_m_opd <- mutate(uga_m_opd, time_prop_cumul = cumsum(time_prop))
uga_m_opd <- mutate(uga_m_opd, time_cumul = cumsum(time))
uga_m_opd <- mutate(uga_m_opd, distance_prop_cumul = cumsum(distance_prop))
uga_m_opd <- mutate(uga_m_opd, distance_cumul = cumsum(distance))




# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)



# Calculate
concentration_uga_time_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$time)) * cov(df_concentration_uga_p_ipd$time, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_distance_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$distance)) * cov(df_concentration_uga_p_ipd$distance, df_concentration_uga_p_ipd$Centile), digits = 4)

concentration_uga_time_p_opd <- round((2/mean(df_concentration_uga_p_opd$time)) * cov(df_concentration_uga_p_opd$time, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_distance_p_opd <- round((2/mean(df_concentration_uga_p_opd$distance)) * cov(df_concentration_uga_p_opd$distance, df_concentration_uga_p_opd$Centile), digits = 4)

concentration_uga_time_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$time)) * cov(df_concentration_uga_d_ipd$time, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_distance_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$distance)) * cov(df_concentration_uga_d_ipd$distance, df_concentration_uga_d_ipd$Centile), digits = 4)

concentration_uga_time_d_opd <- round((2/mean(df_concentration_uga_d_opd$time)) * cov(df_concentration_uga_d_opd$time, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_distance_d_opd <- round((2/mean(df_concentration_uga_d_opd$distance)) * cov(df_concentration_uga_d_opd$distance, df_concentration_uga_d_opd$Centile), digits = 4)

concentration_uga_time_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$time)) * cov(df_concentration_uga_m_ipd$time, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_distance_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$distance)) * cov(df_concentration_uga_m_ipd$distance, df_concentration_uga_m_ipd$Centile), digits = 4)

concentration_uga_time_m_opd <- round((2/mean(df_concentration_uga_m_opd$time)) * cov(df_concentration_uga_m_opd$time, df_concentration_uga_m_opd$Centile), digits = 4)
concentration_uga_distance_m_opd <- round((2/mean(df_concentration_uga_m_opd$distance)) * cov(df_concentration_uga_m_opd$distance, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
time <- c(concentration_uga_time_p_ipd, concentration_uga_time_p_opd, concentration_uga_time_d_ipd, concentration_uga_time_d_opd, concentration_uga_time_m_ipd, concentration_uga_time_m_opd)
distance <- c(concentration_uga_distance_p_ipd, concentration_uga_distance_p_opd, concentration_uga_distance_d_ipd, concentration_uga_distance_d_opd, concentration_uga_distance_m_ipd, concentration_uga_distance_m_opd)
wealth_distrib <- c('income', 'income', 'income', 'income', 'income', 'income')
wealth_source <- c('sample', 'sample', 'sample', 'sample', 'sample', 'sample')
concentration_uga_timedist_inc <- data.frame(disease, visittype, time, distance, wealth_distrib, wealth_source)




# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_time <- graph_time(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_time_p_ipd)
graph_p_ipd_time_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_ipd_distance <- graph_distance(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_distance_p_ipd)
graph_p_ipd_distance_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_time <- graph_time(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_time_p_opd)
graph_p_opd_time_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_opd_distance <- graph_distance(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_distance_p_opd)
graph_p_opd_distance_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time,graph_p_ipd_distance,graph_p_opd_time,graph_p_opd_distance)
dev.off()
jpeg(filename = paste0("graph_p_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time_freq,graph_p_ipd_distance_freq,graph_p_opd_time_freq,graph_p_opd_distance_freq)
dev.off()


# Diarrhea
graph_d_ipd_time <- graph_time(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_time_d_ipd)
graph_d_ipd_time_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_ipd_distance <- graph_distance(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_distance_d_ipd)
graph_d_ipd_distance_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_time <- graph_time(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_time_d_opd)
graph_d_opd_time_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_opd_distance <- graph_distance(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_distance_d_opd)
graph_d_opd_distance_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time,graph_d_ipd_distance,graph_d_opd_time,graph_d_opd_distance)
dev.off()
jpeg(filename = paste0("graph_d_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time_freq,graph_d_ipd_distance_freq,graph_d_opd_time_freq,graph_d_opd_distance_freq)
dev.off()

# Measles 
graph_m_ipd_time <- graph_time(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_time_m_ipd)
graph_m_ipd_time_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_ipd_distance <- graph_distance(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_distance_m_ipd)
graph_m_ipd_distance_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_time <- graph_time(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_time_m_opd)
graph_m_opd_time_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_opd_distance <- graph_distance(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_distance_m_opd)
graph_m_opd_distance_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time,graph_m_ipd_distance,graph_m_opd_time,graph_m_opd_distance)
dev.off()
jpeg(filename = paste0("graph_m_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time_freq,graph_m_ipd_distance_freq,graph_m_opd_time_freq,graph_m_opd_distance_freq)
dev.off()


################################################
# Time & distance based on INCOME (World Bank)
################################################
user0 <- 1
user1 <- 1


# Sum proportion and cumulative amounts of time spent and distance travelled by sample quantile/decile/centile
ugasoc_p_ipd_time <- aggregate(ugasoc_p_ipd_edittime$time_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_time_amount <- aggregate(ugasoc_p_ipd_edittime$time, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_time <- full_join_NA(x = ugasoc_p_ipd_time, y = ugasoc_p_ipd_time_amount, by = "Centile")
ugasoc_p_ipd_time <- if(1 %in% ugasoc_p_ipd_time$Centile == F) {add_row(ugasoc_p_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_time}
ugasoc_p_ipd_time <- arrange(ugasoc_p_ipd_time, Centile)
ugasoc_p_ipd_distance <- aggregate(ugasoc_p_ipd_edittime$distance_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_distance_amount <- aggregate(ugasoc_p_ipd_edittime$distance, by=list(Centile=ugasoc_p_ipd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_ipd_distance <- full_join_NA(x = ugasoc_p_ipd_distance, y = ugasoc_p_ipd_distance_amount, by = "Centile")
ugasoc_p_ipd_distance <- if(1 %in% ugasoc_p_ipd_distance$Centile == F) {add_row(ugasoc_p_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_distance}
ugasoc_p_ipd_distance <- arrange(ugasoc_p_ipd_distance, Centile)

ugasoc_p_opd_time <- aggregate(ugasoc_p_opd_edittime$time_prop, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_time_amount <- aggregate(ugasoc_p_opd_edittime$time, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_time <- full_join_NA(x = ugasoc_p_opd_time, y = ugasoc_p_opd_time_amount, by = "Centile")
ugasoc_p_opd_time <- if(1 %in% ugasoc_p_opd_time$Centile == F) {add_row(ugasoc_p_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_time}
ugasoc_p_opd_time <- arrange(ugasoc_p_opd_time, Centile)
ugasoc_p_opd_distance <- aggregate(ugasoc_p_opd_edittime$distance_prop, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_distance_amount <- aggregate(ugasoc_p_opd_edittime$distance, by=list(Centile=ugasoc_p_opd_edittime$hhhead_income_wb), FUN=sum)
ugasoc_p_opd_distance <- full_join_NA(x = ugasoc_p_opd_distance, y = ugasoc_p_opd_distance_amount, by = "Centile")
ugasoc_p_opd_distance <- if(1 %in% ugasoc_p_opd_distance$Centile == F) {add_row(ugasoc_p_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_distance}
ugasoc_p_opd_distance <- arrange(ugasoc_p_opd_distance, Centile)

ugasoc_d_ipd_time <- aggregate(ugasoc_d_ipd$time_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_time_amount <- aggregate(ugasoc_d_ipd$time, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_time <- full_join_NA(x = ugasoc_d_ipd_time, y = ugasoc_d_ipd_time_amount, by = "Centile")
ugasoc_d_ipd_time <- if(1 %in% ugasoc_d_ipd_time$Centile == F) {add_row(ugasoc_d_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_time}
ugasoc_d_ipd_time <- arrange(ugasoc_d_ipd_time, Centile)
ugasoc_d_ipd_distance <- aggregate(ugasoc_d_ipd$distance_prop, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_distance_amount <- aggregate(ugasoc_d_ipd$distance, by=list(Centile=ugasoc_d_ipd$hhhead_income_wb), FUN=sum)
ugasoc_d_ipd_distance <- full_join_NA(x = ugasoc_d_ipd_distance, y = ugasoc_d_ipd_distance_amount, by = "Centile")
ugasoc_d_ipd_distance <- if(1 %in% ugasoc_d_ipd_distance$Centile == F) {add_row(ugasoc_d_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_distance}
ugasoc_d_ipd_distance <- arrange(ugasoc_d_ipd_distance, Centile)

ugasoc_d_opd_time <- aggregate(ugasoc_d_opd$time_prop, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_time_amount <- aggregate(ugasoc_d_opd$time, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_time <- full_join_NA(x = ugasoc_d_opd_time, y = ugasoc_d_opd_time_amount, by = "Centile")
ugasoc_d_opd_time <- if(1 %in% ugasoc_d_opd_time$Centile == F) {add_row(ugasoc_d_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_time}
ugasoc_d_opd_time <- arrange(ugasoc_d_opd_time, Centile)
ugasoc_d_opd_distance <- aggregate(ugasoc_d_opd$distance_prop, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_distance_amount <- aggregate(ugasoc_d_opd$distance, by=list(Centile=ugasoc_d_opd$hhhead_income_wb), FUN=sum)
ugasoc_d_opd_distance <- full_join_NA(x = ugasoc_d_opd_distance, y = ugasoc_d_opd_distance_amount, by = "Centile")
ugasoc_d_opd_distance <- if(1 %in% ugasoc_d_opd_distance$Centile == F) {add_row(ugasoc_d_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_distance}
ugasoc_d_opd_distance <- arrange(ugasoc_d_opd_distance, Centile)

ugasoc_m_ipd_time <- aggregate(ugasoc_m_ipd$time_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_time_amount <- aggregate(ugasoc_m_ipd$time, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_time <- full_join_NA(x = ugasoc_m_ipd_time, y = ugasoc_m_ipd_time_amount, by = "Centile")
ugasoc_m_ipd_time <- if(1 %in% ugasoc_m_ipd_time$Centile == F) {add_row(ugasoc_m_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_time}
ugasoc_m_ipd_time <- arrange(ugasoc_m_ipd_time, Centile)
ugasoc_m_ipd_distance <- aggregate(ugasoc_m_ipd$distance_prop, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_distance_amount <- aggregate(ugasoc_m_ipd$distance, by=list(Centile=ugasoc_m_ipd$hhhead_income_wb), FUN=sum)
ugasoc_m_ipd_distance <- full_join_NA(x = ugasoc_m_ipd_distance, y = ugasoc_m_ipd_distance_amount, by = "Centile")
ugasoc_m_ipd_distance <- if(1 %in% ugasoc_m_ipd_distance$Centile == F) {add_row(ugasoc_m_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_distance}
ugasoc_m_ipd_distance <- arrange(ugasoc_m_ipd_distance, Centile)

ugasoc_m_opd_time <- aggregate(ugasoc_m_opd$time_prop, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_time_amount <- aggregate(ugasoc_m_opd$time, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_time <- full_join_NA(x = ugasoc_m_opd_time, y = ugasoc_m_opd_time_amount, by = "Centile")
ugasoc_m_opd_time <- if(1 %in% ugasoc_m_opd_time$Centile == F) {add_row(ugasoc_m_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_time}
ugasoc_m_opd_time <- arrange(ugasoc_m_opd_time, Centile)
ugasoc_m_opd_distance <- aggregate(ugasoc_m_opd$distance_prop, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_distance_amount <- aggregate(ugasoc_m_opd$distance, by=list(Centile=ugasoc_m_opd$hhhead_income_wb), FUN=sum)
ugasoc_m_opd_distance <- full_join_NA(x = ugasoc_m_opd_distance, y = ugasoc_m_opd_distance_amount, by = "Centile")
ugasoc_m_opd_distance <- if(1 %in% ugasoc_m_opd_distance$Centile == F) {add_row(ugasoc_m_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_distance}
ugasoc_m_opd_distance <- arrange(ugasoc_m_opd_distance, Centile)



# Rename variables
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time_prop = x.x)
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time = x.y)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance_prop = x.x)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance = x.y)

ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time_prop = x.x)
ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time = x.y)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance_prop = x.x)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance = x.y)

ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time_prop = x.x)
ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time = x.y)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance_prop = x.x)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance = x.y)

ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time_prop = x.x)
ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time = x.y)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance_prop = x.x)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance = x.y)

ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time_prop = x.x)
ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time = x.y)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance_prop = x.x)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance = x.y)

ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time_prop = x.x)
ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time = x.y)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance_prop = x.x)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance = x.y)




# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(ugasoc_p_ipd_time, ugasoc_p_ipd_distance, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(ugasoc_p_opd_time, ugasoc_p_opd_distance, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(ugasoc_d_ipd_time, ugasoc_d_ipd_distance, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(ugasoc_d_opd_time, ugasoc_d_opd_distance, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(ugasoc_m_ipd_time, ugasoc_m_ipd_distance, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(ugasoc_m_opd_time, ugasoc_m_opd_distance, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)



# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, time_prop_cumul = cumsum(time_prop))
uga_p_ipd <- mutate(uga_p_ipd, time_cumul = cumsum(time))
uga_p_ipd <- mutate(uga_p_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_p_ipd <- mutate(uga_p_ipd, distance_cumul = cumsum(distance))

uga_p_opd <- mutate(uga_p_opd, time_prop_cumul = cumsum(time_prop))
uga_p_opd <- mutate(uga_p_opd, time_cumul = cumsum(time))
uga_p_opd <- mutate(uga_p_opd, distance_prop_cumul = cumsum(distance_prop))
uga_p_opd <- mutate(uga_p_opd, distance_cumul = cumsum(distance))

uga_d_ipd <- mutate(uga_d_ipd, time_prop_cumul = cumsum(time_prop))
uga_d_ipd <- mutate(uga_d_ipd, time_cumul = cumsum(time))
uga_d_ipd <- mutate(uga_d_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_d_ipd <- mutate(uga_d_ipd, distance_cumul = cumsum(distance))

uga_d_opd <- mutate(uga_d_opd, time_prop_cumul = cumsum(time_prop))
uga_d_opd <- mutate(uga_d_opd, time_cumul = cumsum(time))
uga_d_opd <- mutate(uga_d_opd, distance_prop_cumul = cumsum(distance_prop))
uga_d_opd <- mutate(uga_d_opd, distance_cumul = cumsum(distance))

uga_m_ipd <- mutate(uga_m_ipd, time_prop_cumul = cumsum(time_prop))
uga_m_ipd <- mutate(uga_m_ipd, time_cumul = cumsum(time))
uga_m_ipd <- mutate(uga_m_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_m_ipd <- mutate(uga_m_ipd, distance_cumul = cumsum(distance))

uga_m_opd <- mutate(uga_m_opd, time_prop_cumul = cumsum(time_prop))
uga_m_opd <- mutate(uga_m_opd, time_cumul = cumsum(time))
uga_m_opd <- mutate(uga_m_opd, distance_prop_cumul = cumsum(distance_prop))
uga_m_opd <- mutate(uga_m_opd, distance_cumul = cumsum(distance))




# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)



# Calculate
concentration_uga_time_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$time)) * cov(df_concentration_uga_p_ipd$time, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_distance_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$distance)) * cov(df_concentration_uga_p_ipd$distance, df_concentration_uga_p_ipd$Centile), digits = 4)

concentration_uga_time_p_opd <- round((2/mean(df_concentration_uga_p_opd$time)) * cov(df_concentration_uga_p_opd$time, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_distance_p_opd <- round((2/mean(df_concentration_uga_p_opd$distance)) * cov(df_concentration_uga_p_opd$distance, df_concentration_uga_p_opd$Centile), digits = 4)

concentration_uga_time_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$time)) * cov(df_concentration_uga_d_ipd$time, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_distance_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$distance)) * cov(df_concentration_uga_d_ipd$distance, df_concentration_uga_d_ipd$Centile), digits = 4)

concentration_uga_time_d_opd <- round((2/mean(df_concentration_uga_d_opd$time)) * cov(df_concentration_uga_d_opd$time, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_distance_d_opd <- round((2/mean(df_concentration_uga_d_opd$distance)) * cov(df_concentration_uga_d_opd$distance, df_concentration_uga_d_opd$Centile), digits = 4)

concentration_uga_time_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$time)) * cov(df_concentration_uga_m_ipd$time, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_distance_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$distance)) * cov(df_concentration_uga_m_ipd$distance, df_concentration_uga_m_ipd$Centile), digits = 4)

concentration_uga_time_m_opd <- round((2/mean(df_concentration_uga_m_opd$time)) * cov(df_concentration_uga_m_opd$time, df_concentration_uga_m_opd$Centile), digits = 4)
concentration_uga_distance_m_opd <- round((2/mean(df_concentration_uga_m_opd$distance)) * cov(df_concentration_uga_m_opd$distance, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
time <- c(concentration_uga_time_p_ipd, concentration_uga_time_p_opd, concentration_uga_time_d_ipd, concentration_uga_time_d_opd, concentration_uga_time_m_ipd, concentration_uga_time_m_opd)
distance <- c(concentration_uga_distance_p_ipd, concentration_uga_distance_p_opd, concentration_uga_distance_d_ipd, concentration_uga_distance_d_opd, concentration_uga_distance_m_ipd, concentration_uga_distance_m_opd)
wealth_distrib <- c('income', 'income', 'income', 'income', 'income', 'income')
wealth_source <- c('wb', 'wb', 'wb', 'wb', 'wb', 'wb')
concentration_uga_timedist_incwb <- data.frame(disease, visittype, time, distance, wealth_distrib, wealth_source)



# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_time <- graph_time(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_time_p_ipd)
graph_p_ipd_time_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_ipd_distance <- graph_distance(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_distance_p_ipd)
graph_p_ipd_distance_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_time <- graph_time(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_time_p_opd)
graph_p_opd_time_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_opd_distance <- graph_distance(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_distance_p_opd)
graph_p_opd_distance_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time,graph_p_ipd_distance,graph_p_opd_time,graph_p_opd_distance)
dev.off()
jpeg(filename = paste0("graph_p_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time_freq,graph_p_ipd_distance_freq,graph_p_opd_time_freq,graph_p_opd_distance_freq)
dev.off()


# Diarrhea
graph_d_ipd_time <- graph_time(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_time_d_ipd)
graph_d_ipd_time_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_ipd_distance <- graph_distance(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_distance_d_ipd)
graph_d_ipd_distance_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_time <- graph_time(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_time_d_opd)
graph_d_opd_time_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_opd_distance <- graph_distance(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_distance_d_opd)
graph_d_opd_distance_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time,graph_d_ipd_distance,graph_d_opd_time,graph_d_opd_distance)
dev.off()
jpeg(filename = paste0("graph_d_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time_freq,graph_d_ipd_distance_freq,graph_d_opd_time_freq,graph_d_opd_distance_freq)
dev.off()

# Measles 
graph_m_ipd_time <- graph_time(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_time_m_ipd)
graph_m_ipd_time_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_ipd_distance <- graph_distance(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_distance_m_ipd)
graph_m_ipd_distance_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_time <- graph_time(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_time_m_opd)
graph_m_opd_time_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_opd_distance <- graph_distance(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_distance_m_opd)
graph_m_opd_distance_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time,graph_m_ipd_distance,graph_m_opd_time,graph_m_opd_distance)
dev.off()
jpeg(filename = paste0("graph_m_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time_freq,graph_m_ipd_distance_freq,graph_m_opd_time_freq,graph_m_opd_distance_freq)
dev.off()


################################################
# Time & distance based on CONSUMPTION (sample)
################################################
user0 <- 2
user1 <- 0


# Sum proportion and cumulative amounts of time spent and distance travelled by sample quantile/decile/centile
ugasoc_p_ipd_time <- aggregate(ugasoc_p_ipd_edittime$time_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhexp), FUN=sum)
ugasoc_p_ipd_time_amount <- aggregate(ugasoc_p_ipd_edittime$time, by=list(Centile=ugasoc_p_ipd_edittime$hhexp), FUN=sum)
ugasoc_p_ipd_time <- full_join_NA(x = ugasoc_p_ipd_time, y = ugasoc_p_ipd_time_amount, by = "Centile")
ugasoc_p_ipd_time <- if(1 %in% ugasoc_p_ipd_time$Centile == F) {add_row(ugasoc_p_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_time}
ugasoc_p_ipd_time <- arrange(ugasoc_p_ipd_time, Centile)
ugasoc_p_ipd_distance <- aggregate(ugasoc_p_ipd_edittime$distance_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhexp), FUN=sum)
ugasoc_p_ipd_distance_amount <- aggregate(ugasoc_p_ipd_edittime$distance, by=list(Centile=ugasoc_p_ipd_edittime$hhexp), FUN=sum)
ugasoc_p_ipd_distance <- full_join_NA(x = ugasoc_p_ipd_distance, y = ugasoc_p_ipd_distance_amount, by = "Centile")
ugasoc_p_ipd_distance <- if(1 %in% ugasoc_p_ipd_distance$Centile == F) {add_row(ugasoc_p_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_distance}
ugasoc_p_ipd_distance <- arrange(ugasoc_p_ipd_distance, Centile)

ugasoc_p_opd_time <- aggregate(ugasoc_p_opd_edittime$time_prop, by=list(Centile=ugasoc_p_opd_edittime$hhexp), FUN=sum)
ugasoc_p_opd_time_amount <- aggregate(ugasoc_p_opd_edittime$time, by=list(Centile=ugasoc_p_opd_edittime$hhexp), FUN=sum)
ugasoc_p_opd_time <- full_join_NA(x = ugasoc_p_opd_time, y = ugasoc_p_opd_time_amount, by = "Centile")
ugasoc_p_opd_time <- if(1 %in% ugasoc_p_opd_time$Centile == F) {add_row(ugasoc_p_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_time}
ugasoc_p_opd_time <- arrange(ugasoc_p_opd_time, Centile)
ugasoc_p_opd_distance <- aggregate(ugasoc_p_opd_edittime$distance_prop, by=list(Centile=ugasoc_p_opd_edittime$hhexp), FUN=sum)
ugasoc_p_opd_distance_amount <- aggregate(ugasoc_p_opd_edittime$distance, by=list(Centile=ugasoc_p_opd_edittime$hhexp), FUN=sum)
ugasoc_p_opd_distance <- full_join_NA(x = ugasoc_p_opd_distance, y = ugasoc_p_opd_distance_amount, by = "Centile")
ugasoc_p_opd_distance <- if(1 %in% ugasoc_p_opd_distance$Centile == F) {add_row(ugasoc_p_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_distance}
ugasoc_p_opd_distance <- arrange(ugasoc_p_opd_distance, Centile)

ugasoc_d_ipd_time <- aggregate(ugasoc_d_ipd$time_prop, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_time_amount <- aggregate(ugasoc_d_ipd$time, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_time <- full_join_NA(x = ugasoc_d_ipd_time, y = ugasoc_d_ipd_time_amount, by = "Centile")
ugasoc_d_ipd_time <- if(1 %in% ugasoc_d_ipd_time$Centile == F) {add_row(ugasoc_d_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_time}
ugasoc_d_ipd_time <- arrange(ugasoc_d_ipd_time, Centile)
ugasoc_d_ipd_distance <- aggregate(ugasoc_d_ipd$distance_prop, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_distance_amount <- aggregate(ugasoc_d_ipd$distance, by=list(Centile=ugasoc_d_ipd$hhexp), FUN=sum)
ugasoc_d_ipd_distance <- full_join_NA(x = ugasoc_d_ipd_distance, y = ugasoc_d_ipd_distance_amount, by = "Centile")
ugasoc_d_ipd_distance <- if(1 %in% ugasoc_d_ipd_distance$Centile == F) {add_row(ugasoc_d_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_distance}
ugasoc_d_ipd_distance <- arrange(ugasoc_d_ipd_distance, Centile)

ugasoc_d_opd_time <- aggregate(ugasoc_d_opd$time_prop, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_time_amount <- aggregate(ugasoc_d_opd$time, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_time <- full_join_NA(x = ugasoc_d_opd_time, y = ugasoc_d_opd_time_amount, by = "Centile")
ugasoc_d_opd_time <- if(1 %in% ugasoc_d_opd_time$Centile == F) {add_row(ugasoc_d_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_time}
ugasoc_d_opd_time <- arrange(ugasoc_d_opd_time, Centile)
ugasoc_d_opd_distance <- aggregate(ugasoc_d_opd$distance_prop, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_distance_amount <- aggregate(ugasoc_d_opd$distance, by=list(Centile=ugasoc_d_opd$hhexp), FUN=sum)
ugasoc_d_opd_distance <- full_join_NA(x = ugasoc_d_opd_distance, y = ugasoc_d_opd_distance_amount, by = "Centile")
ugasoc_d_opd_distance <- if(1 %in% ugasoc_d_opd_distance$Centile == F) {add_row(ugasoc_d_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_distance}
ugasoc_d_opd_distance <- arrange(ugasoc_d_opd_distance, Centile)

ugasoc_m_ipd_time <- aggregate(ugasoc_m_ipd$time_prop, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_time_amount <- aggregate(ugasoc_m_ipd$time, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_time <- full_join_NA(x = ugasoc_m_ipd_time, y = ugasoc_m_ipd_time_amount, by = "Centile")
ugasoc_m_ipd_time <- if(1 %in% ugasoc_m_ipd_time$Centile == F) {add_row(ugasoc_m_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_time}
ugasoc_m_ipd_time <- arrange(ugasoc_m_ipd_time, Centile)
ugasoc_m_ipd_distance <- aggregate(ugasoc_m_ipd$distance_prop, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_distance_amount <- aggregate(ugasoc_m_ipd$distance, by=list(Centile=ugasoc_m_ipd$hhexp), FUN=sum)
ugasoc_m_ipd_distance <- full_join_NA(x = ugasoc_m_ipd_distance, y = ugasoc_m_ipd_distance_amount, by = "Centile")
ugasoc_m_ipd_distance <- if(1 %in% ugasoc_m_ipd_distance$Centile == F) {add_row(ugasoc_m_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_distance}
ugasoc_m_ipd_distance <- arrange(ugasoc_m_ipd_distance, Centile)

ugasoc_m_opd_time <- aggregate(ugasoc_m_opd$time_prop, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_time_amount <- aggregate(ugasoc_m_opd$time, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_time <- full_join_NA(x = ugasoc_m_opd_time, y = ugasoc_m_opd_time_amount, by = "Centile")
ugasoc_m_opd_time <- if(1 %in% ugasoc_m_opd_time$Centile == F) {add_row(ugasoc_m_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_time}
ugasoc_m_opd_time <- arrange(ugasoc_m_opd_time, Centile)
ugasoc_m_opd_distance <- aggregate(ugasoc_m_opd$distance_prop, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_distance_amount <- aggregate(ugasoc_m_opd$distance, by=list(Centile=ugasoc_m_opd$hhexp), FUN=sum)
ugasoc_m_opd_distance <- full_join_NA(x = ugasoc_m_opd_distance, y = ugasoc_m_opd_distance_amount, by = "Centile")
ugasoc_m_opd_distance <- if(1 %in% ugasoc_m_opd_distance$Centile == F) {add_row(ugasoc_m_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_distance}
ugasoc_m_opd_distance <- arrange(ugasoc_m_opd_distance, Centile)



# Rename variables
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time_prop = x.x)
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time = x.y)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance_prop = x.x)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance = x.y)

ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time_prop = x.x)
ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time = x.y)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance_prop = x.x)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance = x.y)

ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time_prop = x.x)
ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time = x.y)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance_prop = x.x)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance = x.y)

ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time_prop = x.x)
ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time = x.y)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance_prop = x.x)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance = x.y)

ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time_prop = x.x)
ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time = x.y)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance_prop = x.x)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance = x.y)

ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time_prop = x.x)
ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time = x.y)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance_prop = x.x)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance = x.y)




# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(ugasoc_p_ipd_time, ugasoc_p_ipd_distance, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(ugasoc_p_opd_time, ugasoc_p_opd_distance, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(ugasoc_d_ipd_time, ugasoc_d_ipd_distance, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(ugasoc_d_opd_time, ugasoc_d_opd_distance, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(ugasoc_m_ipd_time, ugasoc_m_ipd_distance, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(ugasoc_m_opd_time, ugasoc_m_opd_distance, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)



# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, time_prop_cumul = cumsum(time_prop))
uga_p_ipd <- mutate(uga_p_ipd, time_cumul = cumsum(time))
uga_p_ipd <- mutate(uga_p_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_p_ipd <- mutate(uga_p_ipd, distance_cumul = cumsum(distance))

uga_p_opd <- mutate(uga_p_opd, time_prop_cumul = cumsum(time_prop))
uga_p_opd <- mutate(uga_p_opd, time_cumul = cumsum(time))
uga_p_opd <- mutate(uga_p_opd, distance_prop_cumul = cumsum(distance_prop))
uga_p_opd <- mutate(uga_p_opd, distance_cumul = cumsum(distance))

uga_d_ipd <- mutate(uga_d_ipd, time_prop_cumul = cumsum(time_prop))
uga_d_ipd <- mutate(uga_d_ipd, time_cumul = cumsum(time))
uga_d_ipd <- mutate(uga_d_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_d_ipd <- mutate(uga_d_ipd, distance_cumul = cumsum(distance))

uga_d_opd <- mutate(uga_d_opd, time_prop_cumul = cumsum(time_prop))
uga_d_opd <- mutate(uga_d_opd, time_cumul = cumsum(time))
uga_d_opd <- mutate(uga_d_opd, distance_prop_cumul = cumsum(distance_prop))
uga_d_opd <- mutate(uga_d_opd, distance_cumul = cumsum(distance))

uga_m_ipd <- mutate(uga_m_ipd, time_prop_cumul = cumsum(time_prop))
uga_m_ipd <- mutate(uga_m_ipd, time_cumul = cumsum(time))
uga_m_ipd <- mutate(uga_m_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_m_ipd <- mutate(uga_m_ipd, distance_cumul = cumsum(distance))

uga_m_opd <- mutate(uga_m_opd, time_prop_cumul = cumsum(time_prop))
uga_m_opd <- mutate(uga_m_opd, time_cumul = cumsum(time))
uga_m_opd <- mutate(uga_m_opd, distance_prop_cumul = cumsum(distance_prop))
uga_m_opd <- mutate(uga_m_opd, distance_cumul = cumsum(distance))




# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)



# Calculate
concentration_uga_time_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$time)) * cov(df_concentration_uga_p_ipd$time, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_distance_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$distance)) * cov(df_concentration_uga_p_ipd$distance, df_concentration_uga_p_ipd$Centile), digits = 4)

concentration_uga_time_p_opd <- round((2/mean(df_concentration_uga_p_opd$time)) * cov(df_concentration_uga_p_opd$time, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_distance_p_opd <- round((2/mean(df_concentration_uga_p_opd$distance)) * cov(df_concentration_uga_p_opd$distance, df_concentration_uga_p_opd$Centile), digits = 4)

concentration_uga_time_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$time)) * cov(df_concentration_uga_d_ipd$time, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_distance_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$distance)) * cov(df_concentration_uga_d_ipd$distance, df_concentration_uga_d_ipd$Centile), digits = 4)

concentration_uga_time_d_opd <- round((2/mean(df_concentration_uga_d_opd$time)) * cov(df_concentration_uga_d_opd$time, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_distance_d_opd <- round((2/mean(df_concentration_uga_d_opd$distance)) * cov(df_concentration_uga_d_opd$distance, df_concentration_uga_d_opd$Centile), digits = 4)

concentration_uga_time_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$time)) * cov(df_concentration_uga_m_ipd$time, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_distance_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$distance)) * cov(df_concentration_uga_m_ipd$distance, df_concentration_uga_m_ipd$Centile), digits = 4)

concentration_uga_time_m_opd <- round((2/mean(df_concentration_uga_m_opd$time)) * cov(df_concentration_uga_m_opd$time, df_concentration_uga_m_opd$Centile), digits = 4)
concentration_uga_distance_m_opd <- round((2/mean(df_concentration_uga_m_opd$distance)) * cov(df_concentration_uga_m_opd$distance, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
time <- c(concentration_uga_time_p_ipd, concentration_uga_time_p_opd, concentration_uga_time_d_ipd, concentration_uga_time_d_opd, concentration_uga_time_m_ipd, concentration_uga_time_m_opd)
distance <- c(concentration_uga_distance_p_ipd, concentration_uga_distance_p_opd, concentration_uga_distance_d_ipd, concentration_uga_distance_d_opd, concentration_uga_distance_m_ipd, concentration_uga_distance_m_opd)
wealth_distrib <- c('consumption', 'consumption', 'consumption', 'consumption', 'consumption', 'consumption')
wealth_source <- c('sample', 'sample', 'sample', 'sample', 'sample', 'sample')
concentration_uga_timedist_exp <- data.frame(disease, visittype, time, distance, wealth_distrib, wealth_source)




# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_time <- graph_time(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_time_p_ipd)
graph_p_ipd_time_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_ipd_distance <- graph_distance(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_distance_p_ipd)
graph_p_ipd_distance_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_time <- graph_time(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_time_p_opd)
graph_p_opd_time_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_opd_distance <- graph_distance(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_distance_p_opd)
graph_p_opd_distance_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time,graph_p_ipd_distance,graph_p_opd_time,graph_p_opd_distance)
dev.off()
jpeg(filename = paste0("graph_p_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time_freq,graph_p_ipd_distance_freq,graph_p_opd_time_freq,graph_p_opd_distance_freq)
dev.off()


# Diarrhea
graph_d_ipd_time <- graph_time(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_time_d_ipd)
graph_d_ipd_time_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_ipd_distance <- graph_distance(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_distance_d_ipd)
graph_d_ipd_distance_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_time <- graph_time(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_time_d_opd)
graph_d_opd_time_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_opd_distance <- graph_distance(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_distance_d_opd)
graph_d_opd_distance_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time,graph_d_ipd_distance,graph_d_opd_time,graph_d_opd_distance)
dev.off()
jpeg(filename = paste0("graph_d_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time_freq,graph_d_ipd_distance_freq,graph_d_opd_time_freq,graph_d_opd_distance_freq)
dev.off()

# Measles 
graph_m_ipd_time <- graph_time(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_time_m_ipd)
graph_m_ipd_time_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_ipd_distance <- graph_distance(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_distance_m_ipd)
graph_m_ipd_distance_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_time <- graph_time(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_time_m_opd)
graph_m_opd_time_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_opd_distance <- graph_distance(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_distance_m_opd)
graph_m_opd_distance_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time,graph_m_ipd_distance,graph_m_opd_time,graph_m_opd_distance)
dev.off()
jpeg(filename = paste0("graph_m_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time_freq,graph_m_ipd_distance_freq,graph_m_opd_time_freq,graph_m_opd_distance_freq)
dev.off()


################################################
# Time & distance based on CONSUMPTION (World Bank)
################################################
user0 <- 2
user1 <- 1


# Sum proportion and cumulative amounts of time spent and distance travelled by sample quantile/decile/centile
ugasoc_p_ipd_time <- aggregate(ugasoc_p_ipd_edittime$time_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_ipd_time_amount <- aggregate(ugasoc_p_ipd_edittime$time, by=list(Centile=ugasoc_p_ipd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_ipd_time <- full_join_NA(x = ugasoc_p_ipd_time, y = ugasoc_p_ipd_time_amount, by = "Centile")
ugasoc_p_ipd_time <- if(1 %in% ugasoc_p_ipd_time$Centile == F) {add_row(ugasoc_p_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_time}
ugasoc_p_ipd_time <- arrange(ugasoc_p_ipd_time, Centile)
ugasoc_p_ipd_distance <- aggregate(ugasoc_p_ipd_edittime$distance_prop, by=list(Centile=ugasoc_p_ipd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_ipd_distance_amount <- aggregate(ugasoc_p_ipd_edittime$distance, by=list(Centile=ugasoc_p_ipd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_ipd_distance <- full_join_NA(x = ugasoc_p_ipd_distance, y = ugasoc_p_ipd_distance_amount, by = "Centile")
ugasoc_p_ipd_distance <- if(1 %in% ugasoc_p_ipd_distance$Centile == F) {add_row(ugasoc_p_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_ipd_distance}
ugasoc_p_ipd_distance <- arrange(ugasoc_p_ipd_distance, Centile)

ugasoc_p_opd_time <- aggregate(ugasoc_p_opd_edittime$time_prop, by=list(Centile=ugasoc_p_opd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_opd_time_amount <- aggregate(ugasoc_p_opd_edittime$time, by=list(Centile=ugasoc_p_opd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_opd_time <- full_join_NA(x = ugasoc_p_opd_time, y = ugasoc_p_opd_time_amount, by = "Centile")
ugasoc_p_opd_time <- if(1 %in% ugasoc_p_opd_time$Centile == F) {add_row(ugasoc_p_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_time}
ugasoc_p_opd_time <- arrange(ugasoc_p_opd_time, Centile)
ugasoc_p_opd_distance <- aggregate(ugasoc_p_opd_edittime$distance_prop, by=list(Centile=ugasoc_p_opd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_opd_distance_amount <- aggregate(ugasoc_p_opd_edittime$distance, by=list(Centile=ugasoc_p_opd_edittime$hhexp_wb), FUN=sum)
ugasoc_p_opd_distance <- full_join_NA(x = ugasoc_p_opd_distance, y = ugasoc_p_opd_distance_amount, by = "Centile")
ugasoc_p_opd_distance <- if(1 %in% ugasoc_p_opd_distance$Centile == F) {add_row(ugasoc_p_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_p_opd_distance}
ugasoc_p_opd_distance <- arrange(ugasoc_p_opd_distance, Centile)

ugasoc_d_ipd_time <- aggregate(ugasoc_d_ipd$time_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_time_amount <- aggregate(ugasoc_d_ipd$time, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_time <- full_join_NA(x = ugasoc_d_ipd_time, y = ugasoc_d_ipd_time_amount, by = "Centile")
ugasoc_d_ipd_time <- if(1 %in% ugasoc_d_ipd_time$Centile == F) {add_row(ugasoc_d_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_time}
ugasoc_d_ipd_time <- arrange(ugasoc_d_ipd_time, Centile)
ugasoc_d_ipd_distance <- aggregate(ugasoc_d_ipd$distance_prop, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_distance_amount <- aggregate(ugasoc_d_ipd$distance, by=list(Centile=ugasoc_d_ipd$hhexp_wb), FUN=sum)
ugasoc_d_ipd_distance <- full_join_NA(x = ugasoc_d_ipd_distance, y = ugasoc_d_ipd_distance_amount, by = "Centile")
ugasoc_d_ipd_distance <- if(1 %in% ugasoc_d_ipd_distance$Centile == F) {add_row(ugasoc_d_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_ipd_distance}
ugasoc_d_ipd_distance <- arrange(ugasoc_d_ipd_distance, Centile)

ugasoc_d_opd_time <- aggregate(ugasoc_d_opd$time_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_time_amount <- aggregate(ugasoc_d_opd$time, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_time <- full_join_NA(x = ugasoc_d_opd_time, y = ugasoc_d_opd_time_amount, by = "Centile")
ugasoc_d_opd_time <- if(1 %in% ugasoc_d_opd_time$Centile == F) {add_row(ugasoc_d_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_time}
ugasoc_d_opd_time <- arrange(ugasoc_d_opd_time, Centile)
ugasoc_d_opd_distance <- aggregate(ugasoc_d_opd$distance_prop, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_distance_amount <- aggregate(ugasoc_d_opd$distance, by=list(Centile=ugasoc_d_opd$hhexp_wb), FUN=sum)
ugasoc_d_opd_distance <- full_join_NA(x = ugasoc_d_opd_distance, y = ugasoc_d_opd_distance_amount, by = "Centile")
ugasoc_d_opd_distance <- if(1 %in% ugasoc_d_opd_distance$Centile == F) {add_row(ugasoc_d_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_d_opd_distance}
ugasoc_d_opd_distance <- arrange(ugasoc_d_opd_distance, Centile)

ugasoc_m_ipd_time <- aggregate(ugasoc_m_ipd$time_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_time_amount <- aggregate(ugasoc_m_ipd$time, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_time <- full_join_NA(x = ugasoc_m_ipd_time, y = ugasoc_m_ipd_time_amount, by = "Centile")
ugasoc_m_ipd_time <- if(1 %in% ugasoc_m_ipd_time$Centile == F) {add_row(ugasoc_m_ipd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_time}
ugasoc_m_ipd_time <- arrange(ugasoc_m_ipd_time, Centile)
ugasoc_m_ipd_distance <- aggregate(ugasoc_m_ipd$distance_prop, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_distance_amount <- aggregate(ugasoc_m_ipd$distance, by=list(Centile=ugasoc_m_ipd$hhexp_wb), FUN=sum)
ugasoc_m_ipd_distance <- full_join_NA(x = ugasoc_m_ipd_distance, y = ugasoc_m_ipd_distance_amount, by = "Centile")
ugasoc_m_ipd_distance <- if(1 %in% ugasoc_m_ipd_distance$Centile == F) {add_row(ugasoc_m_ipd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_ipd_distance}
ugasoc_m_ipd_distance <- arrange(ugasoc_m_ipd_distance, Centile)

ugasoc_m_opd_time <- aggregate(ugasoc_m_opd$time_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_time_amount <- aggregate(ugasoc_m_opd$time, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_time <- full_join_NA(x = ugasoc_m_opd_time, y = ugasoc_m_opd_time_amount, by = "Centile")
ugasoc_m_opd_time <- if(1 %in% ugasoc_m_opd_time$Centile == F) {add_row(ugasoc_m_opd_time, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_time}
ugasoc_m_opd_time <- arrange(ugasoc_m_opd_time, Centile)
ugasoc_m_opd_distance <- aggregate(ugasoc_m_opd$distance_prop, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_distance_amount <- aggregate(ugasoc_m_opd$distance, by=list(Centile=ugasoc_m_opd$hhexp_wb), FUN=sum)
ugasoc_m_opd_distance <- full_join_NA(x = ugasoc_m_opd_distance, y = ugasoc_m_opd_distance_amount, by = "Centile")
ugasoc_m_opd_distance <- if(1 %in% ugasoc_m_opd_distance$Centile == F) {add_row(ugasoc_m_opd_distance, Centile = 1, x.x = 0, x.y = 0, .after = NULL)} else {ugasoc_m_opd_distance}
ugasoc_m_opd_distance <- arrange(ugasoc_m_opd_distance, Centile)



# Rename variables
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time_prop = x.x)
ugasoc_p_ipd_time <- rename(ugasoc_p_ipd_time, time = x.y)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance_prop = x.x)
ugasoc_p_ipd_distance <- rename(ugasoc_p_ipd_distance, distance = x.y)

ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time_prop = x.x)
ugasoc_p_opd_time <- rename(ugasoc_p_opd_time, time = x.y)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance_prop = x.x)
ugasoc_p_opd_distance <- rename(ugasoc_p_opd_distance, distance = x.y)

ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time_prop = x.x)
ugasoc_d_ipd_time <- rename(ugasoc_d_ipd_time, time = x.y)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance_prop = x.x)
ugasoc_d_ipd_distance <- rename(ugasoc_d_ipd_distance, distance = x.y)

ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time_prop = x.x)
ugasoc_d_opd_time <- rename(ugasoc_d_opd_time, time = x.y)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance_prop = x.x)
ugasoc_d_opd_distance <- rename(ugasoc_d_opd_distance, distance = x.y)

ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time_prop = x.x)
ugasoc_m_ipd_time <- rename(ugasoc_m_ipd_time, time = x.y)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance_prop = x.x)
ugasoc_m_ipd_distance <- rename(ugasoc_m_ipd_distance, distance = x.y)

ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time_prop = x.x)
ugasoc_m_opd_time <- rename(ugasoc_m_opd_time, time = x.y)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance_prop = x.x)
ugasoc_m_opd_distance <- rename(ugasoc_m_opd_distance, distance = x.y)




# Bind healthcare expenditure variables (Adds 0 instead of NA)
uga_p_ipd <- full_join_NA(ugasoc_p_ipd_time, ugasoc_p_ipd_distance, by = "Centile")
uga_p_ipd <- arrange(uga_p_ipd, Centile)

uga_p_opd <- full_join_NA(ugasoc_p_opd_time, ugasoc_p_opd_distance, by = "Centile")
uga_p_opd <- arrange(uga_p_opd, Centile)

uga_d_ipd <- full_join_NA(ugasoc_d_ipd_time, ugasoc_d_ipd_distance, by = "Centile")
uga_d_ipd <- arrange(uga_d_ipd, Centile)

uga_d_opd <- full_join_NA(ugasoc_d_opd_time, ugasoc_d_opd_distance, by = "Centile")
uga_d_opd <- arrange(uga_d_opd, Centile)

uga_m_ipd <- full_join_NA(ugasoc_m_ipd_time, ugasoc_m_ipd_distance, by = "Centile")
uga_m_ipd <- arrange(uga_m_ipd, Centile)

uga_m_opd <- full_join_NA(ugasoc_m_opd_time, ugasoc_m_opd_distance, by = "Centile")
uga_m_opd <- arrange(uga_m_opd, Centile)



# Add cumulative sum
uga_p_ipd <- mutate(uga_p_ipd, time_prop_cumul = cumsum(time_prop))
uga_p_ipd <- mutate(uga_p_ipd, time_cumul = cumsum(time))
uga_p_ipd <- mutate(uga_p_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_p_ipd <- mutate(uga_p_ipd, distance_cumul = cumsum(distance))

uga_p_opd <- mutate(uga_p_opd, time_prop_cumul = cumsum(time_prop))
uga_p_opd <- mutate(uga_p_opd, time_cumul = cumsum(time))
uga_p_opd <- mutate(uga_p_opd, distance_prop_cumul = cumsum(distance_prop))
uga_p_opd <- mutate(uga_p_opd, distance_cumul = cumsum(distance))

uga_d_ipd <- mutate(uga_d_ipd, time_prop_cumul = cumsum(time_prop))
uga_d_ipd <- mutate(uga_d_ipd, time_cumul = cumsum(time))
uga_d_ipd <- mutate(uga_d_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_d_ipd <- mutate(uga_d_ipd, distance_cumul = cumsum(distance))

uga_d_opd <- mutate(uga_d_opd, time_prop_cumul = cumsum(time_prop))
uga_d_opd <- mutate(uga_d_opd, time_cumul = cumsum(time))
uga_d_opd <- mutate(uga_d_opd, distance_prop_cumul = cumsum(distance_prop))
uga_d_opd <- mutate(uga_d_opd, distance_cumul = cumsum(distance))

uga_m_ipd <- mutate(uga_m_ipd, time_prop_cumul = cumsum(time_prop))
uga_m_ipd <- mutate(uga_m_ipd, time_cumul = cumsum(time))
uga_m_ipd <- mutate(uga_m_ipd, distance_prop_cumul = cumsum(distance_prop))
uga_m_ipd <- mutate(uga_m_ipd, distance_cumul = cumsum(distance))

uga_m_opd <- mutate(uga_m_opd, time_prop_cumul = cumsum(time_prop))
uga_m_opd <- mutate(uga_m_opd, time_cumul = cumsum(time))
uga_m_opd <- mutate(uga_m_opd, distance_prop_cumul = cumsum(distance_prop))
uga_m_opd <- mutate(uga_m_opd, distance_cumul = cumsum(distance))




# Calculate concentration index
# Exclude observation 0
df_concentration_uga_p_ipd <- uga_p_ipd %>% filter(Centile != 0)
df_concentration_uga_p_opd <- uga_p_opd %>% filter(Centile != 0)

df_concentration_uga_d_ipd <- uga_d_ipd %>% filter(Centile != 0)
df_concentration_uga_d_opd <- uga_d_opd %>% filter(Centile != 0)

df_concentration_uga_m_ipd <- uga_m_ipd %>% filter(Centile != 0)
df_concentration_uga_m_opd <- uga_m_opd %>% filter(Centile != 0)



# Calculate
concentration_uga_time_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$time)) * cov(df_concentration_uga_p_ipd$time, df_concentration_uga_p_ipd$Centile), digits = 4)
concentration_uga_distance_p_ipd <- round((2/mean(df_concentration_uga_p_ipd$distance)) * cov(df_concentration_uga_p_ipd$distance, df_concentration_uga_p_ipd$Centile), digits = 4)

concentration_uga_time_p_opd <- round((2/mean(df_concentration_uga_p_opd$time)) * cov(df_concentration_uga_p_opd$time, df_concentration_uga_p_opd$Centile), digits = 4)
concentration_uga_distance_p_opd <- round((2/mean(df_concentration_uga_p_opd$distance)) * cov(df_concentration_uga_p_opd$distance, df_concentration_uga_p_opd$Centile), digits = 4)

concentration_uga_time_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$time)) * cov(df_concentration_uga_d_ipd$time, df_concentration_uga_d_ipd$Centile), digits = 4)
concentration_uga_distance_d_ipd <- round((2/mean(df_concentration_uga_d_ipd$distance)) * cov(df_concentration_uga_d_ipd$distance, df_concentration_uga_d_ipd$Centile), digits = 4)

concentration_uga_time_d_opd <- round((2/mean(df_concentration_uga_d_opd$time)) * cov(df_concentration_uga_d_opd$time, df_concentration_uga_d_opd$Centile), digits = 4)
concentration_uga_distance_d_opd <- round((2/mean(df_concentration_uga_d_opd$distance)) * cov(df_concentration_uga_d_opd$distance, df_concentration_uga_d_opd$Centile), digits = 4)

concentration_uga_time_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$time)) * cov(df_concentration_uga_m_ipd$time, df_concentration_uga_m_ipd$Centile), digits = 4)
concentration_uga_distance_m_ipd <- round((2/mean(df_concentration_uga_m_ipd$distance)) * cov(df_concentration_uga_m_ipd$distance, df_concentration_uga_m_ipd$Centile), digits = 4)

concentration_uga_time_m_opd <- round((2/mean(df_concentration_uga_m_opd$time)) * cov(df_concentration_uga_m_opd$time, df_concentration_uga_m_opd$Centile), digits = 4)
concentration_uga_distance_m_opd <- round((2/mean(df_concentration_uga_m_opd$distance)) * cov(df_concentration_uga_m_opd$distance, df_concentration_uga_m_opd$Centile), digits = 4)


# Dataframe with all concentration indexes
disease <- c('Pneumonia', 'Pneumonia', 'Diarrhea', 'Diarrhea', 'Measles', 'Measles')
visittype <- c('Inpatient', 'Outpatient', 'Inpatient', 'Outpatient', 'Inpatient', 'Outpatient')
time <- c(concentration_uga_time_p_ipd, concentration_uga_time_p_opd, concentration_uga_time_d_ipd, concentration_uga_time_d_opd, concentration_uga_time_m_ipd, concentration_uga_time_m_opd)
distance <- c(concentration_uga_distance_p_ipd, concentration_uga_distance_p_opd, concentration_uga_distance_d_ipd, concentration_uga_distance_d_opd, concentration_uga_distance_m_ipd, concentration_uga_distance_m_opd)
wealth_distrib <- c('consumption', 'consumption', 'consumption', 'consumption', 'consumption', 'consumption')
wealth_source <- c('wb', 'wb', 'wb', 'wb', 'wb', 'wb')
concentration_uga_timedist_expwb <- data.frame(disease, visittype, time, distance, wealth_distrib, wealth_source)




# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
# Pneumonia
graph_p_ipd_time <- graph_time(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_time_p_ipd)
graph_p_ipd_time_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_ipd_distance <- graph_distance(dataset = uga_p_ipd, subtitle = "Hospitalized Pneumonia", ci = concentration_uga_distance_p_ipd)
graph_p_ipd_distance_freq <- graph_freq(dataset = ugasoc_p_ipd_edittime, subtitle = paste0("Hospitalized Pneumonia ",write.graph2), label_y = "Patients in all facilities")

graph_p_opd_time <- graph_time(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_time_p_opd)
graph_p_opd_time_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")
graph_p_opd_distance <- graph_distance(dataset = uga_p_opd, subtitle = "Ambulatory Pneumonia", ci = concentration_uga_distance_p_opd)
graph_p_opd_distance_freq <- graph_freq(dataset = ugasoc_p_opd_edittime, subtitle = paste0("Ambulatory Pneumonia ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_p_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time,graph_p_ipd_distance,graph_p_opd_time,graph_p_opd_distance)
dev.off()
jpeg(filename = paste0("graph_p_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_p_ipd_time_freq,graph_p_ipd_distance_freq,graph_p_opd_time_freq,graph_p_opd_distance_freq)
dev.off()


# Diarrhea
graph_d_ipd_time <- graph_time(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_time_d_ipd)
graph_d_ipd_time_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_ipd_distance <- graph_distance(dataset = uga_d_ipd, subtitle = "Hospitalized Diarrhea", ci = concentration_uga_distance_d_ipd)
graph_d_ipd_distance_freq <- graph_freq(dataset = ugasoc_d_ipd, subtitle = paste0("Hospitalized Diarrhea ",write.graph2), label_y = "Patients in all facilities")

graph_d_opd_time <- graph_time(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_time_d_opd)
graph_d_opd_time_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")
graph_d_opd_distance <- graph_distance(dataset = uga_d_opd, subtitle = "Ambulatory Diarrhea", ci = concentration_uga_distance_d_opd)
graph_d_opd_distance_freq <- graph_freq(dataset = ugasoc_d_opd, subtitle = paste0("Ambulatory Diarrhea ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_d_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time,graph_d_ipd_distance,graph_d_opd_time,graph_d_opd_distance)
dev.off()
jpeg(filename = paste0("graph_d_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_d_ipd_time_freq,graph_d_ipd_distance_freq,graph_d_opd_time_freq,graph_d_opd_distance_freq)
dev.off()

# Measles 
graph_m_ipd_time <- graph_time(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_time_m_ipd)
graph_m_ipd_time_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_ipd_distance <- graph_distance(dataset = uga_m_ipd, subtitle = "Hospitalized Measles", ci = concentration_uga_distance_m_ipd)
graph_m_ipd_distance_freq <- graph_freq(dataset = ugasoc_m_ipd, subtitle = paste0("Hospitalized Measles ",write.graph2), label_y = "Patients in all facilities")

graph_m_opd_time <- graph_time(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_time_m_opd)
graph_m_opd_time_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")
graph_m_opd_distance <- graph_distance(dataset = uga_m_opd, subtitle = "Ambulatory Measles", ci = concentration_uga_distance_m_opd)
graph_m_opd_distance_freq <- graph_freq(dataset = ugasoc_m_opd, subtitle = paste0("Ambulatory Measles ",write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_m_time_distance_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time,graph_m_ipd_distance,graph_m_opd_time,graph_m_opd_distance)
dev.off()
jpeg(filename = paste0("graph_m_time_distance_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
plot_grid(graph_m_ipd_time_freq,graph_m_ipd_distance_freq,graph_m_opd_time_freq,graph_m_opd_distance_freq)
dev.off()


################################################
# Generate concentration indexes CSV (time & distance)
################################################

concentration_uga_timedist <- bind_rows(concentration_uga_timedist_inc, concentration_uga_timedist_incwb, concentration_uga_timedist_exp, concentration_uga_timedist_expwb)
write.csv(concentration_uga_timedist, file = paste0("concentration_uga_timedist_", write.time, ".csv"), col.names = TRUE, sep = ",")




################################################
# Time & distance regression analysis
################################################

# MLR of distance to/from all facilities
## Includes public & private facilities

# Model fitting & cleaning extreme outliers (cleaning reported at the beginning of section)
## DISTANCE - PNEUMONIA

## Using: sample centiles
lm_distance_p <- lm(formula = distance ~ district_label + ipd + time + hhhead_income + hhexp + sector_public + male_child + male_caregiver + ethnicity + marital + agechild, data = ugasoc_p)
summary(lm_distance_p)
AIC(lm_distance_p)

## Using nationally-representative centiles
lm_distance_p <- lm(formula = distance ~ district_label + ipd + time + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + ethnicity + marital + agechild, data = ugasoc_p)
summary(lm_distance_p)
AIC(lm_distance_p)
## Ethnicity(Bagisu) is significant, although few observations (10) that include outliers (~800 km)

lm_distance_p <- lm(formula = distance ~ district_label + ipd + time + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + Bagisu + district_label:Bagisu + agechild, data = ugasoc_p)
summary(lm_distance_p)
AIC(lm_distance_p)
## No interaction: district - Bagisu

## CLEANING ADDED FOR DISTANCE >800 km

lm_distance_p <- lm(formula = distance ~ district_label + ipd + time + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + Bagisu + agechild, data = ugasoc_p)
summary(lm_distance_p)
AIC(lm_distance_p)
## Ethnicity(Bagisu) no longer significant
## Ethnicity has an effect on the significance of the district - removed ethnicity

lm_distance_p <- lm(formula = distance ~ district_label + ipd + time + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + agechild, data = ugasoc_p)
summary(lm_distance_p)
AIC(lm_distance_p)


## Making IPD exogenous to the model since it may hide the influence of other factors
lm_distance_p_ipd <- lm(formula = distance ~ district_label + time + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + agechild, data = ugasoc_p_ipd_edittime)
summary(lm_distance_p_ipd)
AIC(lm_distance_p)
## hhexp_wb becomes significant again --> likely hidden by IPD
## This type of model should be kept even if lower R2 (lower AIC, though!)

ggplot(data = ugasoc_p, aes(x = hhexp_wb, y = distance, color = factor(sector_public))) + geom_point() + geom_smooth(method = "lm", se = F) + scale_y_log10() + facet_wrap(~ipd)


## DISTANCE - DIARRHEA
ggplot(data = ugasoc_d, aes(x = hhexp_wb, y = distance, color = factor(sector_public))) + geom_point() + geom_smooth(method = "lm", se = F) + scale_y_log10() + facet_wrap(~ipd)
## No outlier


## DISTANCE - MEASLES
ggplot(data = ugasoc_m, aes(x = hhexp_wb, y = distance, color = factor(sector_public))) + geom_point() + geom_smooth(method = "lm", se = F) + scale_y_log10() + facet_wrap(~ipd)
## Outliers, but not extreme





## TIME - PNEUMONIA
lm_time_p <- lm(formula = time ~ district_label + ipd + distance + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + agechild, data = ugasoc_p)
summary(lm_time_p)
AIC(lm_time_p)
## Low R-squared for pneumonia - no other possible variables
## Ethnicity does not add significant


lm_time_p_ipd <- lm(formula = time ~ district_label + distance + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + agechild, data = ugasoc_p_ipd_edittime)
summary(lm_time_p_ipd)
AIC(lm_time_p_ipd)
## Very low R2 --> corresponds to low concentration index (= -0.02)

lm_time_m_ipd <- lm(formula = time ~ district_label + distance + hhhead_income_wb + hhexp_wb + sector_public + male_child + male_caregiver + agechild, data = ugasoc_p_ipd_edittime)
summary(lm_time_m_ipd)
AIC(lm_time_m_ipd)
## High R2 --> corresponds to high concentration index (= -0.2872)

ggplot(data = ugasoc_p, aes(x = hhexp_wb, y = time, color = factor(sector_public))) + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(~ipd)
## 1 extreme outlier

## TIME - DIARRHEA

ggplot(data = ugasoc_d, aes(x = hhexp_wb, y = time, color = factor(sector_public))) + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(~ipd)
## No outlier


## TIME - MEASLES

ggplot(data = ugasoc_m, aes(x = hhexp_wb, y = time, color = factor(sector_public))) + geom_point() + geom_smooth(method = "lm", se = F) + facet_wrap(~ipd)
## No outlier



# Generate models

# TIME
lm_time_p_ipd <- lm(formula = time ~ distance + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_ipd_edittime)
summary(lm_time_p_ipd)
AIC(lm_time_p_ipd)
lm_time_p_opd <- lm(formula = time ~ distance + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_opd_edittime)
summary(lm_time_p_opd)
AIC(lm_time_p_opd)
lm_time_d_ipd <- lm(formula = time ~ distance + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_ipd)
summary(lm_time_d_ipd)
AIC(lm_time_d_ipd)
lm_time_d_opd <- lm(formula = time ~ distance + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_opd)
summary(lm_time_d_opd)
AIC(lm_time_d_opd)
lm_time_m_ipd <- lm(formula = time ~ distance + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_ipd)
summary(lm_time_m_ipd)
AIC(lm_time_m_ipd)
lm_time_m_opd <- lm(formula = time ~ distance + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_opd)
summary(lm_time_m_opd)
AIC(lm_time_m_opd)

# DISTANCE
lm_distance_p_ipd <- lm(formula = distance ~ time + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_ipd_edittime)
summary(lm_distance_p_ipd)
AIC(lm_distance_p_ipd)
lm_distance_p_opd <- lm(formula = distance ~ time + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_p_opd_edittime)
summary(lm_distance_p_opd)
AIC(lm_distance_p_opd)
lm_distance_d_ipd <- lm(formula = distance ~ time + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_ipd)
summary(lm_distance_d_ipd)
AIC(lm_distance_d_ipd)
lm_distance_d_opd <- lm(formula = distance ~ time + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_d_opd)
summary(lm_distance_d_opd)
AIC(lm_distance_d_opd)
lm_distance_m_ipd <- lm(formula = distance ~ time + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_ipd)
summary(lm_distance_m_ipd)
AIC(lm_distance_m_ipd)
lm_distance_m_opd <- lm(formula = distance ~ time + hhhead_income_n10_wb + hhexp_n10_wb + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_m_opd)
summary(lm_distance_m_opd)
AIC(lm_distance_m_opd)

# Show summary model stats
glance(lm_time_p_ipd)
glance(lm_time_p_opd)
glance(lm_time_d_ipd)
glance(lm_time_d_opd)
glance(lm_time_m_ipd)
glance(lm_time_m_opd)
glance(lm_distance_p_ipd)
glance(lm_distance_p_opd)
glance(lm_distance_d_ipd)
glance(lm_distance_d_opd)
glance(lm_distance_m_ipd)
glance(lm_distance_m_opd)

# Export regression results to CSV ("broom" package)
tidy_lm_time_p_ipd <- tidy(lm_time_p_ipd)
write.csv(tidy_lm_time_p_ipd, file = paste0("lm_time_p_ipd",write.time,".csv"))
tidy_lm_time_p_opd <- tidy(lm_time_p_opd)
write.csv(tidy_lm_time_p_opd, file = paste0("lm_time_p_opd",write.time,".csv"))
tidy_lm_time_d_ipd <- tidy(lm_time_d_ipd)
write.csv(tidy_lm_time_d_ipd, file = paste0("lm_time_d_ipd",write.time,".csv"))
tidy_lm_time_d_opd <- tidy(lm_time_d_opd)
write.csv(tidy_lm_time_d_opd, file = paste0("lm_time_d_opd",write.time,".csv"))
tidy_lm_time_m_ipd <- tidy(lm_time_m_ipd)
write.csv(tidy_lm_time_m_ipd, file = paste0("lm_time_m_ipd",write.time,".csv"))
tidy_lm_time_m_opd <- tidy(lm_time_m_opd)
write.csv(tidy_lm_time_m_opd, file = paste0("lm_time_m_opd",write.time,".csv"))
tidy_lm_distance_p_ipd <- tidy(lm_distance_p_ipd)
write.csv(tidy_lm_distance_p_ipd, file = paste0("lm_distance_p_ipd",write.time,".csv"))
tidy_lm_distance_p_opd <- tidy(lm_distance_p_opd)
write.csv(tidy_lm_distance_p_opd, file = paste0("lm_distance_p_opd",write.time,".csv"))
tidy_lm_distance_d_ipd <- tidy(lm_distance_d_ipd)
write.csv(tidy_lm_distance_d_ipd, file = paste0("lm_distance_d_ipd",write.time,".csv"))
tidy_lm_distance_d_opd <- tidy(lm_distance_d_opd)
write.csv(tidy_lm_distance_d_opd, file = paste0("lm_distance_d_opd",write.time,".csv"))
tidy_lm_distance_m_ipd <- tidy(lm_distance_m_ipd)
write.csv(tidy_lm_distance_m_ipd, file = paste0("lm_distance_m_ipd",write.time,".csv"))
tidy_lm_distance_m_opd <- tidy(lm_distance_m_opd)
write.csv(tidy_lm_distance_m_opd, file = paste0("lm_distance_m_opd",write.time,".csv"))


################################################################################################
# National estimates of vaccine coverage (VC)
################################################

# Variables
#f3                       Immunization card seen?
#f3_label                 "
#immunizationcard_seen    "

#f4                 All immunizations performed   Scheduled age of child
#f4_bcg             BCG (1 dose)                  At birth (or first contact)
#f4_polioopvbirth   Polio OPV at birth (1 dose)   Within 2 weeks after birth
#f4_polioopv1       Polio OPV dose 1              At 6 weeks (or first contact)
#f4_polioopv2       Polio OPV dose 2              At 10 weeks
#f4_polioopv3       Polio OPV dose 3              At 14 weeks
#f4_polioipv        Polio IPV                     At 14 weeks (instead of OPV dose 3)
#f4_penta1          DTP-Hep-Hib dose 1            At 6 weeks (or first contact)
#f4_penta2          DTP-Hep-Hib dose 2            At 10 weeks
#f4_penta3          DTP-Hep-Hib dose 3            At 14 weeks
#f4_pcv1            PCV dose 1                    At 6 weeks (or first contact)
#f4_pcv2            PCV dose 2                    At 10 weeks
#f4_pcv3            PCV dose 3                    At 14 weeks
#f4_measles1        Measles dose 1                At 9 months (or first contact)
#f4_measles2        Measles dose 2                NOT SUBSIDIZED BY UGANDAN GOVERNMENT
#f4_mr2             Mumps-Rubella booster         UNKNOWN (likely not subsidized)

## Missing rotavirus (introduced 2016)


# Create dataset with all diseases, filter for caregivers who showed their immunization card
ugasoc_vc <- ugasoc %>% filter(immunizationcard_seen == 1 & agechild > 0)
ugasoc_vc <- ugasoc_vc %>% filter(!is.na(f4_bcg & f4_polioopvbirth & f4_polioopv1 & f4_penta1 & f4_pcv1 & f4_polioopv2 & f4_penta2 & f4_pcv2 & f4_polioopv3 & f4_polioipv & f4_penta3 & f4_pcv3 & f4_measles1 & f4_measles2))

# Generate a binary variable for whether the child is on schedule for her/his vaccines (age adjusted - in months)
ugasoc_vc <- ugasoc_vc %>% mutate(immucomplete = case_when(
  agechild < 1 ~ ifelse(f4_bcg == 1 & f4_polioopvbirth == 1, 1, 0),
  1 <= agechild & agechild < 2.5 ~ ifelse(f4_bcg == 1 & f4_polioopvbirth == 1 & f4_polioopv1 == 1 & f4_penta1 == 1 & f4_pcv1 == 1, 1, 0),
  2.5 <= agechild & agechild < 3.5 ~ ifelse(f4_bcg == 1 & f4_polioopvbirth == 1 & f4_polioopv1 == 1 & f4_penta1 == 1 & f4_pcv1 == 1 & f4_polioopv2 == 1 & f4_penta2 == 1 & f4_pcv2 == 1, 1, 0),
  3.5 <= agechild & agechild < 8 ~ ifelse(f4_bcg == 1 & f4_polioopvbirth == 1 & f4_polioopv1 == 1 & f4_penta1 == 1 & f4_pcv1 == 1 & f4_polioopv2 == 1 & f4_penta2 == 1 & f4_pcv2 == 1 & (f4_polioopv3 == 1 | f4_polioipv ==1) & f4_penta3 == 1 & f4_pcv3 == 1, 1, 0),
  8 <= agechild ~ ifelse(f4_bcg == 1 & f4_polioopvbirth == 1 & f4_polioopv1 == 1 & f4_penta1 == 1 & f4_pcv1 == 1 & f4_polioopv2 == 1 & f4_penta2 == 1 & f4_pcv2 == 1 & (f4_polioopv3 == 1 | f4_polioipv ==1) & f4_penta3 == 1 & f4_pcv3 == 1 & (f4_measles1 == 1 | f4_measles2 == 1), 1, 0)
))


# Testing immucomplete & debug
#test_ugasoc_vc <- ugasoc_vc %>% select(agechild, f4, immucomplete)
#test_ugasoc_vc <- test_ugasoc_vc %>% filter(agechild < 1)
## OK

#test_ugasoc_vc <- test_ugasoc_vc %>% filter(1 <= agechild & agechild < 2.5) %>% arrange(immucomplete, desc(f4))
## Excludes 2 obs with complete immunization except for Polio OPV birth (is this dose mandatory?)

#test_ugasoc_vc <- test_ugasoc_vc %>% filter(2.5 <= agechild & agechild < 3.5) %>% arrange(immucomplete, desc(f4))
## Excludes 1 obs with complete immunization except for Polio OPV 2

#test_ugasoc_vc <- test_ugasoc_vc %>% filter(3.5 <= agechild & agechild < 8) %>% arrange(immucomplete, desc(f4))
## Excludes 3 obs with complete immunization except for Polio OPV birth
## Excludes 1 obs with complete immunization except for Polio OPV 1 (OPV 2 & 3 are included) - missed dose?
## Excludes 1 obs with complete immunization except for PCV2 (PCV3 is included) - missed dose?

#test_ugasoc_vc <- test_ugasoc_vc %>% filter(8 <= agechild) %>% arrange(immucomplete, desc(f4))
## Excludes 7 obs with complete immunization except for Polio OPV birth
## Excludes 1 obs with complete immunization except for Polio OPV 1 (OPV 2 & 3 are included) - missed dose?
## Excludes 1 obs with complete immunization except for PCV1 & PCV2 (PCV3 is included) - missed doses?
## Excludes 1 obs with complete immunization except for PCV1 (PCV2 & PCV3 are included) - missed dose?
## Excludes 1 obs with complete immunization except for Penta 1 (Penta 2 & 3 are included) - missed dose?

#view(test_ugasoc_vc)


# Associate sample income with WB centiles
ugasoc_vc <- mutate_inc_WBcentiles(ugasoc_vc)

# Conversions WB centiles --> deciles & quantiles
ugasoc_vc <- mutate_inc_WBdeciles(ugasoc_vc)
ugasoc_vc <- mutate_inc_WBquantiles(ugasoc_vc)

# Associate sample expenses with WB centiles
ugasoc_vc <- mutate_exp_WBcentiles(ugasoc_vc)

# Conversions WB centiles --> deciles & quantiles
ugasoc_vc <- mutate_exp_WBdeciles(ugasoc_vc)
ugasoc_vc <- mutate_exp_WBquantiles(ugasoc_vc)

# Add row 0 for "No population" (origin of x-axis)
ugasoc_vc <- add_row(ugasoc_vc, immucomplete=0, dmc_facility=0, dmc_caregiver_ba=0, dmc_caregiver_c=0, nmc_caregiver=0, ic_caregiver=0, hhhead_income_month=0, hhcaregiver_income_month=0, hh_income_month=0, hh_totalexp_month=0, hh_totalexpnf_month=0, time_d_p_ipd=0, time_d_p_opd=0, time_d_d_ipd=0, time_d_d_opd=0, time_d_m_ipd=0, time_d_m_opd=0, distance=0, distance_before=0, distance_current=0, distance_after=0, time=0, dmc_facility_UGX=0, dmc_caregiver_c_UGX=0, dmc_caregiver_ba_UGX=0, nmc_caregiver_UGX=0, ic_caregiver_UGX=0, hhhead_income_wb=0, hhhead_income_n10_wb=0, hhhead_income_n5_wb=0, hhexp_wb=0, hhexp_n10_wb=0, hhexp_n5_wb=0)

# Create proportions of vaccine coverage (vc)
ugasoc_vc <- ugasoc_vc %>% mutate(vc_prop = immucomplete / sum(immucomplete, na.rm = T))


################################################
# Vaccine coverage based on INCOME (World Bank)
################################################
user0 <- 1
user1 <- 1


# Sum proportion of children with complete immunization by centile
uga_vc <- aggregate(ugasoc_vc$vc_prop, by=list(Centile=ugasoc_vc$hhhead_income_wb), FUN=sum)
uga_vc <- if(1 %in% uga_vc$Centile == F) {add_row(uga_vc, Centile = 1, x = 0, .after = NULL)} else {uga_vc}
uga_vc <- arrange(uga_vc, Centile)

# Rename variables
uga_vc <- rename(uga_vc, vc_prop = x)

# Add cumulative sum
uga_vc <- mutate(uga_vc, vc_prop_cumul = cumsum(vc_prop))


# Calculate concentration index
# Exclude observation 0
df_uga_vc <- uga_vc %>% filter(Centile != 0)


# Calculate
concentration_uga_vc <- round((2/mean(df_uga_vc$vc_prop)) * cov(df_uga_vc$vc_prop, df_uga_vc$Centile), digits = 4)


# Dataframe with all concentration indexes
vaccine_coverage <- c(concentration_uga_vc)
wealth_distrib <- c('income')
wealth_source <- c('wb')
concentration_uga_vc_incwb <- data.frame(vaccine_coverage, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
graph_uga_vc <- graph_vc(dataset = uga_vc, subtitle = NULL, ci = concentration_uga_vc)
graph_uga_vc_freq <- graph_freq(dataset = ugasoc_vc, subtitle = paste0(write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_vc_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
graph_uga_vc
dev.off()
jpeg(filename = paste0("graph_vc_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
graph_uga_vc_freq
dev.off()


################################################
# Vaccine coverage based on CONSUMPTION (World Bank)
################################################
user0 <- 2
user1 <- 1


# Sum proportion of children with complete immunization by centile
uga_vc <- aggregate(ugasoc_vc$vc_prop, by=list(Centile=ugasoc_vc$hhexp_wb), FUN=sum)
uga_vc <- if(1 %in% uga_vc$Centile == F) {add_row(uga_vc, Centile = 1, x = 0, .after = NULL)} else {uga_vc}
uga_vc <- arrange(uga_vc, Centile)

# Rename variables
uga_vc <- rename(uga_vc, vc_prop = x)

# Add cumulative sum
uga_vc <- mutate(uga_vc, vc_prop_cumul = cumsum(vc_prop))


# Calculate concentration index
# Exclude observation 0
df_uga_vc <- uga_vc %>% filter(Centile != 0)


# Calculate
concentration_uga_vc <- round((2/mean(df_uga_vc$vc_prop)) * cov(df_uga_vc$vc_prop, df_uga_vc$Centile), digits = 4)


# Dataframe with all concentration indexes
vaccine_coverage <- c(concentration_uga_vc)
wealth_distrib <- c('consumption')
wealth_source <- c('wb')
concentration_uga_vc_expwb <- data.frame(vaccine_coverage, wealth_distrib, wealth_source)


# Meta data
write.time <- gsub("( )|:", "", Sys.time())
if (user0==1 & user1==0){write.estimate = "inc_"} else if (user0==1 & user1==1) {write.estimate = "inc_wb_"} else if (user0==2 & user1==0) {write.estimate = "exp_"} else {write.estimate = "exp_wb_"}
if (user0==1){write.graph1 = "Income"} else {write.graph1 = "Consumption"}
if (user1==0){write.graph2 = "(Sample)"} else {write.graph2 = "(National)"}

# Graph BIA
graph_uga_vc <- graph_vc(dataset = uga_vc, subtitle = NULL, ci = concentration_uga_vc)
graph_uga_vc_freq <- graph_freq(dataset = ugasoc_vc, subtitle = paste0(write.graph2), label_y = "Patients in all facilities")

jpeg(filename = paste0("graph_vc_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
graph_uga_vc
dev.off()
jpeg(filename = paste0("graph_vc_freq_",write.estimate,write.time,".jpg"), width = 1000, height = 500)
graph_uga_vc_freq
dev.off()



################################################
# Generate concentration indexes CSV (vaccine coverage)
################################################

concentration_uga_vaccinecov <- bind_rows(concentration_uga_vc_incwb, concentration_uga_vc_expwb)
write.csv(concentration_uga_vaccinecov, file = paste0("concentration_uga_vc_", write.time, ".csv"), col.names = TRUE, sep = ",")


################################################
# Vaccine coverage regression analysis
################################################

# Generate models

# TIME
glm_vc <- glm(formula = immucomplete ~ agechild,
              data = ugasoc_vc,
              family = "binomial")
summary(glm_vc)
AIC(glm_vc)


# Show summary model stats
glance(glm_vc)


# Export regression results to CSV ("broom" package)
tidy_glm_vc <- tidy(glm_vc)
write.csv(tidy_glm_vc, file = paste0("glm_vc",write.time,".csv"))



################################################
# Table of vaccine coverage by Quantile
################################################

# Crosstab for PCV & MCV
ugasoc_vc_p <- ugasoc_vc %>%
  filter(disease == 3) %>%
  mutate(pcv = f4_pcv1 + f4_pcv2 + f4_pcv3)
ugasoc_vc_m <- ugasoc_vc %>%
  filter(disease == 2) %>%
  mutate(mcv = f4_measles1 + f4_measles2)

# Crosstab for income
crosstab(ugasoc_vc, row.vars = "hhhead_income_n5_wb", col.vars = c("disease_label", "immucomplete"), type = "r", percentages = FALSE, subtotals = TRUE)
crosstab(ugasoc_vc, row.vars = "hhhead_income_n5_wb", col.vars = "disease_label", type = "f", percentages = FALSE, subtotals = TRUE)
crosstab(ugasoc_vc_p, row.vars = "hhhead_income_n5_wb", col.vars = c("disease_label", "pcv"), type = "r", percentages = FALSE, subtotals = TRUE)
crosstab(ugasoc_vc_m, row.vars = "hhhead_income_n5_wb", col.vars = c("disease_label", "mcv"), type = "r", percentages = FALSE, subtotals = TRUE)

# Crosstab for consumption
crosstab(ugasoc_vc, row.vars = "hhexp_n5_wb", col.vars = c("disease_label", "immucomplete"), type = "r", percentages = FALSE, subtotals = TRUE)
crosstab(ugasoc_vc, row.vars = "hhexp_n5_wb", col.vars = "disease_label", type = "f", percentages = FALSE, subtotals = TRUE)
crosstab(ugasoc_vc_p, row.vars = "hhexp_n5_wb", col.vars = c("disease_label", "pcv"), type = "r", percentages = FALSE, subtotals = TRUE)
crosstab(ugasoc_vc_m, row.vars = "hhexp_n5_wb", col.vars = c("disease_label", "mcv"), type = "r", percentages = FALSE, subtotals = TRUE)



################################################################################################
# Income VS. Consumption testing
################################################


ugasoc_new <- bind_rows(ugasoc_p, ugasoc_d)
ugasoc_new <- bind_rows(ugasoc_new, ugasoc_m)


# By Nationally representative centiles
lm_wealth_centile <- lm(formula = hhexp_wb ~ hhhead_income_wb, data = ugasoc_new)
summary(lm_wealth_centile)
AIC(lm_wealth_centile)

cor_wealth_centile <- round(cor(x = ugasoc_new$hhexp_wb, y = ugasoc_new$hhhead_income_wb, method = "pearson"), digits = 4)

graph_wealth_centile <- ggplot(data = ugasoc_new, aes(x = hhexp_wb, y = hhhead_income_wb)) +
                          geom_count(alpha = 0.5, color = "blue") +
                          geom_abline(slope = 1) +
                          scale_size_continuous(breaks = c(2,4,6,8,10)) +
                          scale_color_continuous(breaks = c(2,4,6,8,10)) +
                          labs(title = "Wealth Ranking: Income VS. Consumption", subtitle = paste0("Pearson correlation coefficient = ",cor_wealth_centile), y = "Head of household income (ranking)", x = "Household consumption (ranking)")

# By USD (not grouped)
ugasoc_new <- ugasoc_new %>% mutate(hh_totalexpnf_month_USD = hh_totalexpnf_month/3727)
ugasoc_new <- ugasoc_new %>% mutate(hhhead_income_month_USD = hhhead_income_month/3727)

lm_wealth_amount <- lm(formula = hhhead_income_month_USD ~ hh_totalexpnf_month_USD, data = ugasoc_new)
summary(lm_wealth_amount)
AIC(lm_wealth_amount)

cor_wealth_amount <- round(cor(x = ugasoc_new$hhhead_income_month_USD, y = ugasoc_new$hh_totalexpnf_month_USD, method = "pearson"), digits = 4)

graph_wealth_amount <- ggplot(data = ugasoc_new, aes(x = hh_totalexpnf_month_USD, y = hhhead_income_month_USD)) +
  geom_point(alpha = 0.2, color = "blue") +
  geom_smooth(method = "lm", se = F) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Reported Estimates: Income VS. Consumption", subtitle = paste0("Pearson correlation coefficient = ",cor_wealth_amount), y = "Head of household income (amount)", x = "Household consumption (amount)")

# Graph
jpeg(filename = paste0("graph_wealth_",write.time,".jpg"), width = 1000, height = 300)
plot_grid(graph_wealth_centile, graph_wealth_amount)
dev.off()




### Remove income = 0 and consumption = 0

ugasoc_new_nozero <- ugasoc_new %>% filter(hh_totalexpnf_month_USD > 0 & hhhead_income_month_USD > 0)



# By Nationally representative centiles
lm_wealth_centile_nozero <- lm(formula = hhexp_wb ~ hhhead_income_wb, data = ugasoc_new_nozero)
summary(lm_wealth_centile_nozero)
AIC(lm_wealth_centile_nozero)

cor_wealth_centile_nozero <- round(cor(x = ugasoc_new_nozero$hhexp_wb, y = ugasoc_new_nozero$hhhead_income_wb, method = "pearson"), digits = 4)

graph_wealth_centile_nozero <- ggplot(data = ugasoc_new_nozero, aes(x = hhexp_wb, y = hhhead_income_wb)) +
  geom_count(alpha = 0.5, color = "blue") +
  geom_abline(slope = 1) +
  geom_smooth(method = "lm", se = F) +
  scale_size_continuous(breaks = c(2,4,6,8,10)) +
  scale_color_continuous(breaks = c(2,4,6,8,10)) +
  labs(title = "Wealth Ranking: Income VS. Consumption", subtitle = "Wealth ranking", y = "Head of household income (ranked)", x = "Household consumption (ranked)")

# By USD (not grouped)
lm_wealth_amount_nozero <- lm(formula = hhhead_income_month_USD ~ hh_totalexpnf_month_USD, data = ugasoc_new_nozero)
summary(lm_wealth_amount_nozero)
AIC(lm_wealth_amount_nozero)

cor_wealth_amount_nozero <- round(cor(x = ugasoc_new_nozero$hhhead_income_month_USD, y = ugasoc_new_nozero$hh_totalexpnf_month_USD, method = "pearson"), digits = 4)

graph_wealth_amount_nozero <- ggplot(data = ugasoc_new_nozero, aes(x = hh_totalexpnf_month_USD, y = hhhead_income_month_USD)) +
  geom_point(alpha = 0.2, color = "blue") +
  geom_smooth(method = "lm", se = F) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Reported Estimates: Income VS. Consumption", subtitle = "Amounts (2018 US Dollars)", y = "Head of household income", x = "Household consumption")


# Graph
jpeg(filename = paste0("graph_wealth_nozero_",write.time,".jpg"), width = 1000, height = 300)
plot_grid(graph_wealth_centile_nozero, graph_wealth_amount_nozero)
dev.off()



# Add all controls

lm_wealth_centile_nozero_full1 <- lm(formula = hhexp_wb ~ hhhead_income_wb + time + distance + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_new_nozero)
summary(lm_wealth_centile_nozero_full1)
AIC(lm_wealth_centile_nozero_full1)
lm_wealth_centile_nozero_full2 <- lm(formula = hhhead_income_wb ~ hhexp_wb + time + distance + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_new_nozero)
summary(lm_wealth_centile_nozero_full2)
AIC(lm_wealth_centile_nozero_full2)

lm_wealth_amount_nozero_full1 <- lm(formula = hh_totalexpnf_month_USD ~ hhhead_income_month_USD + time + distance + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_new_nozero)
summary(lm_wealth_amount_nozero_full1)
AIC(lm_wealth_amount_nozero_full1)
lm_wealth_amount_nozero_full2 <- lm(formula = hhhead_income_month_USD ~ hh_totalexpnf_month_USD + time + distance + sector_public + district_label + residence_label + male_child + male_caregiver + agechild + age_caregiver + education, data = ugasoc_new_nozero)
summary(lm_wealth_amount_nozero_full2)
AIC(lm_wealth_amount_nozero_full2)


# Show summary model stats
glance(lm_wealth_centile_nozero_full1)
glance(lm_wealth_centile_nozero_full2)


# Export regression results to CSV ("broom" package)
tidy_lm_wealth_centile_nozero_full1 <- tidy(lm_wealth_centile_nozero_full1)
write.csv(tidy_lm_wealth_centile_nozero_full1, file = paste0("lm_wealth_centile_nozero_full1_",write.time,".csv"))
tidy_lm_wealth_centile_nozero_full2 <- tidy(lm_wealth_centile_nozero_full2)
write.csv(tidy_lm_wealth_centile_nozero_full2, file = paste0("lm_wealth_centile_nozero_full2_",write.time,".csv"))



### Compare with World Bank data

cor_wealth_amount_wb <- round(cor(x = hhincexp$totalconsumption_USD, y = hhincexp$income_USD, method = "pearson", use = "complete.obs"), digits = 4)
graph_wealth_amount_wb <- ggplot(data = hhincexp, aes(x = totalconsumption_USD, y = income_USD)) +
  geom_point(alpha = 0.2, color = "blue") +
  geom_smooth(method = "lm", se = F) +
  geom_abline(slope = 1) +
  scale_x_log10() + scale_y_log10() +
  labs(title = "Reported Estimates: Income VS. Consumption", subtitle = paste0("Pearson correlation coefficient = ",cor_wealth_amount_wb), y = "Head of household income (amount)", x = "Household consumption (amount)")
graph_wealth_amount_wb




################################################################################################
# Other graphs and tables
################################################

ugasoc_new$disease_label <- ordered(ugasoc_new$disease_label, levels = c("pneumonia", "diarrhea", "measles"))


# Direct medical costs for caregivers
graph_dmc_caregiver_c <- ugasoc_new %>% filter(sector == 1) %>%
  mutate(bin = cut_width(hhexp_wb, width = 0.2, boundary = 0)) %>%
  ggplot(aes(x = bin, y = dmc_caregiver_c, fill = disease_label)) +
  geom_boxplot() +
  scale_y_log10() +
  scale_fill_manual(values = c("palegreen3", "palegreen2", "palegoldenrod"),
                    name = "Disease",
                    breaks = c("pneumonia", "diarrhea", "measles"),
                    labels = c("Pneumonia", "Diarrhea", "Measles")) +
  labs(title = "Household Out-of-Pocket Payment at Public Facilities", subtitle = "n = 799 children", x = "Centiles (World Bank)", y = "US dollars")
graph_dmc_caregiver_c

graph_dmc_caregiver <- ugasoc_new %>%
  mutate(bin = cut_width(hhexp_wb, width = 0.2, boundary = 0)) %>%
  ggplot(aes(x = bin, y = dmc_caregiver, fill = as.factor(ipd))) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ disease_label) +
  scale_fill_manual(values = c("royalblue2", "tomato1"),
                    name = "Type of care provided",
                    breaks = c(1,0),
                    labels = c("Hospitalized", "Ambulatory")) +
  labs(title = "Household Out-of-Pocket Payments", subtitle = "Medical costs", x = "Centiles (World Bank)", y = "US dollars")
graph_nmc_caregiver <- ugasoc_new %>%
  mutate(bin = cut_width(hhexp_wb, width = 0.2, boundary = 0)) %>%
  ggplot(aes(x = bin, y = nmc_caregiver, fill = as.factor(ipd))) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ disease_label) +
  scale_fill_manual(values = c("royalblue2", "tomato1"),
                    name = "Type of care provided",
                    breaks = c(1,0),
                    labels = c("Hospitalized", "Ambulatory")) +
  labs(title = "Household Out-of-Pocket Payments", subtitle = "Non-medical costs", x = "Centiles (World Bank)", y = "US dollars")
graph_nmc_caregiver


# Graph
jpeg(filename = paste0("graph_x_dmc_caregiver_c_",write.time,".jpg"), width = 1000, height = 500)
graph_dmc_caregiver_c
dev.off()
jpeg(filename = paste0("graph_x_dmc_caregiver_",write.time,".jpg"), width = 1000, height = 300)
graph_dmc_caregiver
dev.off()
jpeg(filename = paste0("graph_x_nmc_caregiver_",write.time,".jpg"), width = 1000, height = 300)
graph_nmc_caregiver
dev.off()


################################################################################################
# Catastrophic Health Expenditures (nationally representative)
################################################

# Generate CHE
## CHE based on 40 % (without food)
## Note: all_caregiver and hh_totalexpnf_month_USD are both in USD
ugasoc_new <- ugasoc_new %>% mutate(che = all_caregiver / hh_totalexpnf_month_USD)
ugasoc_new <- ugasoc_new %>% mutate(che40 = case_when(
  che < 0.40 ~ 0,
  che >= 0.40 ~ 1))

# Summarize into proportions
prop_che40 <- ugasoc_new %>%
  group_by(disease_label, ipd, hhexp_n5_wb, che40) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

# Append missing measles values
prop_che40 <- rbind(prop_che40, data.frame(disease_label = "measles", ipd = 0, hhexp_n5_wb = 0.2, che40 = 1, n = 0, prop = 0))
prop_che40 <- rbind(prop_che40, data.frame(disease_label = "measles", ipd = 0, hhexp_n5_wb = 0.6, che40 = 1, n = 0, prop = 0))
prop_che40 <- rbind(prop_che40, data.frame(disease_label = "measles", ipd = 0, hhexp_n5_wb = 0.8, che40 = 1, n = 0, prop = 0))

# Filter to keep che40 == 1
prop_che40 <- prop_che40 %>% filter(che40 == 1)

prop_che40$disease_label <- ordered(prop_che40$disease_label, levels = c("pneumonia", "diarrhea", "measles"))

# Graph che40
graph_che40 <- prop_che40 %>%
  ggplot(aes(x = hhexp_n5_wb, y = prop, fill = as.factor(ipd))) +
  geom_col(position = "dodge") +
  facet_wrap(~ disease_label) +
  scale_y_continuous(limits = c(0,1), labels = scales::percent) +
  scale_x_continuous(breaks = c(0.2,0.4,0.6,0.8,1.0)) +
  scale_fill_manual(values = c("royalblue2", "tomato1"),
                    name = "Type of care provided",
                    breaks = c(1,0),
                    labels = c("Hospitalized", "Ambulatory")) +
  labs(title = "Household Catastrophic Health Expenditure", subtitle = "Out-of-pocket payments over Household consumption (without food)", x = paste0("Quantiles ",write.graph2), y = "Proportion of households in quantile")
graph_che40

jpeg(filename = paste0("graph_che40_",write.time,".jpg"), width = 1000, height = 300)
graph_che40
dev.off()


################################################################################################
# Frequency of SES ranks (nationally representative)
################################################

# By amount (World Bank data)
graph_freq_ses_wb_exp <- ggplot(hhincexp, aes(x = totalconsumption_USD)) +
  geom_histogram(bins = 20, color = "black", fill = "grey40") +
  labs(title = "Wealth distribution (national estimate)",
       subtitle = paste0("n = 3159, mean = $", round(mean(hhincexp$totalconsumption_USD), digits = 2), ", median = $", round(median(hhincexp$totalconsumption_USD), digits = 2), ", SD = ", round(sd(hhincexp$totalconsumption_USD), digits = 1), ", SE = ", round((sd(hhincexp$totalconsumption_USD)/sqrt(3159)), digits = 1)),
       y = "Count of observations",
       x = "Household consumption (2018 USD)") +
  scale_x_log10(limits = c(0.1,10000)) +
  theme(legend.position = "none")
graph_freq_ses_wb_exp

# By amount (sample)
graph_freq_ses_exp <- ggplot(ugasoc_new, aes(x = hh_totalexpnf_month_USD)) +
  geom_histogram(bins = 20, color = "black", fill = "deeppink3") +
  labs(title = "Wealth distribution (study estimate)",
       subtitle = paste0("n = 1436, mean = $", round(mean(ugasoc_new$hh_totalexpnf_month_USD), digits = 2), ", median = $", round(median(ugasoc_new$hh_totalexpnf_month_USD), digits = 2), ", SD = ", round(sd(ugasoc_new$hh_totalexpnf_month_USD), digits = 1), ", SE = ", round((sd(ugasoc_new$hh_totalexpnf_month_USD)/sqrt(1436)), digits = 1)),
       y = "Count of observations",
       x = "Household consumption (2018 USD)") +
  scale_x_log10(limits = c(0.1,10000)) +
  theme(legend.position = "none")
graph_freq_ses_exp

# By amount & disease
graph_freq_ses_exp_disease <- ggplot(ugasoc_new, aes(x = hh_totalexpnf_month_USD, fill = disease_label)) +
  geom_histogram(bins = 20, color = "black") +
  labs(title = "Wealth distribution (study estimate)",
       subtitle = "n = 1436",
       y = "Count of observations",
       x = "Household consumption (2018 USD)") +
  facet_grid(ipd_label ~ disease_label) +
  scale_fill_manual(values = c("palegreen3", "palegreen2", "palegoldenrod"),
                    name = "Disease",
                    breaks = c("pneumonia", "diarrhea", "measles"),
                    labels = c("Pneumonia", "Diarrhea", "Measles")) +
  scale_x_log10(limits = c(0.1,10000)) +
  theme(legend.position = "none")
graph_freq_ses_exp_disease

# By grouping & disease
graph_freq_all <- ggplot(ugasoc_new, aes(x = hhexp_wb, fill = disease_label)) +
  geom_histogram(bins = 20, color = "black") +
  labs(title = "Count of observations by wealth centile",
       subtitle = "n = 1436",
       y = "Count of observations",
       x = "Proportion of population (ranked by consumption centile)") +
  facet_grid(ipd_label ~ disease_label) +
  scale_fill_manual(values = c("palegreen3", "palegreen2", "palegoldenrod"),
                    name = "Disease",
                    breaks = c("pneumonia", "diarrhea", "measles"),
                    labels = c("Pneumonia", "Diarrhea", "Measles")) +
  theme(legend.position = "none")
graph_freq_all

jpeg(filename = paste0("graph_freq_all_",write.time,".jpg"), width = 700, height = 500)
graph_freq_all
dev.off()

jpeg(filename = paste0("graph_freq_ses_exp_",write.time,".jpg"), width = 700, height = 300)
plot_grid(graph_freq_ses_exp, graph_freq_ses_wb_exp, label_x = 'Household consumption (2018 USD)')
dev.off()


### Additional graphs

graph_freq_ses_wb_exp_n100 <- ggplot(hhincexp, aes(x = exp_n100)) +
  geom_histogram(bins = 20, color = "black") +
  labs(title = "Distribution of households by consumption level",
       subtitle = "n = 3159",
       y = "Count of observations",
       x = "Proportion of population (ranked by consumption)") +
  theme(legend.position = "none")
graph_freq_ses_wb_exp_n100


graph_freq_ses_wb_inc <- ggplot(hhincexp, aes(x = income_USD)) +
  geom_histogram(bins = 20, color = "black") +
  labs(title = "Distribution of households by income level",
       subtitle = "n = 618",
       y = "Count of observations",
       x = "Proportion of population (ranked by consumption)") +
  scale_x_log10() +
  theme(legend.position = "none")
graph_freq_ses_wb_inc

graph_freq_ses_wb_inc_n100 <- ggplot(hhincexp, aes(x = income_n100)) +
  geom_histogram(bins = 100, color = "black") +
  labs(title = "Count of observations by wealth centile",
       subtitle = "n = 618",
       y = "Count of observations",
       x = "Proportion of population (ranked by consumption)") +
  theme(legend.position = "none")
graph_freq_ses_wb_inc_n100




















