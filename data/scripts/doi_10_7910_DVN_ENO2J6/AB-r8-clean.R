rm(list=ls())
setwd("")

library(tidyverse)
library(haven)
library(readxl)

# load merged file
abr8 <- read_sav("afrobarometer_release-dataset_merge-34ctry_r8_en_2023-03-01.sav")

# Subset to relevant variables
abr8 <- abr8 %>%
      select(RESPNO,DATEINTR,URBRUR,REGION,
       Q1,Q7A,Q7E,Q21,Q27,Q28,Q29,Q31,Q39A,Q39B,Q41A,Q41B,Q41C,Q41D,Q41E,Q41F,Q41G,Q41H,
       Q41I,Q41J,Q41K,Q41L,
       Q42A,Q42B,Q42C,Q42D,Q42E,Q42F,Q42G,Q42H,Q42I,Q43A,Q43B,Q45,Q48PT1,Q48PT2,Q48PT3,
       Q50D,Q61,Q63,Q64A,Q64B,Q65A,Q65B,Q65C,Q65D,Q66,Q67,Q69A,Q69B,
       Q70A,Q70B,Q70C,Q70D,Q70E,Q70F,Q70G,Q70H,Q82B,Q86B,Q86D,Q95A,Q95B,Q95C,Q95D,
       Q97,Q101,
       Q27,Q28,Q29,Q31,Q39A,Q39B,Q41A,Q41B,Q41C,Q41D,Q41E,Q41F,Q41G,Q41H,Q41I,Q41J,Q41K,Q41L,
       Q42A,Q42B,Q42C,Q42D,Q42E,Q42F,
       Q43A,Q43B,Q45,Q48PT1,Q48PT2,Q48PT3,
       Q50D,Q63,Q64A,Q64B,Q65A,Q65B,Q65C,Q65D,Q69A,Q69B,
       Q70A,Q70B,Q70C,Q70D,Q70E,Q70F,Q70G,Q70H,
       Q81,Q82B,Q86B,Q86D, # ethnicity and culture
        Q92A,Q92B,Q92C,Q92D,Q92E,Q92F,
        withinwt_ea,withinwt_hh)

# Functions for coding scaled variables
s1_hi <- function(x){
  recode(as_factor(x),
                "Agree very strongly with 1" = 4,
                "Agree with 1" = 3,
                "Agree with 2" = 2,
                "Agree very strongly with 2" = 1)
}
s1_hi_dum <- function(x){
  recode(as_factor(x),
                "Agree very strongly with 1" = 1,
                "Agree with 1" = 1,
                "Agree with 2" = 0,
                "Agree very strongly with 2" = 0)
}
agree_hi <- function(x){
  recode(as_factor(x),
         "Strongly agree" = 5,
         "Agree" = 4,
         "Neither agree nor disagree" = 3,
         "Disagree" = 2,
         "Strongly disagree" = 1)
}
howmuch <- function(x){
  recode(as_factor(x),
         "A lot" = 4,
         "Somewhat" = 3,
         "Just a little" = 2,
         "Not at all" = 1)
}
howmany <- function(x){
  recode(as_factor(x),
         "All of them" = 4,
         "Most of them" = 3,
         "Some of them" = 2,
         "None" = 1)
}
yes_1 <- function(x){
  recode(as_factor(x),
         "Yes" = 1,
         "No" = 0)
}
howpositive <- function(x){
  recode(as_factor(x),
         "Very positive" = 5,
         "Somewhat positive" = 4,
         "Neither positive nor negative" = 3,
         "Somewhat negative" = 2,
         "Very negative" = 1)
}
howoften <- function(x) {
  recode(as_factor(x),
                        "Never" = 1,
                        "Just once or twice" = 2,
                        "Several times" = 3,
                        "Many times" = 4,
                        "Always" = 5)
}

# Recode variables
abr8 <- abr8 %>%
  mutate(COUNTRY = substr(RESPNO,1,3),
         rural = if_else(URBRUR==2,1,0),
         female = if_else(Q101==2,1,
                          if_else(is.na(Q101)==FALSE,0,NA_real_)),
         age = if_else(Q1>=18 & Q1<900,Q1,NA_real_),
         dem = recode(as_factor(Q21),
                             "STATEMENT 1: Democracy is preferable to any other kind of government." = 1,
                             "STATEMENT 2: In some circumstances, a non-democratic government can be preferable." = 0,
                             "STATEMENT 3: For someone like me, it doesn’t matter what kind of government we have." = 0),
         highprices = recode(as_factor(Q50D),
                                     "Very Badly" = 1,
                                     "Fairly Badly" = 1,
                                     "Fairly Well" = 0,
                                     "Very Well" = 0,
                                     "Don't Know / Haven't heard enough to say" = 0),
         openborders = s1_hi(Q61),
         openborders_dum = s1_hi_dum(Q61),
         trade = s1_hi(Q66),
         trade_dum = s1_hi_dum(Q66),
         trade_dk = recode(as_factor(Q66),
                           "Agree very strongly with 1" = "Support",
                           "Agree with 1" = "Support",
                           "Agree with 2" = "Oppose",
                           "Agree very strongly with 2" = "Oppose",
                           "Agree with neither" = "Neither",
                           "Don't know" = "DK",
                           "Refused" = NA_character_,
                           "Missing" = NA_character_),
         foreigntraders = s1_hi(Q67),
         foreigntraders_dum = s1_hi_dum(Q67),
         natlidonly = if_else(Q81==9990,1,0),
         natlideth = recode(as_factor(Q82B),
                            "I feel only (ethnic group)" = 0,
                            "I feel more (ethnic group) than (national identity)" = 0,
                            "I feel equally (national identity) and (ethnic group)" = 1,
                            "I feel more (national identity) than (ethnic group)" = 2,
                            "I feel only (national identity)" = 2),
         natlid = if_else(natlidonly==1,2,natlideth),
         ethno = recode(as_factor(Q86B),
                                "Strongly  dislike" = 1,
                                "Somewhat dislike" = 1,
                                "Somewhat like" = 0,
                                "Strongly like" = 0,
                                "Would not care" = 0),
         xeno = recode(as_factor(Q86D),
                               "Strongly  dislike" = 1,
                               "Somewhat dislike" = 1,
                               "Somewhat like" = 0,
                               "Strongly like" = 0,
                               "Would not care" = 0),
         employment = recode_factor(as_factor(Q95A),
                                            "Yes, full time" = "Employed",
                                            "Yes, part time" = "Employed",
                                            "No (looking)" = "Unemployed (looking)",
                                            "No (not looking)" = "Unemployed (not looking)"),
         skill = recode(as_factor(Q95C), # drop "Housewife / homemaker" as NA
                               "Never had a job" = 0,
                               "Student" = 2,
                               "Agriculture / farming / fishing / forestry"  = 0,
                               "Trader / hawker / vendor" = 0,
                               "Retail / Shop" = 1,
                               "Unskilled manual worker (e.g., cleaner, laborer, domestic help, unskilled manufacturing worker)" = 0,
                               "Artisan or skilled manual worker (e.g., trades like electrician, mechanic, machinist or skilled manufacturing worker)" = 1,
                               "Clerical or secretarial" = 2,
                               "Supervisor / Foreman / Senior Manager"  = 2,
                               "Security services (police, army, private security)" = 1,
                               "Mid-level professional (e.g., teacher, nurse, mid-level government officer)" = 2,
                               "Upper-level professional (e.g., banker/finance, doctor, lawyer, engineer, accountant, professor, senior-level government" = 2),
         employer = recode_factor(as_factor(Q95D),
                                    "Works for self" = "Self-employed",
                                    "Private sector" = "Private sector",
                                    "Non Governmental Organizations / Civil society sector" = "NGOs / civil society",
                                    "Government" = "Public sector"),
         publicsector = if_else(employer=="Public sector",1,0),
         edu = recode(as_factor(Q97),
                             "No formal schooling" = 1,
                             "Informal schooling only (including Koranic schooling)" = 2,
                             "Some primary schooling" = 3,
                             "Primary school completed" = 4,
                             "Intermediate school or Some secondary school / high school" = 5,
                             "Secondary school / high school completed" = 6,
                             "Post-secondary qualifications, other than university e.g. a diploma or degree from a polytechnic or college" = 7,
                             "Some university" = 8,
                             "University completed" = 9,
                             "Post-graduate"  = 10),
         primary = if_else(edu >= 4,1,
                           if_else(is.na(edu)==FALSE,0,NA_real_)),
         secondary = if_else(edu>=6,1,
                           if_else(is.na(edu)==FALSE,0,NA_real_)),
         highered = if_else(edu>=7,1,
                             if_else(is.na(edu)==FALSE,0,NA_real_)),
         college = if_else(edu>=8,1,
                             if_else(is.na(edu)==FALSE,0,NA_real_)),
         farmer = if_else(as_factor(Q95C)=="Agriculture / farming / fishing / forestry",1,0),
         self_employed = if_else(as_factor(employer)=="Self-employed",1,0),
         landowner = farmer*self_employed,
         radio = if_else(Q92A==2,2,
                        if_else(Q92A==1,1,0)),
         tv = if_else(Q92B==2,2,
                      if_else(Q92B==1,1,0)),
         car = if_else(Q92C==2,2,
                       if_else(Q92C==1,1,0)),
         comp = if_else(Q92D==2,2,
                        if_else(Q92D==1,1,0)),
         bank = if_else(Q92E==2,2,
                        if_else(Q92E==1,1,0)),
         phone = if_else(Q92F==2,2,
                         if_else(Q92F==1,1,0)),
         assetindex = radio+tv+car+comp+bank+phone,
         pov_food = howoften(Q7A),
         pov_cash = howoften(Q7E),
         natlid = if_else(Q81==9990,2,
                          if_else(Q82B%in%c(5,4),2,
                                  if_else(Q82B==3,1,0))),
         postcovid = as.numeric(as.Date(DATEINTR)>"2020-04-03"), # no interviews were conducted between April 3, 2020 and October 19, 2020, so we use this as a cut point for the pandemic
         ethnicity = as_factor(Q81)
         )

# Combined weights for round 8 do not seem to be available
# generate them manually by reweighting so that all national populations are equal in size

abr8 %>%
  mutate(withinwt_ea = as.numeric(withinwt_ea)) %>%
  group_by(COUNTRY) %>%
  summarise(totalwt = sum(withinwt_ea)) %>%
  print(n=34)

abr8 <- abr8 %>%
  mutate(Combinwt = if_else(COUNTRY == "ETH", withinwt_ea*(1200/2378),
                            if_else(COUNTRY %in% c("GHA","KEN"), withinwt_ea*(1200/2400),
                                    if_else(COUNTRY == "NIG", withinwt_ea*(1200/1599),
                                            if_else(COUNTRY == "SUD", withinwt_ea*(1200/1800),
                                                    if_else(COUNTRY == "TAN", withinwt_ea*(1200/2398),
                                                            if_else(COUNTRY == "CMR", withinwt_ea*(1200/2400),
                                                                    if_else(COUNTRY == "MOZ", withinwt_ea*(1200/1106),
                                                                            if_else(COUNTRY == "SAF", withinwt_ea*(1200/1600),
                                                                                    if_else(COUNTRY == "ANG", withinwt_ea*(1200/2402),
                                                            withinwt_ea))))))))))

# standardize country codes to merge with WDI
abr8 <- abr8 %>%
  mutate(ccode = recode_factor(as_factor(COUNTRY), # AB = WDI
                               "ANG" = "AGO", # Angola
                               "BFO" = "BFA", # Burkina
                               "BOT" = "BWA", # Botswana
                               "CAM" = "CMR", # Cameroon
                               "CDI" = "CIV", # Cote d'Ivoire
                               "CVE" = "CPV", # Cape Verde
                               "GAM" = "GMB", # Gambia
                               "GUI" = "GIN", # Guinea
                               "LES" = "LSO", # Lesotho
                               "LIB" = "LBR", # Liberia
                               "MAU" = "MUS", # Mauritius
                               "MLW" = "MWI", # Malawi
                               "MOR" = "MAR", # Morocco
                               "NIG" = "NGA", # Nigeria
                               "NGR" = "NER", # Niger
                               "SRL" = "SLE", # Sierra Leone
                               "SAF" = "ZAF", # South Africa
                               "SUD" = "SDN", # Sudan
                               "TAN" = "TZA", # Tanzania
                               "TOG" = "TGO", # Togo
                               "ZAM" = "ZMB", # Zambia
                               "ZIM" = "ZWE")) # Zimbabwe
    
wdi <- read_csv("WDI download June 8 2022.csv")

wdi <- wdi %>%
  slice(-c(79:83)) %>%
  rename(cname = `Country Name`,
         ccode = `Country Code`,
         year = Time,
         exports = `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]`,
         gdppc = `GDP per capita (current US$) [NY.GDP.PCAP.CD]`,
         arableland = `Arable land (hectares) [AG.LND.ARBL.HA]`) %>%
  select(cname,ccode,year,exports,gdppc,arableland) %>%
  filter(year==2019) %>%
  mutate(ln_gdppc = log(gdppc))

pwt <- read_excel("pwt100.xlsx", sheet = "Data")
pwt <- pwt %>%
  filter(year==2019) %>%
  select(countrycode,rnna)

wdi <- left_join(wdi,pwt,by= c("ccode"="countrycode"))

wdi <- wdi %>%
  mutate(landabundance = arableland/rnna,
         ln_landabundance = log(arableland/rnna)) %>%
  mutate(landabundant = as.numeric(landabundance>median(landabundance)))

abr8 <- left_join(abr8,wdi, by = "ccode") 

wep <- read_csv("10_31_19_0306pm_wep.csv") # note these data are for 2014, as 2019 data are unavailable
wep <- wep %>%
  mutate(workingpopstock = workingpop_WDI/100*pop_WDI,
         tertiarystock = lh_25999_BL/100*workingpopstock,
         secondarystock = ls_25999_BL/100*workingpopstock,
         lowedstock = workingpopstock-secondarystock,
         skillratio = secondarystock/lowedstock) %>%
  select(country,ifs,AB6,AB8,year,trade_WDI,polity2_P4,conflictincidence_UCDP,v2x_freexp_VDEM,skillratio)

abr8 <- left_join(abr8,wep, by = c("ccode" = "ifs"))  

rhci <- read_excel("WITS_RCI.xlsx", sheet = "Sheet1")
abr8 <- left_join(abr8,rhci, by = "ccode")
   
save(abr8,file="abr8_clean.RData")

