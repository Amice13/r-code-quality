rm(list=ls())

setwd("")

library(tidyverse)
library(haven)
library(readxl)

# Read Afrobarometer data into R
abr6 <- read_sav("merged_r6_data_2016_36countries2.sav", encoding="latin1")

# Subset to relevant variables
abr6 <- abr6 %>%
  select(RESPNO,URBRUR,REGION,Q1,Q8A,Q8E,
         Q30,Q66D,
         Q76,Q87,Q88B,Q89B,Q89E,
         Q91A,Q91B,Q91C,Q91D,Q95,Q96A,Q96B,Q97,Q101,Combinwt)

howoften <- function(x) {
  recode(as_factor(x),
         "Never" = 1,
         "Just once or twice" = 2,
         "Several times" = 3,
         "Many times" = 4,
         "Always" = 5)
}

# Recode variables
abr6 <- abr6 %>%
  mutate(COUNTRY = substr(RESPNO,1,3),
         rural = if_else(URBRUR==2,1,0),
         female = if_else(Q101==2,1,
                          if_else(is.na(Q101)==FALSE,0,NA_real_)),
         age = if_else(Q1>=18 & Q1<900,Q1,NA_real_),
         dem = recode(as_factor(Q30),
                      "STATEMENT 1: Democracy preferable" = 1,
                      "STATEMENT 2: Sometimes non-democratic preferable" = 0,
                      "STATEMENT 3: Doesn't matter" = 0),
         highprices = recode(as_factor(Q66D),
                             "Very Badly" = 1,
                             "Fairly Badly" = 1,
                             "Fairly Well" = 0,
                             "Very Well" = 0,
                             "Don't know / Haven't heard enough" = 0),
         openborders = recode(as_factor(Q76),
                              "Agree very strongly with 1" = 4,
                              "Agree with 1" = 3,
                              "Agree with 2" = 2,
                              "Agree very strongly with 2" = 1),
         openborders_dum = recode(as_factor(Q76),
                                  "Agree very strongly with 1" = 1,
                                  "Agree with 1" = 1,
                                  "Agree with 2" = 0,
                                  "Agree very strongly with 2" = 0),
         openborders_dk = recode(as_factor(Q76),
                                 "Agree very strongly with 1" = "Support",
                                 "Agree with 1" = "Support",
                                 "Agree with 2" = "Oppose",
                                 "Agree very strongly with 2" = "Oppose",
                                  "Donâ\u0080\u0099t know" = "DK",
                                 "Agree with neither" = "Neither",
                                 "Missing" = NA_character_,
                                 "Refused" = NA_character_
                                 ),
         natlidonly = if_else(Q87==9990,1,0),
         natlideth = recode(as_factor(Q88B),
                         "I feel only (ethnic group)" = 0,
                         "I feel more (ethnic group) than (national identity)" = 0,
                         "I feel equally (national identity) and (ethnic group)" = 1,
                         "I feel more (national identity) than (ethnic group)" = 2,
                         "I feel only (national identity)" = 2),
         natlid = if_else(natlidonly==1,2,natlideth),
         ethno = recode(as_factor(Q89B),
                        "Strongly  dislike" = 1,
                        "Somewhat dislike" = 1,
                        "Somewhat  like" = 0,
                        "Strongly like" = 0,
                        "Would not care" = 0),
         xeno = recode(as_factor(Q89E),
                       "Strongly  dislike" = 1,
                       "Somewhat dislike" = 1,
                       "Somewhat  like" = 0,
                       "Strongly like" = 0,
                       "Would not care" = 0),
         employment = recode_factor(as_factor(Q95),
                                    "Yes, full time" = "Employed",
                                    "Yes, part time" = "Employed",
                                    "No (looking)" = "Unemployed (looking)",
                                    "No (not looking)" = "Unemployed (not looking)"),
         skill = recode(as_factor(Q96A), # drop "Housewife / homemaker" as NA
                        "Never had a job" = 0,
                        "Student" = 2,
                        "Agriculture / farming / fishing / forestry"  = 0,
                        "Trader / hawker / vendor" = 0,
                        "Retail / Shop" = 1,
                        "Unskilled manual worker" = 0,
                        "Artisan or skilled manual worker" = 1,
                        "Clerical or secretarial" = 2,
                        "Supervisor / Foreman / Senior Manager"  = 2,
                        "Security services" = 1,
                        "Mid-level professional" = 2,
                        "Upper-level professional" = 2),
         employer = recode_factor(as_factor(Q96B),
                                  "Works for  self" = "Self-employed",
                                  "Private sector" = "Private sector",
                                  "Non Governmental Organizations / civil society sector" = "NGOs / civil society",
                                  "Government" = "Public sector"),
         publicsector = if_else(employer=="Public sector",1,0),
         edu = recode(as_factor(Q97),
                      "No formal schooling" = 1,
                      "Informal schooling only" = 2,
                      "Some primary schooling" = 3,
                      "Primary school completed" = 4,
                      "Some secondary school / high school" = 5,
                      "Secondary school / high school completed" = 6,
                      "Post-secondary qualifications, other than university" = 7,
                      "Some university" = 8,
                      "University completed" = 9,
                      "Post-graduate"  = 10),
         primary = if_else(edu>=4,1,
                           if_else(is.na(edu)==FALSE,0,NA_real_)),
         secondary = if_else(edu>=6,1,
                             if_else(is.na(edu)==FALSE,0,NA_real_)),
         highered = if_else(edu>=7,1,
                            if_else(is.na(edu)==FALSE,0,NA_real_)),
         college = if_else(edu>=8,1,
                           if_else(is.na(edu)==FALSE,0,NA_real_)),
         farmer = if_else(as_factor(Q96A)=="Agriculture / farming / fishing / forestry",1,0),
         self_employed = if_else(as_factor(employer)=="Self-employed",1,0),
         landowner = farmer*self_employed,
         radio = if_else(Q91A==2,2,
                         if_else(Q91A==1,1,0)),
         tv = if_else(Q91B==2,2,
                      if_else(Q91B==1,1,0)),
         car = if_else(Q91C==2,2,
                       if_else(Q91C==1,1,0)),
         phone = if_else(Q91D==2,2,
                        if_else(Q91D==1,1,0)),
         assetindex = radio+tv+car+phone,
         pov_food = howoften(Q8A),
         pov_cash = howoften(Q8E),
  )

# Merge country covariates
abr6 <- abr6 %>%
  mutate(ccode = recode_factor(as_factor(COUNTRY),
                       "ALG" = "DZA",
                       "BFO" = "BFA",
                       "BOT" = "BWA",
                       "CAM" = "CMR",
                       "CDI" = "CIV",
                       "CVE" = "CPV",
                       "GAM" = "GMB",
                       "GUI" = "GIN",
                       "LES" = "LSO",
                       "LIB" = "LBR",
                       "MAD" = "MDG",
                       "MLW" = "MWI",
                       "MAU" = "MUS",
                       "MOR" = "MAR",
                       "NGR" = "NER",
                       "NIG" = "NGA",
                       "SAF" = "ZAF",
                       "SRL" = "SLE",
                       "SUD" = "SDN",
                       "TAN" = "TZA",
                       "TOG" = "TGO",
                       "ZIM" = "ZWE",
                       "ZAM" = "ZMB"))

wdi <- read_csv("WDI download June 8 2022.csv")
wdi <- wdi %>%
  slice(-c(77:81)) %>%
  rename(cname = `Country Name`,
         ccode = `Country Code`,
         year = Time,
         exports = `Exports of goods and services (% of GDP) [NE.EXP.GNFS.ZS]`,
         gdppc = `GDP per capita (current US$) [NY.GDP.PCAP.CD]`,
         arableland = `Arable land (hectares) [AG.LND.ARBL.HA]`) %>%
  select(cname,ccode,year,exports,gdppc,arableland) %>%
  filter(year==2014) %>%
  mutate(ln_gdppc = log(gdppc))

pwt <- read_excel("pwt100.xlsx", sheet = "Data")
pwt <- pwt %>%
  filter(year==2014) %>%
  select(countrycode,rnna)

wdi <- left_join(wdi,pwt,by= c("ccode"="countrycode"))

wdi <- wdi %>%
  mutate(landabundance = arableland/rnna,
         ln_landabundance = log(arableland/rnna)) %>%
  mutate(landabundant = as.numeric(landabundance>median(landabundance)))

abr6 <- left_join(abr6,wdi, by = "ccode")  

wep <- read_csv("10_31_19_0306pm_wep.csv")
wep <- wep %>%
  mutate(workingpopstock = workingpop_WDI/100*pop_WDI,
         tertiarystock = lh_25999_BL/100*workingpopstock,
         secondarystock = ls_25999_BL/100*workingpopstock,
         lowedstock = workingpopstock-secondarystock,
         skillratio = secondarystock/lowedstock) %>%
  select(country,ifs,AB6,AB8,year,trade_WDI,polity2_P4,conflictincidence_UCDP,v2x_freexp_VDEM,skillratio)

abr6 <- left_join(abr6,wep, by = c("ccode" = "ifs"))  

rhci <- read_excel("WITS_RCI.xlsx", sheet = "Sheet1")
abr6 <- left_join(abr6,rhci, by = "ccode")

save(abr6,file="abr6_clean.RData")
