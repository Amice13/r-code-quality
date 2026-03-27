# This analysis was conducted using R Studio Version 1.1.419 for Mac and R version 3.5.2 for Mac
install.packages("pacman")
pacman::p_load(foreign, ggplot2, plm, reshape2, countrycode, sandwich, lmtest, MASS, 
               rworldmap, RColorBrewer, states, mice, VIM, stargazer, margins, clusterSEs, lme4, optimx,
               coefplot, topicmodels, tm, tidytext, dplyr, wordcloud, survey, sampleSelection, quanteda,
               fastDummies, readr) # load all relevant packages

setwd("ENTER DIRECTORY") # set working directory (change to your directory)

######### Create Datasets ########
##->## Main Data
wb <- read.csv("Dev_Policy_Action_Database_10.18.csv", fileEncoding="UTF-8-BOM") # load raw data from World Bank's Development Policy Action database, 
# which was obtained via an access to information request in October 2018. Note that I changed the "prsc" column values where a dash was present to "N" to resolve an import error.
wb$ccode <- countrycode(wb$ctry, origin = "country.name", destination = "iso3c") # create country code column in iso3c format

stone <- read.csv("Policy_Categories.csv", fileEncoding="UTF-8-BOM") # load hand-coded data that counts the number of policy categories into which
# a project's conditions fall; they are the same categories used by Kentikelenis et al. (2016); I hand-code to harmonize
# the development policy action data with the Kentikelenis IMF data. This serves as an alternate DV to the count of conditions variable.
# It is based on the DV utilized by Stone (2008)
wb$numcats <- stone$numcats # data was hand-coded to be appended directly to original data

wdi <- read.csv("WDI_11.18.csv", fileEncoding="UTF-8-BOM") # load data from World Development Indicators, batch downloaded in November 2018 (column names were edited in excel but are intuitive)
wdi <- wdi[-c(2,15)] # delete time code and US aid col, which was in current prices, to be replaced with constant price data from OECD
wdi[wdi == ".."] <- NA # change dot notation used to mark NAs in WDI data to "NA"
colnames (wdi) <- c("year", "ctry", "ccode", "pop", "gdp", "dserv", "dshorttoexports", "openness",
                    "reserves", "curracctoGDP", "debt", "FDItoGDP", "inflation", "DACaid")
wdi$gdp <- as.numeric(as.character(wdi$gdp)) # change data format to construct other variables
wdi$pop <- as.numeric(as.character(wdi$pop))
wdi$gdppc <- wdi$gdp / wdi$pop # calculate per capita gdp
wdi$dserv <- as.numeric(as.character(wdi$dserv)) # change data format to construct other variables
wdi$dservtoGDP <- wdi$dserv / wdi$gdp # calculate debt service to GDP
wdi$debt <- as.numeric(as.character(wdi$debt)) # change data format to construct other variables
wdi$debttoGDP <- wdi$debt / wdi$gdp # calculate debt to GDP
wdi$reserves <- as.numeric(as.character(wdi$reserves)) # change data format to construct other variables
wdi$reservestoGDP <- wdi$reserves / wdi$gdp # calculate reserves to GDP
wb <- merge(wb, wdi, by = c("ccode", "year"), all.x = TRUE) # merge WDI and conditionality data, preserving all observations
aid <- read.csv("OECDaid_10.19.csv", fileEncoding="UTF-8-BOM") # load OECD aid data (constant prices) on US ODA, downloaded in October 2019
aid <- aid[c(2,12,19)] # delete extra cols
colnames(aid) <- c("ccode","year","USaid") # rename columns
aid$ccode <- countrycode(aid$ccode, origin = "country.name", destination = "iso3c") # create country code for merging
wb <- merge(wb, aid, by = c("ccode", "year"), all.x = TRUE) # merge aid and conditionality data
wb$USaid[wb$USaid < 0] <- 0 # change negative values to 0 (data is already in millions from OECD)
wb$DACaid <- as.numeric(as.character(wb$DACaid)) / 1000000 # convert aid data from constant gross USD to millions of constant gross USD (was not in millions from WDI)
wb$DACaid[wb$DACaid < 0] <- 0 # change negative values to 0

load("Dyadicdata.RData") # load dyadic UN voting data downloaded from Bailey et al. (2017) in November 2018 V21 https://doi.org/10.7910/DVN/LEJUQZ
UNGA <- x # assign name to loaded data
UNGA_US <- UNGA[UNGA$ccode1 == 2,] # restrict dyads to US and each country
UNGA_US$ccode <- countrycode(UNGA_US$ccode2, origin = "cown", destination = "iso3c") # harmonize country codes
UNGA_US <- UNGA_US[-c(1:2,4:5,7:9,11:21)] # delete unneeded columns
wb <- merge(wb, UNGA_US, by = c("ccode", "year"), all.x = TRUE) # merge data

polity <- read.csv("p4v2017.csv", fileEncoding="UTF-8-BOM") # load polity2 v 2017 data downloaded from systemic peace in November 2018
# note that I deleted extra columns in Excel, leaving only the columns named "ccode", "country", "year", and "polity2"
# which appear as column numbers 2, 4, 5, and 11 in the original dataset. "Country" was renamed "ctry" to match my other codings
polity$ccode <- countrycode(polity$ctry, origin = "country.name", destination = "iso3c") # add country codes
polity <- polity[-c(2)] # delete ctry column
wb <- merge(wb, polity, by = c("ccode", "year"), all.x = TRUE) # merge data

china <- read.csv("CH_aid_11.18.csv", fileEncoding="UTF-8-BOM") # load China ODA-like flows data downloaded from AidData v 1.0 in November 2018
china <- china[china$flow_class == "ODA-like",] # restrict to ODA-like flows for comparison with US aid
china <- china[c(4,17,38)] # delete unneeded columns
china$usd_defl_2014 <- as.numeric(gsub(",","",china$usd_defl_2014)) # eliminate commas from figures
china[is.na(china)] <- 0 # replace NAs with zeroes (NAs are for projects with unknown amounts)
china <- aggregate(list(china$usd_defl_2014), by = list(china$recipient_iso3, china$year), sum) # sum ODA projects by country-year as my unit of analysis
colnames(china) <- c("ccode", "year", "CHaid") # change column names
china$CHaid <- china$CHaid / 1000000 # change from constant gross USD to millions of constant gross USD
wb <- merge(wb, china, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$CHaid[is.na(wb$CHaid)] <- 0 # replace NAs in broader data to 0 because these projects did not receive Chinese aid

nelda <- read_csv("NELDA_11.18.csv") # load election data downloaded from NELDA in November 2018
for (i in 1:3104){
  nelda$leg[i] <- ifelse(nelda$types[i] == "Legislative/Parliamentary", 1, 0)
  nelda$pres[i] <- ifelse(nelda$types[i] == "Executive", 1, 0)
} # code whether there was a legislative or executive election in a given country-year
nelda <- nelda[-c(2:4,6:125)] # delete extra columns
colnames(nelda)[1] <- "ccode" # rename column
nelda <- aggregate(list(nelda$leg, nelda$pres), by = list(nelda$ccode, nelda$year), FUN = sum) # aggregate elections by country-year
colnames(nelda) <- c("ccode", "year", "leg", "pres") # rename columns
nelda$leg[nelda$leg > 1] <- 1 # change values > 1 to 1
nelda$pres[nelda$pres > 1] <- 1 # change values > 1 to 1
nelda$ccode <- as.factor(nelda$ccode) # change format
wb <- merge(wb, nelda, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$elec <- wb$leg + wb$pres # create overall election measure by summing pres and leg
wb$elec[wb$elec > 1] <- 1 # change values > 1 to 1
wb$leg[is.na(wb$leg)] <- 0 # change NA to 0 because no election is currently set as blank
wb$pres[is.na(wb$pres)] <- 0 # change NA to 0 because no election is currently set as blank
wb$elec[is.na(wb$elec)] <- 0 # change NA to 0 because no election is currently set as blank

ucdp <- read.csv("ucdp_11.18.csv", fileEncoding="UTF-8-BOM") # load conflict/war data downloaded from UCDP in November 2018
ucdp <- ucdp[-c(4:18)] # delete extra columns
states <- gwstates # load Gleditch and Ward list of independent states (included in R)
states$ccode <- countrycode(states$iso3c, origin = "cowc", destination = "iso3c") # harmonize country codes
states <- states[-c(2:6)] # delete extra columns
colnames(states) <- c("gwno", "ccode") # rename columns
ucdp2 <- merge(ucdp, states, by = c("gwno")) # merge state data with conflict data
ucdp2 <- unique(ucdp2) # eliminate any duplicates
ucdp2 <- ucdp2[-c(1)] # delete extra column
colnames(ucdp2) <- c("year", "war", "ccode") # rename columns
wb <- merge(wb, ucdp2, by = c("ccode", "year"), all.x = TRUE) # merge data

unsc <- read.csv("unsc_updated.csv", fileEncoding="UTF-8-BOM") # load data on UNSC membership from Dreher, Sturm, and Vreeland (2009) http://www.axel-dreher.de/UNSCdata.xls
# downloaded in November 2018; I hand-coded the more recent years from UN online materials to bring the data up to date
unsc$ccode <- countrycode(unsc$ctry, origin = "country.name", destination = "iso3c") # harmonize country codes
unsc$unsc[unsc$unsc == "."] <- 0 # change empty observations to 0
unsc <- na.omit(unsc) # omit NAs (states that no longer exist)
wb <- merge(unique(wb), unique(unsc), by = c("ccode", "year"), all.x = TRUE) # merge data
wb$unsc[is.na(wb$unsc)] <- 0 # change NAs to 0 because missingness denotes not a member

IMF <- read.csv("IMF_Participation.csv", fileEncoding="UTF-8-BOM") # load IMF program participation data, which I hand-coded based on the 
# project data from Kentikelenis et al. (2016) downloaded in November 2018
IMF <- unique(IMF) # eliminate duplicates (they appear when a country had more than 1 project in a year)
wb <- merge(wb, IMF, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$IMF[is.na(wb$IMF)] <- 0 # changes NAs to 0 because NA denotes no project
wb <- wb[,-c(3:6,8:18,22:27,31,34,60)] # delete unneeded variables
colnames(wb)[3] <- "ctry" # rename ctry variable

hand <- read.csv("WB_Board_EU_Colony.csv", fileEncoding="UTF-8-BOM") # load hand-coded data on whether country is a former colony of current 
# EU president's country and whether country is on World Bank board; these were hand-coded from online 
# EU and WB resources
wb$colony <- hand$colony # append column to main data (I hand-coded exactly the same country-years that appear in main data)
wb$board <- hand$board # append column to main data (I hand-coded exactly the same country-years that appear in main data)
wb <- wb[-c(34)] # delete extra country variable

write.csv(wb, "wb2_nolag.csv") # write the dataset to csv

##->## Impute the non-lagged data
wb$supp <- as.character(wb$supp) # change data formatting to prepare for imputation
wb$ibrd_amt <- as.numeric(as.character(wb$ibrd_amt))
wb$dshorttoexports <- as.numeric(as.character(wb$dshorttoexports))
wb$openness <- as.numeric(as.character(wb$openness))
wb$curracctoGDP <- as.numeric(as.character(wb$curracctoGDP))
wb$FDItoGDP <- as.numeric(as.character(wb$FDItoGDP))
wb$inflation <- as.numeric(as.character(wb$inflation))
wb$ctry <- as.character(wb$ctry)
wb$unsc <- as.numeric(as.character(wb$unsc))
wb_imp <- mice(wb, method = "cart", seed = 500, m = 1)
wb_imp <- mice::complete(wb_imp)

write.csv(wb_imp, "wb2_nolag_imp.csv") # write the dataset to csv; currently toggled off to avoid overwriting

##->## Create data for the selection model robustness check
countries <- data.frame(
  expand.grid(country = c("Afghanistan", "Albania", "Algeria", "Angola", "Antigua and Barbuda", "Argentina", 
                          "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", 
                          "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", 
                          "Botswana", "Brazil", "Brunei Darussalam", "Bulgaria", "Burkina Faso", "Burundi",
                          "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", 
                          "Chile", "China", "Colombia", "Comoros", "Congo, Democratic Republic of", "Congo, Republic of",
                          "Costa Rica", "Cote d'Ivoire", "Croatia", "Cyprus", "Czech Republic", "Denmark", 
                          "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea",
                          "Eritrea", "Estonia", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia",
                          "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau",
                          "Guyana", "Haiti", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland",
                          "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Korea", "Kuwait",
                          "Kyrgyz Republic", "Lao People's Democratic Republic", "Latvia", "Lebanon", "Lesotho", 
                          "Liberia", "Libya", "Lithuania", "Luxembourg", "Macedonia", "Madagascar", "Malawi", "Malaysia", "Maldives",
                          "Mali", "Malta", "Marshall Islands", "Mauritania", "Mauritius", "Mexico", "Moldova",
                          "Mongolia", "Montenegro", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands",
                          "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea",
                          "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russian Federation", "Rwanda", "Samoa", 
                          "San Marino", "Sao Tome and Principe", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone",  "Singapore", "Slovak Republic",
                          "Slovenia", "Solomon Islands", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "St. Kitts and Nevis", "St. Lucia", 
                          "St. Vincent and the Grenadines", "Sudan", "Suriname", "Sweden", "Switzerland", "Syrian Arab Republic", "Tajikistan",
                          "Tanzania", "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", "Tuvalu", "Uganda",
                          "Ukraine", "United Arab Emirates", "United Kingdom", "United States", "Uruguay", "Uzbekistan", "Vanuatu", "Venezuela", "Vietnam", 
                          "Yemen", "Zambia", "Zimbabwe"), year = 2004:2018) # construct shell data with all member countries
)
countries$ccode <- countrycode(countries$country, origin = "country.name", destination = "iso3c") # add country codes
countries <- countries[,-c(1)] # delete country name
wb$part <- 1 # code participation as 1 for all country-years in project dataset
wbpart <- data.frame(cbind(wb$ccode,wb$year,wb$part)) # new dataframe with participating country-years
colnames(wbpart) <- c("ccode", "year", "part") # rename columns
wbpart <- unique(wbpart) # eliminate redundancies

wbpart2 <- merge(wbpart, countries, by = c("ccode","year"), all.y = TRUE) # merge with broader member data
wbpart2$part <- as.numeric(as.character(wbpart2$part)) # change data format
wbpart2$part[is.na(wbpart2$part)] <- 0 # change non-participating country-years to 0
wbpart2$year <- as.numeric(as.character(wbpart2$year)) # change data format
wbpart2$year[is.na(wbpart2$year)] <- 2004 # 2004 years are needed for lagged independent variables; add it back in because it shifts to NA during merge

wbpart2 <- merge(wbpart2, wdi, by = c("ccode", "year"), all.x = TRUE) # merge with covariates
wbpart2 <- merge(wbpart2, aid, by = c("ccode", "year"), all.x = TRUE) # merge with covariates
wbpart2$USaid[wbpart2$USaid < 0] <- 0 # change negative values to 0
wbpart2$DACaid <- as.numeric(as.character(wbpart2$DACaid)) / 1000000 # convert aid data from constant gross USD to millions of constant gross USD
wbpart2$DACaid[wbpart2$DACaid < 0] <- 0 # change negative values to 0
wbpart2 <- merge(wbpart2, nelda, by = c("ccode", "year"), all.x = TRUE)
wbpart2 <- merge(wbpart2, unsc, by = c("ccode", "year"), all.x = TRUE)
wbpart2 <- merge(wbpart2, UNGA_US, by = c("ccode", "year"), all.x = TRUE)
wbpart2 <- merge(wbpart2, ucdp2, by = c("ccode", "year"), all.x = TRUE)
wbpart2 <- merge(wbpart2, IMF, by = c("ccode", "year"), all.x = TRUE)
wbpart2 <- merge(wbpart2, china, by = c("ccode", "year"), all.x = TRUE)
wbpart2 <- merge(wbpart2, polity, by = c("ccode", "year"), all.x = TRUE)

wbpart2 <- wbpart2[,-c(23)] # delete extra country name columns
wbpart2$unsc <- as.numeric(as.character(wbpart2$unsc)) # change data format
wbpart2$unsc[is.na(wbpart2$unsc)] <- 0 # change values converted to NA during merge to 0
wbpart2$IMF[is.na(wbpart2$IMF)] <- 0
wbpart2$war[is.na(wbpart2$war)] <- 0
wbpart2$leg[is.na(wbpart2$leg)] <- 0
wbpart2$pres[is.na(wbpart2$pres)] <- 0
wbpart2$CHaid[is.na(wbpart2$CHaid)] <- 0
wbpart2 <- unique(wbpart2) # eliminate any redundant rows

temp <- data.frame(cbind(as.character(wbpart2$ccode), as.character(wbpart2$year), wbpart2$part)) # create temporary dataframe with DV only
colnames(temp) <- c("ccode", "year", "part") # rename columns
temp$ccode <- as.character(temp$ccode) # change data format
temp$year <- as.numeric(as.character(temp$year)) # change data format
temp$part <- as.numeric(as.character(temp$part)) # change data format
wbpart2 <- wbpart2[-3] # delete DV from original data
wbpart2$ccode <- as.character(wbpart2$ccode) # change data format
wbpart2$year <- as.numeric(as.character(wbpart2$year)) # change data format
wbpart2$year <- wbpart2$year + 1 # lag the independent variables
wbpart2 <- merge(wbpart2, temp, by = c("ccode", "year"))

##->## Impute the selection data
colnames(wbpart2)[3] <- "ctry"
wbpart2$ctry <- as.character(wbpart2$ctry)
wbpart2$dshorttoexports <- as.numeric(as.character(wbpart2$dshorttoexports))
wbpart2$openness <- as.numeric(as.character(wbpart2$openness))
wbpart2$curracctoGDP <- as.numeric(as.character(wbpart2$curracctoGDP))
wbpart2$FDItoGDP <- as.numeric(as.character(wbpart2$FDItoGDP))
wbpart2$inflation <- as.numeric(as.character(wbpart2$inflation))
wbpart2imp <- mice(wbpart2, method = "cart", seed = 500, m = 1) 
wbpart2imp <- mice::complete(wbpart2imp)
write.csv(wbpart2imp, "participation_imp.csv")

##->## Lagged Main Data (same steps / comments apply as in non-lagged data)
wb <- read.csv("Dev_Policy_Action_Database_10.18.csv", fileEncoding="UTF-8-BOM") # load raw data from World Bank's Development Policy Action database, 
# which was obtained via an access to information request in October 2018. Note that I changed the "prsc" column values where a dash was present to "N" to resolve an import error.
wb$ccode <- countrycode(wb$ctry, origin = "country.name", destination = "iso3c") # create country code column in iso3c format

stone <- read.csv("Policy_Categories.csv", fileEncoding="UTF-8-BOM") # load hand-coded data that counts the number of policy categories into which
# a project's conditions fall; they are the same categories used by Kentikelenis et al. (2016); I hand-code to harmonize
# the development policy action data with the Kentikelenis IMF data. This serves as an alternate DV to the count of conditions variable.
# It is based on the DV utilized by Stone (2008)
wb$numcats <- stone$numcats # data was hand-coded to be appended directly to original data

wdi <- read.csv("WDI_11.18.csv", fileEncoding="UTF-8-BOM") # load data from World Development Indicators, batch downloaded in November 2018 (column names were edited in excel but are intuitive)
wdi <- wdi[-c(2,15)] # delete time code and US aid col, which was in current prices, to be replaced with constant price data from OECD
wdi[wdi == ".."] <- NA # change dot notation used to mark NAs in WDI data to "NA"
colnames (wdi) <- c("year", "ctry", "ccode", "pop", "gdp", "dserv", "dshorttoexports", "openness",
                    "reserves", "curracctoGDP", "debt", "FDItoGDP", "inflation", "DACaid")
wdi$gdp <- as.numeric(as.character(wdi$gdp)) # change data format to construct other variables
wdi$pop <- as.numeric(as.character(wdi$pop))
wdi$gdppc <- wdi$gdp / wdi$pop # calculate per capita gdp
wdi$dserv <- as.numeric(as.character(wdi$dserv)) # change data format to construct other variables
wdi$dservtoGDP <- wdi$dserv / wdi$gdp # calculate debt service to GDP
wdi$debt <- as.numeric(as.character(wdi$debt)) # change data format to construct other variables
wdi$debttoGDP <- wdi$debt / wdi$gdp # calculate debt to GDP
wdi$reserves <- as.numeric(as.character(wdi$reserves)) # change data format to construct other variables
wdi$reservestoGDP <- wdi$reserves / wdi$gdp # calculate reserves to GDP
wdi$year <- as.numeric(as.character(wdi$year)) # change data format
wdi$year <- wdi$year + 1 # lag the wdi data by one year
wb <- merge(wb, wdi, by = c("ccode", "year"), all.x = TRUE) # merge WDI and conditionality data, preserving all observations
aid <- read.csv("OECDaid_10.19.csv", fileEncoding="UTF-8-BOM") # load OECD aid data (constant prices) on US ODA, downloaded in October 2019
aid <- aid[c(2,12,19)] # delete extra cols
colnames(aid) <- c("ccode","year","USaid") # rename columns
aid$ccode <- countrycode(aid$ccode, origin = "country.name", destination = "iso3c") # create country code for merging
wb <- merge(wb, aid, by = c("ccode", "year"), all.x = TRUE) # merge aid and conditionality data
wb$USaid[wb$USaid < 0] <- 0 # change negative values to 0 (data is already in millions from OECD)
wb$DACaid <- as.numeric(as.character(wb$DACaid)) / 1000000 # convert aid data from constant gross USD to millions of constant gross USD (was not in millions from WDI)
wb$DACaid[wb$DACaid < 0] <- 0 # change negative values to 0

load("Dyadicdata.RData") # load dyadic UN voting data downloaded from Bailey et al. (2017) in November 2018 V21 https://doi.org/10.7910/DVN/LEJUQZ
UNGA <- x # assign name to loaded data
UNGA_US <- UNGA[UNGA$ccode1 == 2,] # restrict dyads to US and each country
UNGA_US$ccode <- countrycode(UNGA_US$ccode2, origin = "cown", destination = "iso3c") # harmonize country codes
UNGA_US <- UNGA_US[-c(1:2,4:5,7:9,11:21)] # delete unneeded columns
UNGA_US$year <- UNGA_US$year + 1
wb <- merge(wb, UNGA_US, by = c("ccode", "year"), all.x = TRUE) # merge data

polity <- read.csv("p4v2017.csv", fileEncoding="UTF-8-BOM") # load polity2 v 2017 data downloaded from systemic peace in November 2018
# note that I deleted extra columns in Excel, leaving only the columns named "ccode", "country", "year", and "polity2"
# which appear as column numbers 2, 4, 5, and 11 in the original dataset. "Country" was renamed "ctry" to match my other codings
polity$ccode <- countrycode(polity$ctry, origin = "country.name", destination = "iso3c") # add country codes
polity <- polity[-c(2)] # delete ctry column
polity$year <- polity$year + 1
wb <- merge(wb, polity, by = c("ccode", "year"), all.x = TRUE) # merge data

china <- read.csv("CH_aid_11.18.csv", fileEncoding="UTF-8-BOM") # load China ODA-like flows data downloaded from AidData v 1.0 in November 2018
china <- china[china$flow_class == "ODA-like",] # restrict to ODA-like flows for comparison with US aid
china <- china[c(4,17,38)] # delete unneeded columns
china$usd_defl_2014 <- as.numeric(gsub(",","",china$usd_defl_2014)) # eliminate commas from figures
china[is.na(china)] <- 0 # replace NAs with zeroes (NAs are for projects with unknown amounts)
china <- aggregate(list(china$usd_defl_2014), by = list(china$recipient_iso3, china$year), sum) # sum ODA projects by country-year as my unit of analysis
colnames(china) <- c("ccode", "year", "CHaid") # change column names
china$CHaid <- china$CHaid / 1000000 # change from constant gross USD to millions of constant gross USD
china$year <- china$year + 1
wb <- merge(wb, china, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$CHaid[is.na(wb$CHaid)] <- 0 # replace NAs in broader data to 0 because these projects did not receive Chinese aid

nelda <- read_csv("NELDA_11.18.csv") # load election data downloaded from NELDA in November 2018
for (i in 1:3104){
  nelda$leg[i] <- ifelse(nelda$types[i] == "Legislative/Parliamentary", 1, 0)
  nelda$pres[i] <- ifelse(nelda$types[i] == "Executive", 1, 0)
} # code whether there was a legislative or executive election in a given country-year
nelda <- nelda[-c(2:4,6:125)] # delete extra columns
colnames(nelda)[1] <- "ccode" # rename column
nelda <- aggregate(list(nelda$leg, nelda$pres), by = list(nelda$ccode, nelda$year), FUN = sum) # aggregate elections by country-year
colnames(nelda) <- c("ccode", "year", "leg", "pres") # rename columns
nelda$leg[nelda$leg > 1] <- 1 # change values > 1 to 1
nelda$pres[nelda$pres > 1] <- 1 # change values > 1 to 1
nelda$year <- nelda$year + 1
nelda$ccode <- as.factor(nelda$ccode) # change data format
wb <- merge(wb, nelda, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$elec <- wb$leg + wb$pres # create overall election measure by summing pres and leg
wb$elec[wb$elec > 1] <- 1 # change values > 1 to 1
wb$leg[is.na(wb$leg)] <- 0 # change NA to 0 because no election is currently set as blank
wb$pres[is.na(wb$pres)] <- 0 # change NA to 0 because no election is currently set as blank
wb$elec[is.na(wb$elec)] <- 0 # change NA to 0 because no election is currently set as blank

ucdp <- read.csv("ucdp_11.18.csv", fileEncoding="UTF-8-BOM") # load conflict/war data downloaded from UCDP in November 2018
ucdp <- ucdp[-c(4:18)] # delete extra columns
states <- gwstates # load Gleditch and Ward list of independent states (included in R)
states$ccode <- countrycode(states$iso3c, origin = "cowc", destination = "iso3c") # harmonize country codes
states <- states[-c(2:6)] # delete extra columns
colnames(states) <- c("gwno", "ccode") # rename columns
ucdp2 <- merge(ucdp, states, by = c("gwno")) # merge state data with conflict data
ucdp2 <- unique(ucdp2) # eliminate any duplicates
ucdp2 <- ucdp2[-c(1)] # delete extra column
colnames(ucdp2) <- c("year", "war", "ccode") # rename columns
ucdp2$year <- ucdp2$year + 1
wb <- merge(wb, ucdp2, by = c("ccode", "year"), all.x = TRUE) # merge data

unsc <- read.csv("unsc_updated.csv", fileEncoding="UTF-8-BOM") # load data on UNSC membership from Dreher, Sturm, and Vreeland (2009) http://www.axel-dreher.de/UNSCdata.xls
# downloaded in November 2018; I hand-coded the more recent years from UN online materials to bring the data up to date
unsc$ccode <- countrycode(unsc$ctry, origin = "country.name", destination = "iso3c") # harmonize country codes
unsc$unsc[unsc$unsc == "."] <- 0 # change empty observations to 0
unsc <- na.omit(unsc) # omit NAs (states that no longer exist)
unsc$year <- unsc$year + 1
wb <- merge(unique(wb), unique(unsc), by = c("ccode", "year"), all.x = TRUE) # merge data
wb$unsc[is.na(wb$unsc)] <- 0 # change NAs to 0 because missingness denotes not a member

IMF <- read.csv("IMF_Participation.csv", fileEncoding="UTF-8-BOM") # load IMF program participation data, which I hand-coded based on the 
# project data from Kentikelenis et al. (2016) downloaded in November 2018
IMF <- unique(IMF) # eliminate duplicates (they appear when a country had more than 1 project in a year)
IMF$year <- IMF$year + 1
wb <- merge(wb, IMF, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$IMF[is.na(wb$IMF)] <- 0 # changes NAs to 0 because NA denotes no project
wb <- wb[,-c(3:6,8:18,22:27,31,34,60)] # delete unneeded variables
colnames(wb)[3] <- "ctry" # rename ctry variable

hand <- read.csv("WB_Board_EU_Colony_Lagged.csv", fileEncoding="UTF-8-BOM") # load hand-coded data on whether country is a former colony of current 
# EU president's country and whether country is on World Bank board; these were hand-coded from online 
# EU and WB resources; I hand-lagged the board member data in Excel because it required looking for additional years of data
wb$colony <- hand$colony # append column to main data (I hand-coded exactly the same country-years that appear in main data)
wb$board <- hand$board # append column to main data (I hand-coded exactly the same country-years that appear in main data)
wb <- wb[-c(34)] # delete extra country columns

write.csv(wb, "wb2_lags.csv") # write the dataset to csv; currently toggled off to avoid overwriting

##->## Impute the lagged data
wb$supp <- as.character(wb$supp) # change data formatting to prepare for imputation
wb$ibrd_amt <- as.numeric(as.character(wb$ibrd_amt))
wb$dshorttoexports <- as.numeric(as.character(wb$dshorttoexports))
wb$openness <- as.numeric(as.character(wb$openness))
wb$curracctoGDP <- as.numeric(as.character(wb$curracctoGDP))
wb$FDItoGDP <- as.numeric(as.character(wb$FDItoGDP))
wb$inflation <- as.numeric(as.character(wb$inflation))
wb$ctry <- as.character(wb$ctry)
wb$unsc <- as.numeric(as.character(wb$unsc))
wb_lag_imp <- mice(wb, method = "cart", seed = 500, m = 1)
wb_lag_imp <- mice::complete(wb_lag_imp)
write.csv(wb_lag_imp, "wb2_lags_imp.csv") # write the dataset to csv; currently toggled off to avoid overwriting

##->## Impute the lagged data with alternate method
wb_lag_impk <- kNN(wb, k=10)
wb_lag_impk <- wb_lag_impk[-c(40:78)] # delete extra columns indicating which rows were imputed
write.csv(wb_lag_impk, "wb2_lags_impk.csv") # write dataset to csv; currently toggled off to avoid overwriting

##->## Lagged data with no lag for IMF and UNSC (for interaction robustness check, which tests whether 
# countries apply their leverage strategically across the IMF and World Bank)
wb <- read.csv("Dev_Policy_Action_Database_10.18.csv", fileEncoding="UTF-8-BOM") # load raw data from World Bank's Development Policy Action database, 
# which was obtained via an access to information request in October 2018. Note that I changed the "prsc" column values where a dash was present to "N" to resolve an import error.
wb$ccode <- countrycode(wb$ctry, origin = "country.name", destination = "iso3c") # create country code column in iso3c format

stone <- read.csv("Policy_Categories.csv", fileEncoding="UTF-8-BOM") # load hand-coded data that counts the number of policy categories into which
# a project's conditions fall; they are the same categories used by Kentikelenis et al. (2016); I hand-code to harmonize
# the development policy action data with the Kentikelenis IMF data. This serves as an alternate DV to the count of conditions variable.
# It is based on the DV utilized by Stone (2008)
wb$numcats <- stone$numcats # data was hand-coded to be appended directly to original data

wdi <- read.csv("WDI_11.18.csv", fileEncoding="UTF-8-BOM") # load data from World Development Indicators, batch downloaded in November 2018 (column names were edited in excel but are intuitive)
wdi <- wdi[-c(2,15)] # delete time code and US aid col, which was in current prices, to be replaced with constant price data from OECD
wdi[wdi == ".."] <- NA # change dot notation used to mark NAs in WDI data to "NA"
colnames (wdi) <- c("year", "ctry", "ccode", "pop", "gdp", "dserv", "dshorttoexports", "openness",
                    "reserves", "curracctoGDP", "debt", "FDItoGDP", "inflation", "DACaid")
wdi$gdp <- as.numeric(as.character(wdi$gdp)) # change data format to construct other variables
wdi$pop <- as.numeric(as.character(wdi$pop))
wdi$gdppc <- wdi$gdp / wdi$pop # calculate per capita gdp
wdi$dserv <- as.numeric(as.character(wdi$dserv)) # change data format to construct other variables
wdi$dservtoGDP <- wdi$dserv / wdi$gdp # calculate debt service to GDP
wdi$debt <- as.numeric(as.character(wdi$debt)) # change data format to construct other variables
wdi$debttoGDP <- wdi$debt / wdi$gdp # calculate debt to GDP
wdi$reserves <- as.numeric(as.character(wdi$reserves)) # change data format to construct other variables
wdi$reservestoGDP <- wdi$reserves / wdi$gdp # calculate reserves to GDP
wdi$year <- as.numeric(as.character(wdi$year)) # change data format
wdi$year <- wdi$year + 1 # lag the wdi data by one year
wb <- merge(wb, wdi, by = c("ccode", "year"), all.x = TRUE) # merge WDI and conditionality data, preserving all observations
aid <- read.csv("OECDaid_10.19.csv", fileEncoding="UTF-8-BOM") # load OECD aid data (constant prices) on US ODA, downloaded in October 2019
aid <- aid[c(2,12,19)] # delete extra cols
colnames(aid) <- c("ccode","year","USaid") # rename columns
aid$ccode <- countrycode(aid$ccode, origin = "country.name", destination = "iso3c") # create country code for merging
wb <- merge(wb, aid, by = c("ccode", "year"), all.x = TRUE) # merge aid and conditionality data
wb$USaid[wb$USaid < 0] <- 0 # change negative values to 0 (data is already in millions from OECD)
wb$DACaid <- as.numeric(as.character(wb$DACaid)) / 1000000 # convert aid data from constant gross USD to millions of constant gross USD (was not in millions from WDI)
wb$DACaid[wb$DACaid < 0] <- 0 # change negative values to 0

load("Dyadicdata.RData") # load dyadic UN voting data downloaded from Bailey et al. (2017) in November 2018 V21 https://doi.org/10.7910/DVN/LEJUQZ
UNGA <- x # assign name to loaded data
UNGA_US <- UNGA[UNGA$ccode1 == 2,] # restrict dyads to US and each country
UNGA_US$ccode <- countrycode(UNGA_US$ccode2, origin = "cown", destination = "iso3c") # harmonize country codes
UNGA_US <- UNGA_US[-c(1:2,4:5,7:9,11:21)] # delete unneeded columns
UNGA_US$year <- UNGA_US$year + 1
wb <- merge(wb, UNGA_US, by = c("ccode", "year"), all.x = TRUE) # merge data

polity <- read.csv("p4v2017.csv", fileEncoding="UTF-8-BOM") # load polity2 v 2017 data downloaded from systemic peace in November 2018
# note that I deleted extra columns in Excel, leaving only the columns named "ccode", "country", "year", and "polity2"
# which appear as column numbers 2, 4, 5, and 11 in the original dataset. "Country" was renamed "ctry" to match my other codings
polity$ccode <- countrycode(polity$ctry, origin = "country.name", destination = "iso3c") # add country codes
polity <- polity[-c(2)] # delete ctry column
polity$year <- polity$year + 1
wb <- merge(wb, polity, by = c("ccode", "year"), all.x = TRUE) # merge data

china <- read.csv("CH_aid_11.18.csv", fileEncoding="UTF-8-BOM") # load China ODA-like flows data downloaded from AidData v 1.0 in November 2018
china <- china[china$flow_class == "ODA-like",] # restrict to ODA-like flows for comparison with US aid
china <- china[c(4,17,38)] # delete unneeded columns
china$usd_defl_2014 <- as.numeric(gsub(",","",china$usd_defl_2014)) # eliminate commas from figures
china[is.na(china)] <- 0 # replace NAs with zeroes (NAs are for projects with unknown amounts)
china <- aggregate(list(china$usd_defl_2014), by = list(china$recipient_iso3, china$year), sum) # sum ODA projects by country-year as my unit of analysis
colnames(china) <- c("ccode", "year", "CHaid") # change column names
china$CHaid <- china$CHaid / 1000000 # change from constant gross USD to millions of constant gross USD
china$year <- china$year + 1
wb <- merge(wb, china, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$CHaid[is.na(wb$CHaid)] <- 0 # replace NAs in broader data to 0 because these projects did not receive Chinese aid

nelda <- read_csv("NELDA_11.18.csv") # load election data downloaded from NELDA in November 2018
for (i in 1:3104){
  nelda$leg[i] <- ifelse(nelda$types[i] == "Legislative/Parliamentary", 1, 0)
  nelda$pres[i] <- ifelse(nelda$types[i] == "Executive", 1, 0)
} # code whether there was a legislative or executive election in a given country-year
nelda <- nelda[-c(2:4,6:125)] # delete extra columns
colnames(nelda)[1] <- "ccode" # rename column
nelda <- aggregate(list(nelda$leg, nelda$pres), by = list(nelda$ccode, nelda$year), FUN = sum) # aggregate elections by country-year
colnames(nelda) <- c("ccode", "year", "leg", "pres") # rename columns
nelda$leg[nelda$leg > 1] <- 1 # change values > 1 to 1
nelda$pres[nelda$pres > 1] <- 1 # change values > 1 to 1
nelda$year <- nelda$year + 1
nelda$ccode <- as.factor(nelda$ccode) # change data format
wb <- merge(wb, nelda, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$elec <- wb$leg + wb$pres # create overall election measure by summing pres and leg
wb$elec[wb$elec > 1] <- 1 # change values > 1 to 1
wb$leg[is.na(wb$leg)] <- 0 # change NA to 0 because no election is currently set as blank
wb$pres[is.na(wb$pres)] <- 0 # change NA to 0 because no election is currently set as blank
wb$elec[is.na(wb$elec)] <- 0 # change NA to 0 because no election is currently set as blank

ucdp <- read.csv("ucdp_11.18.csv", fileEncoding="UTF-8-BOM") # load conflict/war data downloaded from UCDP in November 2018
ucdp <- ucdp[-c(4:18)] # delete extra columns
states <- gwstates # load Gleditch and Ward list of independent states (included in R)
states$ccode <- countrycode(states$iso3c, origin = "cowc", destination = "iso3c") # harmonize country codes
states <- states[-c(2:6)] # delete extra columns
colnames(states) <- c("gwno", "ccode") # rename columns
ucdp2 <- merge(ucdp, states, by = c("gwno")) # merge state data with conflict data
ucdp2 <- unique(ucdp2) # eliminate any duplicates
ucdp2 <- ucdp2[-c(1)] # delete extra column
colnames(ucdp2) <- c("year", "war", "ccode") # rename columns
ucdp2$year <- ucdp2$year + 1
wb <- merge(wb, ucdp2, by = c("ccode", "year"), all.x = TRUE) # merge data

unsc <- read.csv("unsc_updated.csv", fileEncoding="UTF-8-BOM") # load data on UNSC membership from Dreher, Sturm, and Vreeland (2009) http://www.axel-dreher.de/UNSCdata.xls
# downloaded in November 2018; I hand-coded the more recent years from UN online materials to bring the data up to date
unsc$ccode <- countrycode(unsc$ctry, origin = "country.name", destination = "iso3c") # harmonize country codes
unsc$unsc[unsc$unsc == "."] <- 0 # change empty observations to 0
unsc <- na.omit(unsc) # omit NAs (states that no longer exist)
wb <- merge(unique(wb), unique(unsc), by = c("ccode", "year"), all.x = TRUE) # merge data
wb$unsc[is.na(wb$unsc)] <- 0 # change NAs to 0 because missingness denotes not a member

IMF <- read.csv("IMF_Participation.csv", fileEncoding="UTF-8-BOM") # load IMF program participation data, which I hand-coded based on the 
# project data from Kentikelenis et al. (2016) downloaded in November 2018
IMF <- unique(IMF) # eliminate duplicates (they appear when a country had more than 1 project in a year)
wb <- merge(wb, IMF, by = c("ccode", "year"), all.x = TRUE) # merge data
wb$IMF[is.na(wb$IMF)] <- 0 # changes NAs to 0 because NA denotes no project
wb <- wb[,-c(3:6,8:18,22:27,31,34,60)] # delete unneeded variables
colnames(wb)[3] <- "ctry" # rename ctry variable

hand <- read.csv("WB_Board_EU_Colony_Lagged.csv", fileEncoding="UTF-8-BOM") # load hand-coded data on whether country is a former colony of current 
# EU president's country and whether country is on World Bank board; these were hand-coded from online 
# EU and WB resources; I hand-lagged the board member data in Excel because it required looking for additional years of data
wb$colony <- hand$colony # append column to main data (I hand-coded exactly the same country-years that appear in main data)
wb$board <- hand$board # append column to main data (I hand-coded exactly the same country-years that appear in main data)
wb <- wb[-c(34)] # delete extra country columns

##->## Impute the lagged data
wb$supp <- as.character(wb$supp) # change data formatting to prepare for imputation
wb$ibrd_amt <- as.numeric(as.character(wb$ibrd_amt))
wb$dshorttoexports <- as.numeric(as.character(wb$dshorttoexports))
wb$openness <- as.numeric(as.character(wb$openness))
wb$curracctoGDP <- as.numeric(as.character(wb$curracctoGDP))
wb$FDItoGDP <- as.numeric(as.character(wb$FDItoGDP))
wb$inflation <- as.numeric(as.character(wb$inflation))
wb$ctry <- as.character(wb$ctry)
wb$unsc <- as.numeric(as.character(wb$unsc))
wb_lag_impr <- mice(wb, method = "cart", seed = 500, m = 1)
wb_lag_impr <- mice::complete(wb_lag_impr)

write.csv(wb_lag_impr, "wb2_lags_impr.csv") # write the dataset to csv; currently toggled off to avoid overwriting

######### Load Data #######
## Note that before loading these datasets (as well as the text datasets used for text analysis in the
## following sections that are written directly to file from this script) I manually added the column 
## name "rowID" in Excel to the first column for each created dataset. These datasets when written to
## file from R include a rowID column without a name. The final datasets in the replication package
## already have this column name added (it is necessary to prevent merge errors). If you choose to 
## recreate the datasets using the above code, you will need to add the column name yourself.
wb <- read.csv("wb2_nolag.csv", fileEncoding="UTF-8-BOM") # non-lagged data not imputed
wb_imp <- read.csv("wb2_nolag_imp.csv", fileEncoding="UTF-8-BOM")
wb2_lag <- read.csv("wb2_lags.csv", fileEncoding="UTF-8-BOM") # lagged data not imputed
wb2_lag_impm <- read.csv("wb2_lags_imp.csv", fileEncoding="UTF-8-BOM") # lagged data imputed with multiple imputation
wb2_lag_impmr <- read.csv("wb2_lags_impr.csv", fileEncoding="UTF-8-BOM") # lagged data imputed with multiple imputation for interaction
wb2_lag_impk <- read.csv("wb2_lags_impk.csv", fileEncoding="UTF-8-BOM") # lagged data imputed with k-NN imputation
wbpart2imp <- read.csv("participation_imp.csv", fileEncoding="UTF-8-BOM") # data for selection robustness check

######### Descriptive Plots #############
# Total Disbursements Over Time
tot <- aggregate(list(wb2_lag_impm$proj_cost), by = list(wb2_lag_impm$year), FUN = sum)
colnames(tot) <- c("year", "proj_cost")
barplot(tot$proj_cost, names.arg = tot$year, xlab = "Year", ylab = "Total Disbursements") # exported as disb_year.pdf (Figure 1)

# Average num of conditions by country map
wb2_lag_impm$ctry <- countrycode(wb2_lag_impm$ccode, origin = "iso3c", destination = "country.name")
map <- as.data.frame(cbind(as.character(wb2_lag_impm$ccode), as.character(wb2_lag_impm$ctry), wb2_lag_impm$year, wb2_lag_impm$count_pa))
colnames(map) <- c("ccode", "ctry","year", "count_pa")
map$count_pa <- as.numeric(as.character(map$count_pa))
map <- aggregate(list(map$count_pa), by = list(map$ccode, map$ctry), mean)
colnames(map) <- c("ccode", "ctry", "count_pa")

maps <- joinCountryData2Map(map, joinCode = "ISO3", nameJoinColumn = "ccode", nameCountryColumn = "ctry")
maps <- subset(maps, continent != "Antarctica")
mapss <- mapCountryData(mapToPlot = maps, nameColumnToPlot = "count_pa", mapRegion = "world", 
                        catMethod = "pretty", colourPalette = brewer.pal(11, name = "Greys"),
                        addLegend = FALSE, mapTitle = "")
do.call( addMapLegend, c(mapss, legendWidth=0.5, legendMar = 5)) # exported as wb_cond_map_per.pdf (Figure 2)

# Average Conditions Over Time
avg <- aggregate(list(wb2_lag_impm$count_pa), by = list(wb2_lag_impm$year), FUN = mean)
colnames(avg) <- c("year", "count")
barplot(avg$count, names.arg = avg$year, xlab = "Year", ylab = "Average Number of Conditions") # exported as mean_cond_year.pdf (Figure 3)

# Histogram of number of prior actions by project for appendix
hist(sort(wb2_lag_impm$count_pa), xlab = "Number of Conditions", density = 30, col = "blue",
     breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120), ylab = "Frequency", main = "") # exported as cond_disb_pa.pdf (Figure A1)

######### Scale and Process #######
wb2_lag <- wb2_lag[wb2_lag$supp == "N",] # restrict data to not include supplemental loans because
# they are substantively different than typical DPF programs (only a handful of observations)
wb_imp <- wb_imp[wb_imp$supp == "N",]
wb2_lag_impk <- wb2_lag_impk[wb2_lag_impk$supp == "N",]
wb2_lag_impm <- wb2_lag_impm[wb2_lag_impm$supp == "N",]
wb2_lag_impmr <- wb2_lag_impmr[wb2_lag_impmr$supp == "N",]

wb2_lag$USaid <- scale(log1p(wb2_lag$USaid)) # log aid values then scale all rest to aid convergence
wb2_lag$CHaid <- scale(log1p(wb2_lag$CHaid))
wb2_lag$absidealdiff <- scale(wb2_lag$absidealdiff)
wb2_lag$absidealimportantdiff <- scale(wb2_lag$absidealimportantdiff)
wb2_lag$gdppc <- scale(wb2_lag$gdppc)
wb2_lag$dservtoGDP <- scale(wb2_lag$dservtoGDP)
wb2_lag$dshorttoexports <- scale(wb2_lag$dshorttoexports)
wb2_lag$inflation <- scale(wb2_lag$inflation)
wb2_lag$debttoGDP <- scale(wb2_lag$debttoGDP)
wb2_lag$FDItoGDP <- scale(wb2_lag$FDItoGDP)
wb2_lag$pop <- scale(wb2_lag$pop)
wb2_lag$polity2 <- scale(wb2_lag$polity2)
wb2_lag$openness <- scale(wb2_lag$openness)
wb2_lag$ida_amt <- scale(wb2_lag$ida_amt)
wb2_lag$proj_cost <- scale(wb2_lag$proj_cost)
wb2_lag$crisis <- 0 # create the post-2012 variable to adjust for time properties
wb2_lag$crisis[wb2_lag$year > 2012] <- 1

wb2_lag_impk$USaid <- scale(log1p(wb2_lag_impk$USaid)) # log and aid values then scale all rest to aid convergence
wb2_lag_impk$CHaid <- scale(log1p(wb2_lag_impk$CHaid))
wb2_lag_impk$absidealdiff <- scale(wb2_lag_impk$absidealdiff)
wb2_lag_impk$absidealimportantdiff <- scale(wb2_lag_impk$absidealimportantdiff)
wb2_lag_impk$gdppc <- scale(wb2_lag_impk$gdppc)
wb2_lag_impk$dservtoGDP <- scale(wb2_lag_impk$dservtoGDP)
wb2_lag_impk$dshorttoexports <- scale(wb2_lag_impk$dshorttoexports)
wb2_lag_impk$inflation <- scale(wb2_lag_impk$inflation)
wb2_lag_impk$debttoGDP <- scale(wb2_lag_impk$debttoGDP)
wb2_lag_impk$FDItoGDP <- scale(wb2_lag_impk$FDItoGDP)
wb2_lag_impk$pop <- scale(wb2_lag_impk$pop)
wb2_lag_impk$polity2 <- scale(wb2_lag_impk$polity2)
wb2_lag_impk$openness <- scale(wb2_lag_impk$openness)
wb2_lag_impk$ida_amt <- scale(wb2_lag_impk$ida_amt)
wb2_lag_impk$proj_cost <- scale(wb2_lag_impk$proj_cost)
wb2_lag_impk$crisis <- 0 # create the post-2012 variable to adjust for time properties
wb2_lag_impk$crisis[wb2_lag_impk$year > 2012] <- 1

wb2_lag_impm$USaid <- scale(log1p(wb2_lag_impm$USaid)) # log aid values then scale all rest to aid convergence
wb2_lag_impm$CHaid <- scale(log1p(wb2_lag_impm$CHaid))
wb2_lag_impm$absidealdiff <- scale(wb2_lag_impm$absidealdiff)
wb2_lag_impm$absidealimportantdiff <- scale(wb2_lag_impm$absidealimportantdiff)
wb2_lag_impm$gdppc <- scale(wb2_lag_impm$gdppc)
wb2_lag_impm$dservtoGDP <- scale(wb2_lag_impm$dservtoGDP)
wb2_lag_impm$dshorttoexports <- scale(wb2_lag_impm$dshorttoexports)
wb2_lag_impm$inflation <- scale(wb2_lag_impm$inflation)
wb2_lag_impm$debttoGDP <- scale(wb2_lag_impm$debttoGDP)
wb2_lag_impm$FDItoGDP <- scale(wb2_lag_impm$FDItoGDP)
wb2_lag_impm$pop <- scale(wb2_lag_impm$pop)
wb2_lag_impm$polity2 <- scale(wb2_lag_impm$polity2)
wb2_lag_impm$openness <- scale(wb2_lag_impm$openness)
wb2_lag_impm$ida_amt <- scale(wb2_lag_impm$ida_amt)
wb2_lag_impm$proj_cost <- scale(wb2_lag_impm$proj_cost)
wb2_lag_impm$crisis <- 0 # create the post-2012 variable to adjust for time properties
wb2_lag_impm$crisis[wb2_lag_impm$year > 2012] <- 1

wb2_lag_impmr$USaid <- scale(log1p(wb2_lag_impmr$USaid)) # log aid values then scale all rest to aid convergence
wb2_lag_impmr$CHaid <- scale(log1p(wb2_lag_impmr$CHaid))
wb2_lag_impmr$absidealdiff <- scale(wb2_lag_impmr$absidealdiff)
wb2_lag_impmr$absidealimportantdiff <- scale(wb2_lag_impmr$absidealimportantdiff)
wb2_lag_impmr$gdppc <- scale(wb2_lag_impmr$gdppc)
wb2_lag_impmr$dservtoGDP <- scale(wb2_lag_impmr$dservtoGDP)
wb2_lag_impmr$dshorttoexports <- scale(wb2_lag_impmr$dshorttoexports)
wb2_lag_impmr$inflation <- scale(wb2_lag_impmr$inflation)
wb2_lag_impmr$debttoGDP <- scale(wb2_lag_impmr$debttoGDP)
wb2_lag_impmr$FDItoGDP <- scale(wb2_lag_impmr$FDItoGDP)
wb2_lag_impmr$pop <- scale(wb2_lag_impmr$pop)
wb2_lag_impmr$polity2 <- scale(wb2_lag_impmr$polity2)
wb2_lag_impmr$openness <- scale(wb2_lag_impmr$openness)
wb2_lag_impmr$ida_amt <- scale(wb2_lag_impmr$ida_amt)
wb2_lag_impmr$proj_cost <- scale(wb2_lag_impmr$proj_cost)
wb2_lag_impmr$crisis <- 0 # create the post-2012 variable to adjust for time properties
wb2_lag_impmr$crisis[wb2_lag_impmr$year > 2012] <- 1

wb_imp$USaid <- scale(log1p(wb_imp$USaid)) # log aid values then scale all rest to aid convergence
wb_imp$CHaid <- scale(log1p(wb_imp$CHaid))
wb_imp$absidealdiff <- scale(wb_imp$absidealdiff)
wb_imp$absidealimportantdiff <- scale(wb_imp$absidealimportantdiff)
wb_imp$gdppc <- scale(wb_imp$gdppc)
wb_imp$dservtoGDP <- scale(wb_imp$dservtoGDP)
wb_imp$dshorttoexports <- scale(wb_imp$dshorttoexports)
wb_imp$inflation <- scale(wb_imp$inflation)
wb_imp$debttoGDP <- scale(wb_imp$debttoGDP)
wb_imp$FDItoGDP <- scale(wb_imp$FDItoGDP)
wb_imp$pop <- scale(wb_imp$pop)
wb_imp$polity2 <- scale(wb_imp$polity2)
wb_imp$openness <- scale(wb_imp$openness)
wb_imp$ida_amt <- scale(wb_imp$ida_amt)
wb_imp$proj_cost <- scale(wb_imp$proj_cost)
wb_imp$crisis <- 0 # create the post-2012 variable to adjust for time properties
wb_imp$crisis[wb_imp$year > 2012] <- 1

wbpart2imp$USaid <- scale(log1p(wbpart2imp$USaid)) # log aid values then scale all rest to aid convergence
wbpart2imp$CHaid <- scale(log1p(wbpart2imp$CHaid))
wbpart2imp$DACaid <- scale(log1p(wbpart2imp$DACaid))
wbpart2imp$absidealdiff <- scale(wbpart2imp$absidealdiff)
wbpart2imp$absidealimportantdiff <- scale(wbpart2imp$absidealimportantdiff)
wbpart2imp$pop <- scale(wbpart2imp$pop)
wbpart2imp$polity2 <- scale(wbpart2imp$polity2)
wbpart2imp$reservestoGDP <- scale(wbpart2imp$reservestoGDP)
wbpart2imp$gdppc <- scale(wbpart2imp$gdppc)
wbpart2imp$curracctoGDP <- scale(wbpart2imp$curracctoGDP)
wbpart2imp$debttoGDP <- scale(wbpart2imp$debttoGDP)
wbpart2imp$FDItoGDP <- scale(wbpart2imp$FDItoGDP)
wbpart2imp$inflation <- scale(wbpart2imp$inflation)
wbpart2imp$openness <- scale(wbpart2imp$openness)
wbpart2imp$elec <- 0
wbpart2imp$elec[wbpart2imp$pres == 1 | wbpart2imp$leg == 1] <- 1

######### Summary Statistics ##############
covs_desc <- c("Number of prior actions", "Number of categories", "Short-term debt / exports", "Openness", 
               "FDI / GDP", "Inflation", "GDPPC", "Debt service / GDP", "Debt / GDP", "U.S. aid", 
               "UN voting (ideal pt dist from U.S.)", "Polity2", "Chinese aid", "Election year", "War",
               "UNSC member", "IMF program", "EU president colony", "World Bank board member") # covariate labels
desc <- read.csv("wb2_lags_imp.csv", fileEncoding="UTF-8-BOM") # re-load main imputed data
desc <- desc[desc$supp == "N",] # subset without supplemental projects
desc <- desc[,-c(1:5, 7:11, 13:14, 15, 18:20, 23, 27, 29:30, 34)] # subset to exclude non-numeric and extra variables
stargazer(desc, omit.summary.stat = c("p25", "p75"), digits = 2, style = "ajps",
          covariate.labels = covs_desc) # print table

######### Regression Models #########
# Bivariate  (country fixed effects)

ctryfe <- list(c("Country fixed effects", "Yes", "Yes"))
ctryre <- list(c("Country random effects", "Yes", "Yes"))
yearfe <- list(c("Year fixed effects", "Yes", "Yes"))

regbiv <- glm.nb(count_pa ~ absidealimportantdiff + factor(ccode), data = wb2_lag, 
                 control=glm.control(maxit=20)) # bivariate with count DV
(sebiv <- coeftest(regbiv, vcov = vcovCL(regbiv, cluster = wb2_lag$ccode)))

regbiv2 <- glm.nb(numcats ~ absidealimportantdiff + factor(ccode), data = wb2_lag, 
                  control=glm.control(maxit=20)) # bivariate with stone DV
(sebiv2 <- coeftest(regbiv2, vcov = vcovCL(regbiv2, cluster = wb2_lag$ccode)))

stargazer(regbiv, regbiv2, se = list(sebiv[,2], sebiv2[,2]), 
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = c("UN voting (ideal point dist from U.S.)"),
          style = "ajps", omit = "ccode", add.lines = ctryfe,
          omit.stat = c("ll", "aic", "theta")) # print table (Table 1)

# Main models (country fixed effects, multiple imputation, stone and count DVs)

covs_full <- c("UN voting (ideal pt dist from U.S.)", "World Bank board member", "EU president colony", 
               "UNSC member", "U.S. aid", "Chinese aid", "GDPPC", "Debt service / GDP", 
               "Short-term debt / exports", "Inflation", "Debt / GDP", "FDI / GDP", "Polity2", 
               "Openness", "War", "Election year", "IMF program", "Post-2012") # covariate labels

reg1 <- glm.nb(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                 dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                 openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impm, 
               control=glm.control(maxit=20)) # full controls with count DV
(se1 <- coeftest(reg1, vcov = vcovHC(reg1, type = "HC0", cluster = wb2_lag_impm$ccode)))

reg2 <- glm.nb(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                 dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                 openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impm, 
               control=glm.control(maxit=20)) # full controls with stone DV
(se2 <- coeftest(reg2, vcov = vcovHC(reg2, type = "HC0", cluster = wb2_lag_impm$ccode)))
stargazer(reg1, reg2, se = list(se1[,2], se2[,2]), 
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode", add.lines = ctryfe,
          omit.stat = c("ll", "aic", "theta")) # print table (Table 2)

# Year fixed effects robustness check (with multiple imputation, ctry fixed effects)

wb2_lag_impm2 <- dummy_cols(wb2_lag_impm, select_columns = "year") # create dummies for each year so
# that 2013 can be excluded (because crisis remains in the model)

om <- c("ccode") # list of covariates to be excluded from table

covs_full_y <- c("UN voting (ideal pt dist from U.S.)", "World Bank board member", "EU president colony", 
               "UNSC member", "U.S. aid", "Chinese aid", "GDPPC", "Debt service / GDP", 
               "Short-term debt / exports", "Inflation", "Debt / GDP", "FDI / GDP", "Polity2", 
               "Openness", "War", "Election year", "IMF program", "Post-2012", "2006", "2007", "2008", 
               "2009","2010", "2011", "2012", "2014", "2015", "2016", "2017", "2018") # covariate labels

reg3a <- glm.nb(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                  dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                  openness + war + elec + IMF + crisis + factor(ccode) + year_2006 +
                  year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 +
                  year_2014 + year_2015 + year_2016 + year_2017 + year_2018, data = wb2_lag_impm2, 
                control=glm.control(maxit=20)) # year fixed effects added to main model
(se3a <- coeftest(reg3a, vcov = vcovHC(reg3a, type = "HC0", cluster = wb2_lag_impm$ccode)))

reg3b <- glm.nb(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                  dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                  openness + war + elec + IMF + crisis + factor(ccode) + + year_2006 +
                  year_2007 + year_2008 + year_2009 + year_2010 + year_2011 + year_2012 +
                  year_2014 + year_2015 + year_2016 + year_2017 + year_2018, data = wb2_lag_impm2, 
                control=glm.control(maxit=20)) # year fixed effects added to main model
(se3b <- coeftest(reg3b, vcov = vcovHC(reg3b, type = "HC0", cluster = wb2_lag_impm$ccode)))

stargazer(reg3a, reg3b, se = list(se3a[,2], se3b[,2]),
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full_y,
          style = "ajps", omit = om, add.lines = c(ctryfe, yearfe),
          omit.stat = c("ll", "aic", "theta")) # print table (Table A9)

linearHypothesis(reg3a, c("year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + 
                          year_2011 + year_2012 + year_2014 + year_2015 + year_2016 + 
                          year_2017 + year_2018"), test = "F") # tests for joint significance of each of
# the year dummies; F tells us we cannot reject the null, so we can reject year FEs
linearHypothesis(reg3b, c("year_2006 + year_2007 + year_2008 + year_2009 + year_2010 + 
                          year_2011 + year_2012 + year_2014 + year_2015 + year_2016 + 
                          year_2017 + year_2018"), test = "F") # tests for joint significance of each of
# the year dummies; F tells us we cannot reject the null, so we can reject year FEs

# kNN Imputation (with ctry fixed effects)

reg4a <- glm.nb(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                  dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                  openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impk, 
                control=glm.control(maxit=20)) # kNN imputation model
(se4a <- coeftest(reg4a, vcov = vcovHC(reg4a, type = "HC0", cluster = wb2_lag_impk$ccode)))

reg4b <- glm.nb(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                  dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                  openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impk, 
                control=glm.control(maxit=20))
(se4b <- coeftest(reg4b, vcov = vcovHC(reg4b, type = "HC0", cluster = wb2_lag_impk$ccode)))

stargazer(reg4a, reg4b, se = list(se4a[,2], se4b[,2]),
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode", add.lines = ctryfe,
          omit.stat = c("ll", "aic", "theta")) # print table (Table A4)

# Drop Missing Variables (with multiple imputation, ctry fixed effects)

reg5a <- glm.nb(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                  dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                  openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag, 
                control=glm.control(maxit=20)) # missing data rows are dropped
(se5a <- coeftest(reg5a, vcov = vcovHC(reg5a, type = "HC0", cluster = wb2_lag$ccode)))

reg5b <- glm.nb(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                  dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                  openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag, 
                control=glm.control(maxit=20)) # missing data rows are dropped
(se5b <- coeftest(reg5b, vcov = vcovHC(reg5b, type = "HC0", cluster = wb2_lag$ccode)))

stargazer(reg5a, reg5b, se = list(se5a[,2], se5b[,2]),
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode", add.lines = ctryfe,
          omit.stat = c("ll", "aic", "theta")) # print table (Table A5)

# Ctry Random Effects (with multiple imputation)

reg7a <- glmer.nb(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                    dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                    openness + war + elec + IMF + crisis + (1|ccode), data = wb2_lag_impm, 
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e10))) # random effects
summary(reg7a) # view results

reg7b <- glmer.nb(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                    dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                    openness + war + elec + IMF + crisis + (1|ccode), data = wb2_lag_impm, 
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e10))) # random effects
summary(reg7b) # view results

stargazer(reg7a, reg7b,
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode", add.lines = ctryre,
          omit.stat = c("ll", "aic", "bic", "theta")) # print table (Table A8)

# Alternate model specifications robustness checks (with ctry fixed effects and multiple imputation)
ctryfe2 <- list(c("Country fixed effects", "Yes", "Yes", "Yes", "Yes"))
reg8a <- lm(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
              dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
              openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impm) # OLS
(se8a <- coeftest(reg8a, vcov = vcovHC(reg8a, type = "HC0", cluster = wb2_lag_impm$ccode)))

reg9a <- glm(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
               dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
               openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impm,
             family = poisson(link = "log"), control=glm.control(maxit=20)) # Poisson
(se9a <- coeftest(reg9a, vcov = vcovHC(reg9a, type = "HC0", cluster = wb2_lag_impm$ccode)))

reg8b <- lm(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
              dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
              openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impm) # OLS
(se8b <- coeftest(reg8b, vcov = vcovHC(reg8b, type = "HC0", cluster = wb2_lag_impm$ccode)))

reg9b <- glm(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
               dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
               openness + war + elec + IMF + crisis + factor(ccode), data = wb2_lag_impm,
             family = poisson(link = "log"), control=glm.control(maxit=20)) # Poisson
(se9b <- coeftest(reg9b, vcov = vcovHC(reg9b, type = "HC0", cluster = wb2_lag_impm$ccode)))

stargazer(reg8a, reg9a, reg8b, reg9b, se = list(se8a[,2], se9a[,2], se8b[,2], se9b[,2]),
          dep.var.labels = c("Number of prior actions", "Number of prior actions", "Number of categories", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode", add.lines = ctryfe2,
          omit.stat = c("ll", "aic", "theta", "rsq", "adj.rsq", "ser", "f")) # print table (Table A7)

# Only keep the most theoretically important covariates without substantial missingness (with ctry fixed effects and missing rows dropped)

covs_restrict = c("UN voting (ideal pt dist from U.S.)", "World Bank board member", "EU president colony", 
                  "UNSC member", "IMF program", "Post-2012")

reg10a <- glm.nb(count_pa ~ absidealimportantdiff + board + colony + unsc +
                   IMF + crisis + factor(ccode), 
                 data = wb2_lag, 
                 control=glm.control(maxit=20)) # restricted controls model
(se10a <- coeftest(reg10a, vcov = vcovHC(reg10a, type = "HC0", cluster = wb2_lag$ccode)))

reg10b <- glm.nb(numcats ~ absidealimportantdiff + board + colony + unsc +
                   IMF + crisis + factor(ccode), 
                 data = wb2_lag, 
                 control=glm.control(maxit=20)) # restricted controls model
(se10b <- coeftest(reg10b, vcov = vcovHC(reg10b, type = "HC0", cluster = wb2_lag$ccode)))

stargazer(reg10a, reg10b, se = list(se10a[,2], se10b[,2]),
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_restrict,
          style = "ajps", omit = "ccode", add.lines = ctryfe,
          omit.stat = c("ll", "aic", "theta")) # print table (Table A6)

# Interaction between IMF and UNSC (with ctry fixed effects and multiple imputation)

covs_full_int <- c("UNSC member", "IMF program", "UN voting (ideal pt dist from U.S.)", "World Bank board member", "EU president colony", 
                   "U.S. aid", "Chinese aid", "GDPPC", "Debt service / GDP", 
                   "Short-term debt / exports", "Inflation", "Debt / GDP", "FDI / GDP", "Polity2", 
                   "Openness", "War", "Election year", "Post-2012", "UNSC member:IMF program") # covariate labels

wb2_lag_impmr$noIMF <- ifelse(wb2_lag_impmr$IMF == 1, 0, 1) # create no IMF variable to get significance on unsc

reg12a <- glm.nb(count_pa ~ unsc*IMF + absidealimportantdiff + board + colony + USaid + CHaid + gdppc + 
                   dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                   openness + war + elec + crisis + factor(ccode), data = wb2_lag_impmr, 
                 control=glm.control(maxit=20)) # full controls with count DV
(se12a <- coeftest(reg12a, vcov = vcovHC(reg12a, type = "HC0", cluster = wb2_lag_impmr$ccode)))
# if we swap interaction to noIMF to get the coefficient on unsc for IMF countries, coefficient is
# -0.0591550 with p = 0.3755495

reg12b <- glm.nb(numcats ~ unsc*IMF + absidealimportantdiff + board + colony + USaid + CHaid + gdppc + 
                   dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                   openness + war + elec + crisis + factor(ccode), data = wb2_lag_impmr, 
                 control=glm.control(maxit=20)) # full controls with stone DV
(se12b <- coeftest(reg12b, vcov = vcovHC(reg12b, type = "HC0", cluster = wb2_lag_impmr$ccode)))
# if we swap interaction to noIMF (just change the IMF term in the above regression to noIMF) to get 
# the coefficient on unsc for IMF countries, coefficient is 0.1277904 with p = 0.0800234

stargazer(reg12a, reg12b, se = list(se12a[,2], se12b[,2]), 
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full_int,
          style = "ajps", omit = "ccode", add.lines = ctryfe,
          omit.stat = c("ll", "aic", "theta")) # print table (Table A10)

interaction_plot_binary <- function(model, effect, moderator, interaction, varcov="default", conf=.95, title="Marginal effects plot", xlabel="Value of moderator", ylabel="Estimated marginal coefficient", factor_labels=c(0,1)){
  
  # Extract Variance Covariance matrix
  if (varcov == "default"){
    covMat = vcov(model)
  }else{
    covMat = varcov
  }
  
  # Extract the data frame of the model
  mod_frame = model.frame(model)
  
  # Get coefficients of variables
  beta_1 = model$coefficients[[effect]]
  beta_3 = model$coefficients[[interaction]]
  
  # Create list of moderator values at which marginal effect is evaluated
  x_2 <- c(0,1)
  
  # Compute marginal effects
  delta_1 = beta_1 + beta_3*x_2
  
  # Compute variances
  var_1 = covMat[effect,effect] + (x_2^2)*covMat[interaction, interaction] + 2*x_2*covMat[effect, interaction]
  
  # Standard errors
  se_1 = sqrt(var_1)
  
  # Upper and lower confidence bounds
  z_score = qnorm(1 - ((1 - conf)/2))
  upper_bound = delta_1 + z_score*se_1
  lower_bound = delta_1 - z_score*se_1
  
  # Determine the bounds of the graphing area
  max_y = max(upper_bound)
  min_y = min(lower_bound)
  
  # Initialize plotting window
  plot(x=c(), y=c(), ylim=c(min_y, max_y), xlim=c(-.5, 1.5), xlab=xlabel, ylab=ylabel, main=title, xaxt="n")
  
  # Plot points of estimated effects
  points(x=x_2, y=delta_1, pch=16)
  
  # Plot lines of confidence intervals
  lines(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), lty=1)
  points(x=c(x_2[1], x_2[1]), y=c(upper_bound[1], lower_bound[1]), pch=c(25,24), bg="black")
  lines(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), lty=1)
  points(x=c(x_2[2], x_2[2]), y=c(upper_bound[2], lower_bound[2]), pch=c(25,24), bg="black")
  
  # Label the axis
  axis(side=1, at=c(0,1), labels=factor_labels)
  
  # Add a dashed horizontal line for zero
  abline(h=0, lty=3)
  
}
interaction_plot_binary(reg12a, "unsc", "IMF", "unsc:IMF", title = "", xlabel = "IMF progam", 
                        ylabel = "Estimated marginal coefficient of UNSC member") # Figure A2
interaction_plot_binary(reg12b, "unsc", "IMF", "unsc:IMF", title = "", xlabel = "IMF progam", 
                        ylabel = "Estimated marginal coefficient of UNSC member") # Figure A3

# Nelson robustness check (using Nelson's data on liberal ideology)

nelson <- read.dta("nelson.dta") # load Nelson replication data
nelson$ccode <- countrycode(nelson$ccode, origin = "cown", destination = "iso3c") # harmonize codes
nelsonimp <- wb2_lag_impm # create new data for Nelson analysis
nelsonagg <- aggregate(nelson$proportion_neoliberal, by = list(nelson$ccode), FUN = mean) # take average liberal score for each country
nelsonimp$ccode <- as.character(nelsonimp$ccode) # change data format
colnames(nelsonagg) <- c("ccode", "prop_neolib") # rename columns
nelsonimp <- merge(nelsonimp, nelsonagg, by = c("ccode")) # merge data

covs_full_nelson <- c("UN voting (ideal pt dist from U.S.)", "World Bank board member", "EU president colony", 
                      "UNSC member", "U.S. aid", "Chinese aid", "GDPPC", "Debt service / GDP", 
                      "Short-term debt / exports", "Inflation", "Debt / GDP", "FDI / GDP", "Polity2", 
                      "Openness", "War", "Election year", "IMF program", "Post-2012", "Liberal Ideology") # covariate labels

reg13a <- glm.nb(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                   dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                   openness + war + elec + IMF + crisis + prop_neolib, data = nelsonimp, 
                 control=glm.control(maxit=20)) # full controls with count DV no country FE
(se13a <- coeftest(reg13a, vcov = vcovHC(reg13a, type = "HC0", cluster = nelsonimp$ccode)))

reg13b <- glm.nb(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                   dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                   openness + war + elec + IMF + crisis + prop_neolib, data = nelsonimp, 
                 control=glm.control(maxit=20)) # full controls with stone DV no country FE
(se13b <- coeftest(reg13b, vcov = vcovHC(reg13b, type = "HC0", cluster = nelsonimp$ccode)))

stargazer(reg13a, reg13b, se = list(se13a[,2], se13b[,2]), 
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full_nelson,
          style = "ajps", omit = "ccode",
          omit.stat = c("ll", "aic", "theta")) # print table (Table A14)

# Nelson interaction tests (to tell whether the two variables work together)
reg13c <- glm.nb(count_pa ~ absidealimportantdiff*prop_neolib + board + colony + unsc + USaid + CHaid + gdppc + 
                   dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                   openness + war + elec + IMF + crisis, data = nelsonimp, 
                 control=glm.control(maxit=20))
(se13c <- coeftest(reg13c, vcov = vcovHC(reg13c, type = "HC0", cluster = nelsonimp$ccode)))

reg13d <- glm.nb(numcats ~ absidealimportantdiff*prop_neolib + board + colony + unsc + USaid + CHaid + gdppc + 
                   dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                   openness + war + elec + IMF + crisis, data = nelsonimp, 
                 control=glm.control(maxit=20))
(se13d <- coeftest(reg13d, vcov = vcovHC(reg13d, type = "HC0", cluster = nelsonimp$ccode)))
# results to this test do not appear in a table, but they are discussed briefly in the caption to
# Table A14

##->## Selection Robustness Check

# First stage selection model

c2 <- c("UN voting (ideal pt dist from U.S.)", "UNSC member", "U.S. aid", "DAC aid",
        "Chinese aid", "Population",  "Polity2", "Reserves / GDP", "GDPPC", "Current account / GDP", 
        "Debt / GDP", "FDI / GDP", "Inflation", "Election year", "War", "IMF program") # covariate labels for first stage

select1 <- glm(part ~ absidealimportantdiff + unsc + USaid + DACaid + CHaid + pop + polity2 + 
                 reservestoGDP + gdppc + curracctoGDP + debttoGDP + FDItoGDP + inflation + elec + 
                 war + IMF + factor(ccode) + factor(year), family = quasibinomial(link = "probit"), 
               data = wbpart2imp, control=glm.control(maxit=500)) # first stage regression
(seselect1 <- coeftest(select1, vcov = vcovHC(select1, type = "HC0", cluster = wbpart2imp$ccode)))

stargazer(select1, se = list(seselect1[,2]),
          dep.var.labels = c("Participation in DPF program"),
          covariate.labels = c2,
          style = "ajps", omit = c("ccode", "year"), add.lines = c(ctryfe, yearfe),
          omit.stat = c("ll", "aic", "theta")) # print table (Table A12)                         

# Merge predicted probabilities into data

wbpart2imp$fit <- fitted(select1) # extract fitted values
wbpart2imp$year <- as.numeric(as.character(wbpart2imp$year)) # change data format
fitdat <- cbind(as.character(wbpart2imp$ccode), wbpart2imp$year, wbpart2imp$fit) # create new participation dataframe
colnames(fitdat) <- c("ccode", "year", "fit2") # change column names
fitdat <- data.frame(fitdat) # change to dataframe format
wb2_lag_impms <- merge(wb2_lag_impm, fitdat, by = c("ccode", "year"), all.x = T) # merge into imputed data
wb2_lag_impms$fit2 <- as.numeric(as.character(wb2_lag_impms$fit2)) # change data format
wb2_lag_impms <- na.omit(wb2_lag_impms) # eliminate any missing data from first stage

# Second stage selection model with inverse probability weights

des1 <- svydesign(id=~ccode, weights=~fit2, variables =~ USaid + CHaid + absidealimportantdiff + gdppc +
                    dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                    openness + war + elec + board + colony + unsc + IMF + numcats +
                    count_pa + crisis, data = wb2_lag_impms) # set up weighting

select2a <- svyglm(count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                     dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                     openness + war + elec + IMF + crisis, family = poisson(), design=des1) # second stage regression
summary(select2a) # view results

select2b <- svyglm(numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                     dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                     openness + war + elec + IMF + crisis, family = poisson(), design=des1) # second stage regression
summary(select2b) # view results

stargazer(select2a, select2b,
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode",
          omit.stat = c("ll", "aic", "theta")) # exported as Table A13

# Heckman Two Stage

selsub <- cbind(as.character(wb2_lag_impm$ccode), wb2_lag_impm$year, wb2_lag_impm$count_pa, 
                wb2_lag_impm$board, wb2_lag_impm$colony,
                wb2_lag_impm$unsc, wb2_lag_impm$elec, wb2_lag_impm$numcats) # add variables not currently in the selection data to the selection data
colnames(selsub) <- c("ccode","year","count_pa", "board", "colony","unsc","elec", "numcats")
selsub <- data.frame(selsub)
sel <- merge(wbpart2imp, selsub, all.x = T)
sel$part <- as.logical(sel$part)
sel <- unique(sel)
sel$count_pa <- as.numeric(as.character(sel$count_pa))
sel$numcats <- as.numeric(as.character(sel$numcats))
sel$board <- as.numeric(as.character(sel$board))
sel$colony <- as.numeric(as.character(sel$colony))
sel$elec <- as.numeric(as.character(sel$elec))
sel$gdppc <- scale(sel$gdppc)
sel$dservtoGDP <- scale(sel$dservtoGDP)
sel$crisis <- 0
sel$crisis[sel$year > 2012] <- 1
sel <- unique(sel)

select3a <- heckit(selection = part ~ USaid + absidealimportantdiff + DACaid + CHaid + unsc + pop + polity2 + 
                     reservestoGDP + gdppc + curracctoGDP + debttoGDP + inflation + elec + 
                     war + IMF + factor(year) + factor(ccode), outcome = count_pa ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                     dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                     openness + war + elec + IMF + crisis, data = sel) # run heckman
summary(select3a) # Note: will not run with fixed effects in outcome, so they are dropped for this test only

select3b <- heckit(selection = part ~ USaid + absidealimportantdiff + DACaid + CHaid + unsc + pop + polity2 + 
                     reservestoGDP + gdppc + curracctoGDP + debttoGDP + inflation + elec + 
                     war + IMF + factor(year) + factor(ccode), outcome = numcats ~ absidealimportantdiff + board + colony + unsc + USaid + CHaid + gdppc + 
                     dservtoGDP + dshorttoexports + inflation + debttoGDP + FDItoGDP + polity2 + 
                     openness + war + elec + IMF + crisis, data = sel) # run heckman
summary(select3b) # Note: will not run with fixed effects in outcome, so they are dropped for this test only

stargazer(select3a, select3b,
          dep.var.labels = c("Number of prior actions", "Number of categories"),
          covariate.labels = covs_full,
          style = "ajps", omit = "ccode",
          omit.stat = c("ll", "aic", "theta", "rsq", "adj.rsq")) # exported as Table A11

######### Initial Text Work ###########

##->## World Bank

# Text data creation and summary bar plot
pa1 <- read.csv("Prior_Actions_Text_WB.csv", fileEncoding="UTF-8-BOM") # this is just the text tab of the Development Policy Action database 
# supplied to me by the World Bank with hand-coded IMF policy categories inspired by Kentikelenis et al.'s (2016)
# work on the IMF as well as project ID, country, and year.
pa1$TEXT <- as.character(pa1$TEXT) # change text to character

# Restrict WB sample to only observations with simultaneous IMF programs for comparability purposes
imfprog <- as.data.frame(cbind(as.character(wb2_lag_impm$ccode), wb2_lag_impm$year, wb2_lag_impm$IMF)) # create dataset with IMF program participation information
colnames(imfprog) <- c("ccode", "year", "IMF") # change column names
imfprog$ccode <- as.character(imfprog$ccode)
pa1$ctry <- as.character(pa1$ctry)
pa1$ccode <- countrycode(pa1$ctry, origin = "country.name", destination = "iso3c")
pa1alt <- merge(pa1, imfprog, by = c("ccode", "year"), all.x = T) # merge data
pa1alt <- unique(pa1alt) # eliminate duplicates
pa1sub <- pa1alt[pa1alt$IMF == 1,] # subset the data to observations with IMF programs
corpus2a <- Corpus(VectorSource(pa1sub$TEXT)) # change to corpus for analysis
docs2a <- tm_map(corpus2a, content_transformer(tolower)) # Convert the text to lower case
docs2a <- tm_map(docs2a, removeNumbers) # Remove numbers
docs2a <- tm_map(docs2a, removeWords, stopwords("english")) # Remove english common stopwords
docs2a <- tm_map(docs2a, removeWords, c("blabla1", "blabla2")) # specify your stopwords as a character vector
docs2a <- tm_map(docs2a, removePunctuation) # Remove punctuations
docs2a <- tm_map(docs2a, removeWords, c("dated", "year", "january", "february", "march", "april", "may", "june", 
                                        "july", "august", "september", "october", "november", "december",
                                        "will", "shortterm", "mediumlongterm", "mefp")) # Remove my won commonly identified stop words
docs2a <- tm_map(docs2a, stripWhitespace) # Eliminate extra white spaces

dtm2a <- TermDocumentMatrix(docs2a) # change format
m2a <- as.matrix(dtm2a) # change to matrix
v2a <- sort(rowSums(m2a),decreasing=TRUE) # sort data
d2a <- data.frame(word = names(v2a),freq=v2a) # record frequencies
d2a <- d2a[-c(5,14,15),] # deletes stop words "new" and "including" and plural of "recipient" for redundancy
head(d2a, 20) # print top 20 words and frequencies

wbsub <- data.frame(text = sapply(docs2a, as.character), stringsAsFactors = FALSE) # create dataframe
wbsub <- na.omit(wbsub) # remove NAs
write.csv(wbsub, "wbtextsub.csv") # write text data to csv

barplot(d2a[1:20,]$freq, las = 2, names.arg = d2a[1:20,]$word,
        col ="lightblue",
        ylab = "Word Frequencies",
        cex.names = .75) # Figure A4

# NOTE: I use the free app Wordle for Mac to generate the word clouds; they are nicer and easier to
# interpret than the ones produced by common R packages. I just copy and paste the text columns from
# the downloaded data (wbtextsub.csv) directly into the Wordle input box. The maximum number of words 
# option is set to 150. See the readme file for more detailed specifications for these plots.

##->## IMF

# Text data creation and summary bar plot
pa2 <- read_csv("Prior_Actions_Text_IMF.csv") # load text of conditions from Kentikelenis et al. (2016); this is their raw data
pa2 <- pa2[pa2$Year > 2004,] # restrict to the same years as WB data
pa2$Text <- as.character(pa2$Text) # change format
corpus2 <- Corpus(VectorSource(pa2$Text)) # change to corpus for analysis
docs2 <- tm_map(corpus2, content_transformer(tolower)) # Convert the text to lower case
docs2 <- tm_map(docs2, removeNumbers) # Remove numbers
docs2 <- tm_map(docs2, removeWords, stopwords("english")) # Remove english common stopwords
docs2 <- tm_map(docs2, removeWords, c("blabla1", "blabla2")) # specify your stopwords as a character vector
docs2 <- tm_map(docs2, removePunctuation) # Remove punctuations
docs2 <- tm_map(docs2, removeWords, c("dated", "year", "january", "february", "march", "april", "may", "june", 
                                      "july", "august", "september", "october", "november", "december",
                                      "will", "shortterm", "mediumlongterm", "mefp")) # Remove my commonly identified stop words and redundant plurals
docs2 <- tm_map(docs2, stripWhitespace) # Eliminate extra white spaces

dtm2 <- TermDocumentMatrix(docs2) # change format
m2 <- as.matrix(dtm2) # change to matrix
v2 <- sort(rowSums(m2),decreasing=TRUE) # sort data
d2 <- data.frame(word = names(v2),freq=v2) # record frequencies
d2 <- d2[-4,] # delete stop word "new"
head(d2, 20) # print top 20 words and frequencies

imf <- data.frame(text = sapply(docs2, as.character), stringsAsFactors = FALSE) # create dataframe
write.csv(imf, "imftext.csv") # write text data to csv
# NOTE: I use the free app Wordle for Mac to geenrate the word clouds; they are nicer and easier to
# interpret than the ones produced by common R packages. I just copy and paste the text columns from
# the downloaded data (imftext.csv) directly into the Wordle input box. The maximum number of words 
# option is set to 150. See the readme file for more detailed specifications for these plots.

barplot(d2[1:20,]$freq, las = 2, names.arg = d2[1:20,]$word,
        col ="red",
        ylab = "Word Frequencies",
        cex.names = .75) # Figure A4

# Policy area count plots
pa2$count <- 1 # want to sum conditions by policy area; this is a counter variable
c2 <- aggregate(pa2$count, by = list(pa2$`Policy Area (short)`), FUN = sum) # aggregate counter by area
colnames(c2) <- c("cat", "count") # change column names
# I exported this file and manually combined it with the hand-coded World Bank policy area data to yield
# the dataset that is loaded below
counts <- read.csv("Policy_Area_Counts_WB.csv", fileEncoding="UTF-8-BOM") # load hand-coded data of policy area counts for WB projects
positions <- c("FP", "FIN", "DEB", "RTP", "SOE", "EXT", "LAB", "INS", "POV", "PRI", "SP", "ENV") # set policy area levels for plots
# Plot of frequency of conditions in each category
ggplot(counts, aes(fill=IO, y=freq, x=cat)) +
  theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = "Conditionality Category", y = "Ratio") +
  scale_x_discrete(limits = positions,
                   labels = c("Fiscal Policy", "Financial Policy", "Debt Policy", "Revenues & Taxes", 
                              "SOE Reform", "External Sector", "Labor", "Institutions", 
                              "Redistribution", "Privatization", "Social Policy", 
                              "Environment")) +
  scale_fill_grey(start = 0, end = .9)

######### Text for U.S. friends vs. foes #########

# Split word cloud data by friends and foes of the U.S.
pa1$ccode <- countrycode(pa1$ctry, origin = "country.name", destination = "iso3c") # add country code to World Bank text data
untemp <- data.frame(cbind(as.character(wb2_lag_impm$ccode), as.character(wb2_lag_impm$year), wb2_lag_impm$absidealimportantdiff)) # make temporary UN dataset from overall data set
colnames(untemp) <- c("ccode", "year", "absidealimportantdiff") # change column names
untemp$absidealimportantdiff <- as.numeric(as.character(untemp$absidealimportantdiff)) # change format
untemp$year <- as.numeric(as.character(untemp$year)) # change format
pa1s <- merge(pa1, untemp, by = c("ccode", "year")) # merge data
pa1s$absidealimportantdiff <- as.numeric(as.character(pa1s$absidealimportantdiff)) # change format
pa1s$year <- as.numeric(as.character(pa1s$year)) # change format
pa1s$friend <- 0 # make empty friend column
pa1s$foe <- 0 # make empty foe column

for (i in 1:11405) {
  yr <- pa1s$year[i]
  temp <- untemp[untemp$year == yr,]
  med <- median(na.omit(temp$absidealimportantdiff))
  pa1s$friend[i] <- ifelse(pa1s$absidealimportantdiff[i] < med, 1, 0)
  pa1s$foe[i] <- ifelse(pa1s$absidealimportantdiff[i] > med, 1, 0)
}  # code friends as countries below median UN voting dist and foes as above

pa1friend <- pa1s[pa1s$friend == 1,] # make a subset of just friends
pa1foe <- pa1s[pa1s$foe == 1,] # make a subset of just foes

# Process the data as above for both friends and foes
pa1friend$TEXT <- as.character(pa1friend$TEXT)
corpus <- Corpus(VectorSource(pa1friend$TEXT))
docs <- tm_map(corpus, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("dated", "year", "january", "february", "march", "april", "may", "june", 
                                    "july", "august", "september", "october", "november", "december", "iii"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
friend <- data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)

pa1foe$TEXT <- as.character(pa1foe$TEXT)
corpus <- Corpus(VectorSource(pa1foe$TEXT))
docs <- tm_map(corpus, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("dated", "year", "january", "february", "march", "april", "may", "june", 
                                    "july", "august", "september", "october", "november", "december", "iii"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
foe <- data.frame(text = sapply(docs, as.character), stringsAsFactors = FALSE)

write.csv(friend, "friendtext.csv") # print the text data to csv; toggled off to avoid overwriting
write.csv(foe, "foetext.csv")
# NOTE: I use the free app Wordle for Mac to generate the word clouds; they are nicer and easier to
# interpret than the ones produced by common R packages. I just copy and paste the text columns from
# the downloaded data (friendtext.csv and foetext.csv) directly into the Wordle input box. The maximum 
# number of words option is set to 150. See the readme file for more detailed specifications for these plots.
