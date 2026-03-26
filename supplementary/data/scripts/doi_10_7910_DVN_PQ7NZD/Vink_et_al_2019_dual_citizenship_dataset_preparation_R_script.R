##############################################
###
### basic R code for the creation of the dataset for:

# Vink, M., Schakel, A., Reichel, D., De Groot, R., and Luk, C. (2019):
# The international diffusion of expatriate dual citizenship,
# Migration Studies

###Dataset files 
#Running this R script will produce the following six files: 

#Full data file with observations for all country-years
#File "Vink_Schakel_Reichel_Chun_deGroot_2019_full.csv"
#File "Vink_Schakel_Reichel_Chun_deGroot_2019_full.xlsx"

#File in event data format used for the main analysis (in .csv/.xlsx)
#File 'Vink_Schakel_Reichel_Chun_deGroot_2019_main.csv'
#File 'Vink_Schakel_Reichel_Chun_deGroot_2019_main.xlsx'

#File used for robustnesss analysis, excluding backsliding countries (in .csv/.xlsx)
#File 'Vink_Schakel_Reichel_Chun_deGroot_2019_excl_backsliding_countries.csv'
#File 'Vink_Schakel_Reichel_Chun_deGroot_2019_excl_backsliding_countries.xlsx'
#These files are posted in the replication dataverse

###Data sources
#The following data files were used in order to construct the final datasets prepared for the analyses included in the paper:
#These replication files are posted in the replication dataverse (#except File 'V-Dem-CY-Core-v8.csv')

#Dual citizenship acceptance
#File 'MACIMIDE Global Dual Citizenship Database_v3.00.csv'
#Source: Vink, M., de Groot, G. -R. and Luk, N.C. (2015) ‘MACIMIDE Global Expatriate Dual Citizenship Dataset’, doi: 10.7910/DVN/TTMZ08, Harvard Dataverse, V3.

#Voting from abroad 
#File: 'IDEA.xlsx'
#Source: International Institute for Democracy and Electoral Assistance (International IDEA) (2015) ‘Voting from Abroad Database’ <http://www.idea.int/elections/vfa/search.cfm> accessed 19 Apr 2016.

#Neighbouring countries (cshapes)
#File 'dyadic_dist_2017b.txt'
#File 'dyadic_dist_2016b.txt'
#File 'dyadic_dist_2015b.txt'
#File 'dyadic_dist_2000_2014b.txt'
#File 'dyadic_dist_1990_1999b.txt'
#File 'dyadic_dist_1980_1989b.txt'
#File 'dyadic_dist_1970_1979b.txt'
#File 'dyadic_dist_1960_1969b.txt'
#Source: Weidmann, N. B. and Gleditsch, K. S. (2010) ‘Mapping and Measuring Country Shapes: The Cshapes Package’, The R Journal, 2/1: 18–24.

#Political regime
#File 'V-Dem-CY-Core-v8.csv'
#NB File is NOT posted in dataverse (due to size of the file) but can be downloaded from orignal source at DOI below
#Source: Coppedge, M. et al. (2018) ‘V-Dem [Country-Year/Country-Date] Dataset v8’. Varieties of Democracy (V-Dem) Project <https://doi.org/10.23696/vdemcy18> accessed 9 April 2019.

#Received remittances
#File '#API_BX.TRF.PWKR.CD.DT_DS2_en_excel_v2_9944665.xlsx'
#Source: Arel-Bundock, V. (2018) WDI: World Development Indicators (World Bank). R package version 2.5. <http://CRAN.R-project.org/package=WDI> accessed 9 April 2019.

##############################################
## 
# July 2018

library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(openxlsx)
library(countrycode)

setwd("PATH") # insert working directory where all files are stored

dat <- read.csv("MACIMIDE Global Dual Citizenship Database_v3.00.csv",
                stringsAsFactors = FALSE)
nrow(dat) # check number of rows 

# calculate dummy variable as main dependent variable
dat$Dualcit_binary2 <- ifelse(dat$Dualcit_binary == 1, 0,
                              ifelse(dat$Dualcit_binary == 2, 1, NA))

dat$independ_af_1960 <- ifelse(dat$Year == 1960 & dat$Dualcit_cat == 0,
                               1, 0)
dat <- dat %>%
  group_by(ISO3) %>%
  mutate(independ_af_1960 = ifelse(sum(independ_af_1960) != 0, 1, 0)) %>%
  ungroup()

# We assign the Soviet Union codes to Russia
dat$Dualcit_binary2[dat$ISO3 ==  "RUS" & dat$Year %in% c(1960:1991)] <- 1
# to exclude Soviet Union altogether we could run: dat[dat$ISO3 != "SUN", ]

dat$cowc <- countrycode(dat$ISO3, "iso3c", "cowc") # add cowc codes to original dataset
# Some values were not matched unambiguously: SCG, SRB, SUN
# countrycode("Serbia and Montenegro", "country.name", "cowc")

dat$cown <- countrycode(dat$ISO3, "iso3c", "cown")

dat[dat$cowc == "AFG", c("cowc", "ISO3", "country", "Year")]

dat$cowc[dat$cowc == "DRC"] <- "ZAI"
dat$cowc[dat$cowc == "GMY" & dat$Year %in% c(1946:1990)] <- "GFR" # Germany only available as of 1960
dat$cowc[dat$cowc == "ETH" & dat$Year %in% c(1993:2018)] <- "ETI"
dat$cowc[dat$cowc == "MNG"] <- "MNT"
dat$cowc[dat$cowc == "ROM"] <- "RUM"
dat$cowc[dat$ISO3 == "SRB"] <- "SER"
dat$cowc[dat$ISO3 == "SUN"] <- "USR"
dat$cowc[dat$cowc == "DRV"] <- "VIE"

# calculate if there was a change from 0 to 1 compared to the previous years
# (basically if the value of the current year is larger than the previous year)
dat <- dat %>%
  arrange(ISO3, Year) %>%
  group_by(ISO3) %>%
  mutate(change_dual_bin = Dualcit_binary2 > lag(Dualcit_binary2)) %>%
  ungroup()

dat <- dat %>%
  group_by(world_region, Year) %>%
  mutate(meanDualregion = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()

# calculate neighbours and variable
# the tables with the distance were prepared based on the R package cshapes
# and saved away due to the long time it takes to prepare/calculate them
# we use the same dataset from 2015 up to 2017 as no major differences are expected in borders

dat0c <- read.table("dyadic_dist_2017b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)

dat0b <- read.table("dyadic_dist_2016b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat0b <- subset(dat0b, mindist<500)

dat0 <- read.table("dyadic_dist_2015b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat0 <- subset(dat0, mindist<500)

dat1 <- read.table("dyadic_dist_2000_2014b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat1 <- subset(dat1, mindist<500)

dat2 <- read.table("dyadic_dist_1990_1999b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat2 <- subset(dat2, mindist<500)

dat3 <- read.table("dyadic_dist_1980_1989b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat3 <- subset(dat3, mindist<500)

dat4 <- read.table("dyadic_dist_1970_1979b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat4 <- subset(dat4, mindist<500)

dat5 <- read.table("dyadic_dist_1960_1969b.txt", header=T) %>%
  group_by(ccode1) %>% 
  mutate(ranknear = rank(mindist)) %>%
  ungroup() %>%
  filter(mindist < 500 | ranknear < 4) %>%
  dplyr::select(-ranknear)
# dat5 <- subset(dat5, mindist<500)

nrow(dat1)

datfull <- rbind(dat0c, dat0b, dat0, dat1, dat2, dat3, dat4, dat5)
head(datfull)
rm(dat0c)
rm(dat0b)
rm(dat0)
rm(dat1)
rm(dat2)
rm(dat3)
rm(dat4)
rm(dat5)

datfull$iso3 <- countrycode(datfull$ccode1, "cown", "iso3c")
datfull$iso3b <- countrycode(datfull$ccode2, "cown", "iso3c")

# datfull$iso3b[datfull$ccode2==345] <- "YUG"
datfull$iso3b[datfull$ccode2==260] <- "DEU"
datfull$iso3[datfull$ccode1==260] <- "DEU"

datfull$iso3b[datfull$ccode2==678] <- "YEM"
datfull$iso3[datfull$ccode1==678] <- "YEM"

datfull$iso3b[datfull$ccode2==817] <- "VNM"
datfull$iso3[datfull$ccode1==817] <- "VNM"

# Kosovo is not included in the dataset

# subset only including selected variables
datsub <- dplyr::select(dat, ISO3, Year, Dualcit_binary2, change_dual_bin)

# merge with neighbouring states dataset
datfull <- merge(datfull, datsub, by.x=c("iso3b", "year"), by.y=c("ISO3", "Year"),
                 all.x=TRUE)
datfull <- dplyr::arrange(datfull, iso3, year)

datfull_agg <- datfull %>%
  group_by(iso3, year) %>%
  summarise(mean_dual_cit_neighbours = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()
names(datfull_agg)[3] <- "mean_dual_cit_neighbours500km"

# changein neighbouring country
datfull_agg2 <- datfull %>%
  group_by(iso3, year) %>%
  summarise(change_dual_cit_neighbours = sum(change_dual_bin, na.rm = TRUE)) %>%
  ungroup()

datfull_agg2 <- arrange(datfull_agg2, iso3, year)

datfull_agg2 <- datfull_agg2 %>%
  group_by(iso3) %>%
  mutate(change_dual_cit_neighbours_lag1 = dplyr::lag(change_dual_cit_neighbours, 1),
         change_dual_cit_neighbours_lag2 = dplyr::lag(change_dual_cit_neighbours, 2),
         change_dual_cit_neighbours_lag3 = dplyr::lag(change_dual_cit_neighbours, 3),
         change_dual_cit_neighbours_lag4 = dplyr::lag(change_dual_cit_neighbours, 4),
         change_dual_cit_neighbours_lag5 = dplyr::lag(change_dual_cit_neighbours, 5)) %>%
  ungroup()

# calculate sum of changes in the two years before of neighbours
datfull_agg2$change_dual_cit_neighbours1_2 <- rowSums(cbind(datfull_agg2$change_dual_cit_neighbours_lag1, 
                                                            datfull_agg2$change_dual_cit_neighbours_lag2), na.rm = TRUE)

# create one counting the changes and one dummy
datfull_agg2$change_dual_cit_neighbours1_2_cont <- datfull_agg2$change_dual_cit_neighbours1_2
datfull_agg2$change_dual_cit_neighbours1_2 <- ifelse(datfull_agg2$change_dual_cit_neighbours1_2 > 0,
                                                     1, 0)

# calculate sum of changes in the five years before of neighbours
datfull_agg2$change_dual_cit_neighbours1_5 <- rowSums(cbind(datfull_agg2$change_dual_cit_neighbours_lag1, 
                                                            datfull_agg2$change_dual_cit_neighbours_lag2,
                                                            datfull_agg2$change_dual_cit_neighbours_lag3,
                                                            datfull_agg2$change_dual_cit_neighbours_lag4,
                                                            datfull_agg2$change_dual_cit_neighbours_lag5), na.rm = TRUE)

# create one counting the changes and one dummy
datfull_agg2$change_dual_cit_neighbours1_5_cont <- datfull_agg2$change_dual_cit_neighbours1_5
datfull_agg2$change_dual_cit_neighbours1_5 <- ifelse(datfull_agg2$change_dual_cit_neighbours1_5 > 0,
                                                     1, 0)

dat <- merge(dat, datfull_agg, by.x=c("ISO3", "Year"), by.y=c("iso3", "year"),
             all.x=T)
dat <- merge(dat, datfull_agg2, by.x=c("ISO3", "Year"), by.y=c("iso3", "year"),
             all.x=T)
nrow(dat)
# --------------------------------------------------------------------------------

# -----------------------------------------------------------------------------------
# new calculate non neighbour change

# calculate neighbours and variable
# the tables with the distance were prepared based on the R package and saved away due to the long time it takes to prepare/calculate them
dat0 <- read.table("dyadic_dist_2015b.txt", header=T)
dat0 <- subset(dat0, mindist>500)

dat1 <- read.table("dyadic_dist_2000_2014b.txt", header=T)
dat1 <- subset(dat1, mindist>500)

dat2 <- read.table("dyadic_dist_1990_1999b.txt", header=T)
dat2 <- subset(dat2, mindist>500)

dat3 <- read.table("dyadic_dist_1980_1989b.txt", header=T)
dat3 <- subset(dat3, mindist>500)

dat4 <- read.table("dyadic_dist_1970_1979b.txt", header=T)
dat4 <- subset(dat4, mindist>500)

dat5 <- read.table("dyadic_dist_1960_1969b.txt", header=T)
dat5 <- subset(dat5, mindist>500)

nrow(dat1)

datfull <- rbind(dat0, dat1, dat2, dat3, dat4, dat5)
head(datfull)
rm(dat0)
rm(dat1)
rm(dat2)
rm(dat3)
rm(dat4)
rm(dat5)

datfull$iso3 <- countrycode(datfull$ccode1, "cown", "iso3c")
datfull$iso3b <- countrycode(datfull$ccode2, "cown", "iso3c")

# datfull$iso3b[datfull$ccode2==345] <- "YUG"
datfull$iso3b[datfull$ccode2==260] <- "DEU"
datfull$iso3[datfull$ccode1==260] <- "DEU"

datfull$iso3b[datfull$ccode2==678] <- "YEM"
datfull$iso3[datfull$ccode1==678] <- "YEM"

datfull$iso3b[datfull$ccode2==817] <- "VNM"
datfull$iso3[datfull$ccode1==817] <- "VNM"

# no kosovo

datsub <- dplyr::select(dat, ISO3, Year, Dualcit_binary2, change_dual_bin)

datfull <- merge(datfull, datsub, by.x=c("iso3b", "year"), by.y=c("ISO3", "Year"),
                 all.x=TRUE)
datfull <- dplyr::arrange(datfull, iso3, year)

datfull_agg <- datfull %>%
  group_by(iso3, year) %>%
  summarise(mean_dual_cit_non_neighbours = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()
head(datfull_agg)
names(datfull_agg)[3] <- "mean_dual_cit_non_neighbours500km"

# change in non-neighbouring country
datfull_agg2 <- datfull %>%
  group_by(iso3, year) %>%
  summarise(change_dual_cit_non_neighbours = sum(change_dual_bin, na.rm = TRUE)) %>%
  ungroup()

datfull_agg2 <- arrange(datfull_agg2, iso3, year)

datfull_agg2 <- datfull_agg2 %>%
  group_by(iso3) %>%
  mutate(ch_dual_cit_non_nghb_lag1 = dplyr::lag(change_dual_cit_non_neighbours, 1),
         ch_dual_cit_non_nghb_lag2 = dplyr::lag(change_dual_cit_non_neighbours, 2),
         ch_dual_cit_non_nghb_lag3 = dplyr::lag(change_dual_cit_non_neighbours, 3),
         ch_dual_cit_non_nghb_lag4 = dplyr::lag(change_dual_cit_non_neighbours, 4),
         ch_dual_cit_non_nghb_lag5 = dplyr::lag(change_dual_cit_non_neighbours, 5)) %>%
  ungroup()

datfull_agg2$ch_dual_cit_non_nghb1_2 <- rowSums(cbind(datfull_agg2$ch_dual_cit_non_nghb_lag1, 
                                                      datfull_agg2$ch_dual_cit_non_nghb_lag2), na.rm = TRUE)

datfull_agg2$ch_dual_cit_non_nghb1_2_cont <- datfull_agg2$ch_dual_cit_non_nghb1_2
datfull_agg2$ch_dual_cit_non_nghb1_2 <- ifelse(datfull_agg2$ch_dual_cit_non_nghb1_2 > 0,
                                               1, 0)

datfull_agg2$ch_dual_cit_non_nghb1_5 <- rowSums(cbind(datfull_agg2$ch_dual_cit_non_nghb_lag1, 
                                                      datfull_agg2$ch_dual_cit_non_nghb_lag2,
                                                      datfull_agg2$ch_dual_cit_non_nghb_lag3,
                                                      datfull_agg2$ch_dual_cit_non_nghb_lag4,
                                                      datfull_agg2$ch_dual_cit_non_nghb_lag5), na.rm = TRUE)

datfull_agg2$ch_dual_cit_non_nghb1_5_cont <- datfull_agg2$ch_dual_cit_non_nghb1_5
datfull_agg2$ch_dual_cit_non_nghb1_5 <- ifelse(datfull_agg2$ch_dual_cit_non_nghb1_5 > 0,
                                               1, 0)

dat <- merge(dat, datfull_agg, by.x=c("ISO3", "Year"), by.y=c("iso3", "year"),
             all.x=T)
dat <- merge(dat, datfull_agg2, by.x=c("ISO3", "Year"), by.y=c("iso3", "year"),
             all.x=T)
nrow(dat)

rm(datfull_agg)
rm(datfull_agg2)



# --------------------------------------------------------------------------------
# add World Development Indicators (WDI)
library(WDI)

wd <- WDI(country = "all", indicator = c("SP.POP.TOTL", "NY.GDP.PCAP.CD",
                                          "NY.GDP.PCAP.PP.CD"),
           start = 1960, end = 2017, extra = TRUE)

wd <- wd[ ,c(1,2,3,4,5,6,7,10,11)]

dat <- merge(dat, wd, by.x = c("ISO3", "Year"), by.y = c("iso3c", "year"),
             all.x = TRUE)

nrow(dat)
mean(dat$Dualcit_binary)
rm(wd)

##### --------------------------------
# add vdem data
vdem <- read.csv("V-Dem-CY-Core-v8.csv") 
vdem <- subset(vdem, year>=1960)

vdem <- dplyr::select(vdem, country_text_id, year, v2x_regime) %>%
  rename(ISO3 = country_text_id, Year = year)

vdem$v2x_regime_dem <- as.integer(ifelse(vdem$v2x_regime < 2, 0, 1))

dat <- merge(dat, vdem, by = c("Year", "ISO3"),
             all.x=TRUE)
nrow(dat)
mean(dat$Dualcit_binary)
rm(vdem) # remove file to save space in memory

##### --------------------------------
# remittances received
rd <- read.xlsx("API_BX.TRF.PWKR.CD.DT_DS2_en_excel_v2_9944665.xlsx", sheet = 1,
                startRow = 3)

rd <- rd[ ,c(2,5:62)]

rd <- melt(rd, id="Country.Code")
head(rd)
rd$variable <- as.numeric(as.character(rd$variable))
names(rd) <- c("ISO3", "year", "remit_received")

dat <- merge(dat, rd, by.x=c("ISO3", "Year"), by.y=c("ISO3", "year"),
             all.x=TRUE)

mean(dat$Dualcit_binary)
nrow(dat)
rm(rd)


##### --------------------------------
# voting rights abroad
vot <- read.xlsx("IDEA.xlsx", sheet = 1)
vot$ISO2[vot$Country == "Namibia"] <- "NA"

# create empty dataset
vot_df <- expand.grid(year = c(1918:2017), country = unique(vot$ISO2))
vot_df$vfa <- 0
head(vot_df)

# fill empty dataset with 1 if voting rigths abroad have been adopted
for(i in unique(vot$ISO2)){
  for(j in 1918:2017){
    if (subset(vot, ISO2 == i)$vfa <= j) {
      vot_df$vfa[vot_df$year == j & vot_df$country == i] <- 1
    }
  }
}

vot_df$vfa[vot_df$year == 2015 & vot_df$country == "NA"]

vot_df <- vot_df %>%
  group_by(country) %>%
  mutate(ysince_vfa = cumsum(vfa)) %>%
  ungroup()

vot_df$vfa_intro_last3y <- ifelse(vot_df$ysince_vfa > 0 & vot_df$ysince_vfa < 4, 1,
                            0)
vot_df$vfa_intro_last5y <- ifelse(vot_df$ysince_vfa > 0 & vot_df$ysince_vfa < 6, 1,
                                  0)
vot_df$vfa_intro_coming3y <- dplyr::lead(vot_df$vfa_intro_last3y, 3)
vot_df$vfa_intro_coming5y <- dplyr::lead(vot_df$vfa_intro_last5y, 5)
vot_df <- subset(vot_df, year > 1959)

dat <- merge(dat, vot_df, by.x = c("ISO2", "Year"),
             by.y = c("country", "year"), all.x = TRUE)
mean(dat$Dualcit_binary)
nrow(dat)
rm(vot_df)
rm(vot)

names(dat) <- str_replace_all(names(dat), "\\.", "_")

# ---------------------------------------
# recent independence

dat <- dat %>%
  group_by(ISO3) %>%
  arrange(Year) %>%
  mutate(RecInd = ifelse(dplyr::lag(Dualcit_cat == 0) & Dualcit_cat != 0 | 
                        dplyr::lag(Dualcit_cat == 0, 2) & Dualcit_cat != 0 | 
                        dplyr::lag(Dualcit_cat == 0, 3) & Dualcit_cat != 0, 1, 0)) %>%
  ungroup()

# ----------------------------------------
# global average by per year and lagged global average
dat <- dat %>%
  group_by(Year) %>%
  mutate(mean_dual_cit_global = mean(Dualcit_binary2, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(ISO3, Year) %>%
  group_by(ISO3) %>%
  mutate(mean_dual_cit_global_lag1 = dplyr::lag(mean_dual_cit_global)) %>%
  ungroup()

# -----------------------------------------
# change in world region in the past 2 or 5 years
dat <- dat %>%
  group_by(ISO3) %>%
  arrange(ISO3, Year) %>%
  ungroup() %>%
  group_by(world_region, Year) %>%
  mutate(mean_dual_cit_region = mean(Dualcit_binary2, na.rm = TRUE),
         any_change_region = sum(change_dual_bin, na.rm = TRUE) > 0) %>%
  ungroup() %>%
  group_by(ISO3) %>%
  arrange(ISO3, Year) %>%
  mutate(change_dual_cit_regional_1_2 = ifelse(dplyr::lag(any_change_region) == TRUE | 
                                                  dplyr::lag(any_change_region, 2) == TRUE, 1, 0),
         change_dual_cit_regional_1_5 = ifelse(dplyr::lag(any_change_region) == TRUE | 
                                                 dplyr::lag(any_change_region, 2) == TRUE | 
                                                 dplyr::lag(any_change_region, 3) == TRUE | 
                                                 dplyr::lag(any_change_region, 4) == TRUE | 
                                                 dplyr::lag(any_change_region, 5) == TRUE, 1, 0)) %>%
  ungroup()


##
mean(dat$Dualcit_binary)
nrow(dat)

#save full dataset
write.csv(dat, file = "Vink_Schakel_Reichel_Chun_deGroot_2019_full.csv", row.names = FALSE,
            na = "")
write.xlsx(dat, file = "Vink_Schakel_Reichel_Chun_deGroot_2019_full.xlsx")



# -------------------------------------------------------------------
# change to event history format
dattest <- dat
rm(dat)
names(dattest)
head(dattest)
dattest <- dattest[!is.na(dattest$Dualcit_binary2), ]
dattest <- dattest %>% 
  group_by(ISO3) %>%
  mutate(cs = cumsum(Dualcit_binary2),
         meanDualcit = mean(Dualcit_binary2)) 

dattest <- dattest %>%
  filter(meanDualcit != 1) # exclude all without changes

dattest <- dattest %>%
  group_by(ISO3) %>%
  mutate(one = lag(Dualcit_binary2)) # check dualcit in the previous year

dattest$lna <- is.na(dattest$one)
dattest$one[dattest$lna==TRUE] <- 0

dattest <- dattest %>%
  filter(one != 1) # delete where previous year was 1

# delete where previous year was NA and cit ==1
dattest$filt <- ifelse(dattest$lna==TRUE & dattest$Dualcit_binary2 == 1, 0,
                       1)
dattest <- dattest %>%
  filter(filt==1) 

# remove vars 
dattest <- select(dattest,-c(one, cs, lna, filt, meanDualcit))

### add vars
dattest <- dattest %>% 
  group_by(ISO3) %>%
  mutate(end = seq_along(Year),
         start = end - 1,
         no_years = max(end)) %>%
  ungroup()

# var names to lower
nms <- names(dattest)
nms <- tolower(nms)
names(dattest) <- nms

## create logged variables
dattest$log_sp_pop_totl <- log(dattest$sp_pop_totl)
dattest$log_ny_gdp_pcap_cd <- log(dattest$ny_gdp_pcap_cd)

#write file
write.csv(dattest, file = "Vink_Schakel_Reichel_Chun_deGroot_2019_main.csv", row.names = FALSE,
          na = "")
write.xlsx(dattest, file = "Vink_Schakel_Reichel_Chun_deGroot_2019_main.xlsx")


# ----------------------------------------------------------
# change to EHA
# excluding backsliding countries
dat <- dattest
rm(dattest)

dat <- dat %>%
  group_by(iso3) %>%
  mutate(mean_dual_self = mean(dualcit_binary2, na.rm = TRUE)) %>%
  ungroup()

dat$world_region2 <- ifelse(dat$world_region == 1, "Africa",
                            ifelse(dat$world_region == 2, "Asia",
                                   ifelse(dat$world_region == 3, "Europe",
                                          ifelse(dat$world_region == 6, "Oceania",
                                                 ifelse(dat$world_region == 4 | dat$world_region == 5,
                                                        "America", NA)))))

# - exclude backsliding countries.
bsls <- c("NGA", "LSO", "ARG", "PRY", "DOM", "VEN", "KAZ", "SVK",
          "BIH", "EST", "LTU", "FJI")
dat$ind <- (dat$iso3 %in% bsls)

dat <- subset(dat, ind == FALSE)
dat <- dplyr::select(dat, -ind, -mean_dual_self)

#write file excl back-sliding
write.csv(dat, file = "Vink_Schakel_Reichel_Chun_deGroot_2019_excl_backsliding_countries.csv", row.names = FALSE,
          na = "")
write.xlsx(dat, file = "Vink_Schakel_Reichel_Chun_deGroot_2019_excl_backsliding_countries.xlsx")
