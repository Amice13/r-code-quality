rm(list=ls())

install.packages("mamlr")
library(mamlr)
install.packages("readxl")
library(readxl)
install.packages("foreign")
library(foreign)


################################################################################
####################### Prepare ParlSpeech data UK  ############################
########################### plus other meta data  ##############################
################################################################################



### Load ParlSpeech data, add MP ids based on name matching ####################

# Load parlspeech data (text column removed, for efficiency)
parlspeech <-readRDS("parlspeech_notxt.rds")

# Load list of MPs (MP X parliamentary period)
mps <- read_xlsx("mps_uk.xlsx")
# Aggregate by MP
mps_unique <- mp_X_period %>%
  dplyr::select(actorId, firstName, middleNames, lastName, partyId) %>%
  group_by(actorId) %>%
  summarise(lastName = first(lastName), firstName = first(firstName), middleNames = first(middleNames), partyId=first(partyId))

# Remove first and last character from middle name
mps_unique$middleNames <- str_sub(mps_unique$middleNames, 2, -2)
# Paste first, middle and last name
mps_unique$name <- paste(mps_unique$firstName, mps_unique$middleNames, mps_unique$lastName, sep = " ")
mps_unique$name <- str_remove(mps_unique$name, " NA")

# Replace "-" with " " in speaker column of parlspeech data
parlspeech$speaker <- str_replace(parlspeech$speaker, "-", " ")

# Add MP id (actorId) and party id to parlspeech data, based on name match
parlspeech <- left_join(parlspeech, mps_unique, by = c("speaker"="name"))

# Drop df
rm(mps_unique)
################################################################################




### Aggregate and expand dataset################################################ 

# Create unique list of MPs X periods with start and end date
mpsXperiod <- mps %>%
  dplyr::select(actorId, startDate, endDate, parlPeriod, partyId) %>%
  group_by(actorId, parlPeriod) %>%
  summarise(rep_from = first(startDate), rep_to = first(endDate), partyId=first(partyId))

# Create dataframe of all mp X months based on rep_from and rep_to
mps_allmonths <- data.frame()

for(i in 1:nrow(mpsXperiod)) {
  df_mp <- mpsXperiod[i,]
  mp_dates <- data.frame(values = seq(from = as.Date(df_mp$rep_from), to = as.Date(df_mp$rep_to), by = 'month')) %>%
    mutate(actor = df_mp$actorId, party_id = df_mp$partyId,
           legspeech =0
    )
  mps_allmonths <- bind_rows(mps_allmonths,mp_dates)
}

# Create date grouping variables
mps_allmonths <- mps_allmonths %>%
  mutate(
    year = strftime(`values`, format = '%Y'),
    yearmonth = strftime(mps_allmonths$`values`, format = '%Y%m'),
    month = strftime(mps_allmonths$`values`, format = '%m')
  )

# Remove years outside our period
mps_allmonths$year <- as.numeric(mps_allmonths$year)
mps_allmonths <- mps_allmonths[(mps_allmonths$year<2016),]
mps_allmonths <- mps_allmonths[(mps_allmonths$year>1999),]


# Collapse the parlspeech mps by actorId and month
colnames(parlspeech)[colnames(parlspeech)=="actorId"] <- "actor"
parlspeech_mp_X_month <- parlspeech %>%
  dplyr::select(actor, yearmonth) %>%
  group_by(actor, yearmonth) %>%
  summarise(legspeech = n())

# Drop actorid NAs (remaining from non-matched names)
parlspeech_mp_X_month <- parlspeech_mp_X_month %>% drop_na("actor")

# Join mpsXmonth with mps_allmonths
parlspeech_mp_X_month <- full_join(parlspeech_mp_X_month, dplyr::select(mps_allmonths, -c("legspeech", "values"))
                             , by=c("actor", "yearmonth"))

# Drop duplicates (resulting from full_join)
parlspeech_mp_X_month <-parlspeech_mp_X_month[!duplicated(parlspeech_mp_X_month), ]

# Drop party_id NAs (small number of observations)
parlspeech_mp_X_month <- parlspeech_mp_X_month %>% drop_na("party_id")

# Replace legspeech NAs with 0 (because these NAs are the months where the MPs had a seat but did not speak in parliament)
parlspeech_mp_X_month$legspeech[is.na(parlspeech_mp_X_month$legspeech)] <- 0

# Reformat variable
parlspeech_mp_X_month$yearmonth <- as.numeric(parlspeech_mp_X_month$yearmonth)

# Drop month with close to no parliamentary activity (August)
parlspeech_mp_X_month <-parlspeech_mp_X_month[!grepl('08', parlspeech_mp_X_month$month),]
################################################################################




### Drop mps that are party leaders ############################################
partyleaders_uk <- read_xlsx("partyleaders_uk.xlsx")

# create dataframe of all partyleaders X months based on rep_from and rep_to
partyleaders_allmonths <- data.frame()

for(i in 1:nrow(partyleaders_uk)) {
  df_pl <- partyleaders_uk[i,]
  pl_dates <- data.frame(values = seq(from = as.Date(df_pl$startDate), to = as.Date(df_pl$endDate), by = 'month')) %>%
    mutate(actor = df_pl$actorId, 
           partyleader = 1
    )
  partyleaders_allmonths <- bind_rows(partyleaders_allmonths,pl_dates)
}

# Create date grouping variables
partyleaders_allmonths <- partyleaders_allmonths %>%
  mutate(
    year = strftime(`values`, format = '%Y'),
    yearmonth = strftime(partyleaders_allmonths$`values`, format = '%Y%m'),
    month = strftime(partyleaders_allmonths$`values`, format = '%m')
  )

# Join with mp data and drop rows where partyleader=1
partyleaders_allmonths <- dplyr::select(partyleaders_allmonths, -c('values', 'year', 'month'))
partyleaders_allmonths$yearmonth <- as.numeric(partyleaders_allmonths$yearmonth)
parlspeech_mp_X_month <- left_join(parlspeech_mp_X_month, partyleaders_allmonths, by=c("actor", "yearmonth"))
parlspeech_mp_X_month$partyleader[is.na(parlspeech_mp_X_month$partyleader)] <- 0
parlspeech_mp_X_month <- parlspeech_mp_X_month[(parlspeech_mp_X_month$partyleader==0),]
parlspeech_mp_X_month <- dplyr::select(parlspeech_mp_X_month, -c('partyleader'))
################################################################################




### Save and export to dta #####################################################
# saveRDS(parlspeech_mp_X_month, file = 'parlspeech_mp_X_month.rds')
write.dta(parlspeech_mp_X_month, "parlspeech_mp_X_month.dta")
################################################################################

