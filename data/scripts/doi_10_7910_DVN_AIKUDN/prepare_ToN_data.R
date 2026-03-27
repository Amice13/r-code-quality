rm(list=ls())

install.packages("mamlr")
library(mamlr)
install.packages("readxl")
library(readxl)
install.packages("foreign")
library(foreign)


################################################################################
####################### Prepare Talk of Norway data  ###########################
########################### plus other meta data  ##############################
################################################################################



### Load ToN data, select data from matched MPs, aggregate and expand dataset###

# Load ToN data (text column removed, for efficiency)
ton <-readRDS("ToN_noTxt.rds")

# Load list of MPs that have been matched with MPs in MaML data
mps_matched <- read_xlsx("mps_matched_ton_and_maml.xlsx")

# Join matched MPs with ToN data
ton <- inner_join(ton, dplyr::select(mps_matched, c("actorId", "url_rep_id")), by=c("url_rep_id"))

# Keep speeches from MPs and drop speeches from suppleants
ton <- ton[grepl('Representant',ton$speaker_role),]
ton <- ton[!grepl('Vararepresentant',ton$rep_type),]
# Drop speeches prior to our period
ton <- ton[!grepl('199',ton$yearmonth),]

# Recode party id
ton$party_id[ton$party_id=="FrP"] <- "P_351"
ton$party_id[ton$party_id=="Sp"] <- "P_702"
ton$party_id[ton$party_id=="A"] <- "P_104"
ton$party_id[ton$party_id=="KrF"] <- "P_1538"
ton$party_id[ton$party_id=="H"] <- "P_1435"
ton$party_id[ton$party_id=="SV"] <- "P_81"
ton$party_id[ton$party_id=="V"] <- "P_647"
ton$party_id[ton$party_id=="TF"] <- "P_OTHERS"
ton$party_id[ton$party_id=="Kp"] <- "P_780"
ton$party_id[ton$party_id=="MDG"] <- "P_2254"

# Rename actor variable
colnames(ton)[colnames(ton)=="actorId"] <- "actor"


# Create unique list of MPs X periods 
ton_mpsXperiod <- ton %>%
  dplyr::select(actor, rep_from, rep_to, parl_period, party_id) %>%
  group_by(actor, parl_period) %>%
  summarise(rep_from = first(rep_from), rep_to = first(rep_to), party_id = first(party_id))


# Expand dataset: create dataframe of all mp X months based on rep_from and rep_to
mps_allmonths <- data.frame()

for(i in 1:nrow(ton_mpsXperiod)) {
  df_mp <- ton_mpsXperiod[i,]
  mp_dates <- data.frame(values = seq(from = as.Date(df_mp$rep_from), to = as.Date(df_mp$rep_to), by = 'month')) %>%
    mutate(actor = df_mp$actor, parl_period = df_mp$parl_period, party_id = df_mp$party_id,
           legspeech =0
    )
  mps_allmonths <- bind_rows(mps_allmonths,mp_dates)
}

# Create yearmonth and month variable
mps_allmonths <- mps_allmonths %>%
  mutate(
    year = strftime(`values`, format = '%Y'),
    yearmonth = strftime(mps_allmonths$`values`, format = '%Y%m'),
    month = strftime(mps_allmonths$`values`, format = '%m')
    
  )

# Collapse mps by actorId and month, and count number of legislative speeches 
ton_mp_X_month <- ton %>%
  dplyr::select(actor, yearmonth, party_id) %>%
  group_by(actor, yearmonth) %>%
  summarise(legspeech = n(), party_id = first (party_id))

# Join mpsXmonth with mps_allmonths
ton_mp_X_month <- full_join(ton_mp_X_month, dplyr::select(mps_allmonths, -c("legspeech"))
                             , by=c("actor", "yearmonth", "party_id"))

# Replace legspeech NAs with 0 (because these NAs are the months where the MPs had a seat but did not speak in parliament)
ton_mp_X_month$legspeech[is.na(ton_mp_X_month$legspeech)] <- 0

# Drop variables
ton_mp_X_month <- subset(ton_mp_X_month, select = -c(values))

# Reformat variable
ton_mp_X_month$yearmonth <- as.numeric(ton_mp_X_month$yearmonth)
################################################################################




### Extract committee and list position from ToN data ##########################
# Create MP X period level metadata from ToN data
ton_meta <- ton %>%
  dplyr::select(actor, parl_period, county, list_number, com_member, com_role) %>%
  group_by(actor, parl_period) %>%
  summarise(county = first(county), list_number= first(list_number), com_member=first(com_member), com_role=first(com_role))

# Recode committee role to committee leader dummy
ton_meta$comt_leader <- ton_meta$com_role
ton_meta$comt_leader <- str_extract(ton_meta$comt_leader, "leder")
ton_meta[c("comt_leader")][is.na(ton_meta[c("comt_leader")])] <- "medlem"

# Reduce to one committee and dropping non-policy related committees
ton_meta$com_one <- ton_meta$com_member
ton_meta$com_one <- trimws(ton_meta$com_one)
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "Valgkomiteen ;")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "; Valgkomiteen")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "Arbeidsordningskomiteen ;")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "; Arbeidsordningskomiteen")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "Fullmaktskomiteen ;")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "; Fullmaktskomiteen")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "Den forberedende fullmaktskomité ;")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "; Den forberedende fullmaktskomité")
ton_meta$com_one  <- str_remove_all(ton_meta$com_one, "Kontroll- og konstitusjonskomiteen ;")
ton_meta$com_first <- sub("\\;.*", "", ton_meta$com_one)
ton_meta$com_first <- trimws(ton_meta$com_first)
# recode fullmaktskomiteen til kontroll- og konstitusjonskomiteen
ton_meta$com_first  <- str_replace(ton_meta$com_first, "Fullmaktskomiteen", "Kontroll- og konstitusjonskomiteen")

# Load committee list with soft / hard categorization
committee_codes <- read_xlsx("committees.xlsx")

# Join with ton meta data
ton_meta <- left_join(ton_meta, committee_codes, by = c("com_first"))

# Drop variables
ton_meta <- subset(ton_meta, select = -c(com_member, com_role, com_one, com_first, com_no))

# Join with monthly dataset
ton_mp_X_month <- left_join(ton_mp_X_month, ton_meta, by=c("actor", "parl_period"))
################################################################################




### Create relative list position variable #####################################
# Collapse by parl_period and partyId and county
list_number_minmax <- ton %>%
  dplyr::select(party_id, parl_period, county, list_number) %>%
  group_by(party_id, parl_period, county) %>%
  summarise(list_number_min= min(list_number), list_number_max = max(list_number))

# Join list min and max with monthly dataset
ton_mp_X_month <- left_join(ton_mp_X_month, list_number_minmax, by = c("party_id", "parl_period", "county"))

# Create relative list position measure
ton_mp_X_month$list_number_relative <- ton_mp_X_month$list_number/ton_mp_X_month$list_number_max
# Invert the measure
ton_mp_X_month$list_number_relative_inv <- ton_mp_X_month$list_number_relative * -1
table(ton_mp_X_month$list_number_relative_inv)

# Drop variables
ton_mp_X_month <- subset(ton_mp_X_month, select = -c(list_number_min, list_number_max, list_number_relative, list_number))
################################################################################




### Add relative distance from capital #########################################
# Load distance data
counties <- read_xlsx("counties.xlsx")
# Join with monthly dataset
ton_mp_X_month <- left_join(ton_mp_X_month, counties, by = c("county"))
################################################################################



### Add constituency size etc ##################################################
# Load constituency data
const <- read_xlsx("constituency_size.xlsx")
# Join with monthly dataset
ton_mp_X_month <- left_join(ton_mp_X_month, const, by = c("county_no", "parl_period"))
################################################################################



### Drop mps that are party leaders ############################################
partyleaders_no <- read_xlsx("partyleaders_no.xlsx")

# Create dataframe of all partyleaders X months based on start and end date
partyleaders_allmonths <- data.frame()

for(i in 1:nrow(partyleaders_no)) {
  df_pl <- partyleaders_no[i,]
  pl_dates <- data.frame(values = seq(from = as.Date(df_pl$startDate), to = as.Date(df_pl$endDate), by = 'month')) %>%
    mutate(actor = df_pl$actor, 
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

# Join with MP data and drop rows where partyleader=1
partyleaders_allmonths <- dplyr::select(partyleaders_allmonths, -c('values', 'year', 'month'))
partyleaders_allmonths$yearmonth <- as.numeric(partyleaders_allmonths$yearmonth)
ton_mp_X_month <- left_join(ton_mp_X_month, partyleaders_allmonths, by=c("actor", "yearmonth"))
ton_mp_X_month$partyleader[is.na(ton_mp_X_month$partyleader)] <- 0
ton_mp_X_month <- ton_mp_X_month[(ton_mp_X_month$partyleader==0),]
ton_mp_X_month <- dplyr::select(ton_mp_X_month, -c('partyleader'))
################################################################################



### Save and export to dta #####################################################
# saveRDS(ton_mp_X_month, file = 'ton_mp_X_month.rds')
write.dta(ton_mp_X_month, "ton_mp_X_month.dta")
################################################################################


