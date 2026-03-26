# Load libraries
install.packages(c("plyr","manifestoR","quanteda","readstata13"))
library(plyr)
library(manifestoR)
library(quanteda)
library(readstata13)

# Set up manifestoR. Type in the text file for your API key in the mp_setapikey function.
mp_load_cache(file="cache.RData")
mp_setapikey("manifesto_apikey.txt")
mpd <- mp_maindataset()

# Create empty object for data collection
data_collection <- NULL

# Start the for loop for each country:

for(c  in c("Australia", "Canada", "Ireland", "New Zealand", "United Kingdom", "United States")){
  
# Download the entire corpus
corp <- mp_corpus(countryname == c)

#####################
##### Reduce to annotated manifestos
#####################

corp_cleaned <- corp

# Identify manifestos not annotated at all
take_out <- NULL
for(i in 1:length(corp_cleaned)){
  take_out <- c(take_out, corp_cleaned[[i]]$meta$annotations) # If FALSE, not annotated
}

# If all manifestos are annotated, the below chunk of code is skipped. If at least one manifesto is not annotated, the code takes out the unannotated manifestos.
if(length(which(!take_out))!=0){
  corp_cleaned <- corp_cleaned[-which(!take_out)]
}

#####################
#### Reduce to English manifestos
#####################

# Check if all manifestos are in English.
english <- NULL
for(i in 1:length(corp_cleaned)){
  english <- c(english, corp_cleaned[[i]]$meta$language=="english")
}

# For Canada, check that the manifestos that aren't marked as English are really not English.
if(c=="Canada"){
  english[3] <- TRUE
}

# If all manifestos are in English, the below chunk of code is skipped. If at least one manifesto is not english, the code takes out the non-English manifestos.
if(length(which(!english))!=0){
  corp_cleaned <- corp_cleaned[-which(!english)]
}

#####################
##### Create dataset of supplementary information
#####################

# Create a dataframe with vector of parties
temp <- data.frame(party_election=names(corp_cleaned))

# Create a variable for party number, excluding election date
temp$party_number <- gsub(pattern="_.*$", replacement="", x=temp$party)

# Create a variable for election date, excluding party number
temp$election_date <- gsub(".*_","",x=temp$party_election)

# Combine party and date variables in the main dataset.
mpd$party_date <- paste(mpd$party, mpd$date, sep="_")

# Use the variable party_date to assign partyname to temp
country_code <- subset(mpd, countryname==c)[,c("countryname","partyname","party_date")]
temp$party_name <- mapvalues(temp$party_election, country_code$party_date, country_code$partyname)

# Assign country name
temp <- cbind(country_name=country_code$countryname[1],temp)

# Use the variable party_date to assign partyabbrev to temp
temp$party_abbrev <- mapvalues(temp$party_election, mpd$party_date, mpd$partyabbrev)
unique(temp$party_abbrev) # Some are empty. Replace them with NA
temp[temp==""] <- NA

# Use the variable party_date to assign parfam to temp
temp$parfam <- mapvalues(temp$party_election, mpd$party_date, mpd$parfam)
# Create variable with meaning of the numbers
temp$parfam_content <- mapvalues(temp$parfam, c(10,20,30,40,50,60,70,80,90,95,98,999), c("ECO","COM","SOC","LIB","CHR","CON","NAT","AGR","ETH","SIP","DIV",NA))

# Add party ideology at the time of the election
temp$rile <- mapvalues(temp$party_election, mpd$party_date, mpd$rile)

# Append
data_collection <- rbind(data_collection, temp)

}

##################
## Add left, right, niche information
##################

data_collection$left <- ifelse(data_collection$parfam_content=="SOC"|data_collection$parfam_content=="COM",1,0)
data_collection$right <- ifelse(data_collection$parfam_content=="CON"|data_collection$parfam_content=="LIB"|data_collection$parfam_content=="NAT"|data_collection$parfam_content=="CHR",1,0)
data_collection$niche <- ifelse(data_collection$parfam_content=="AGR"|data_collection$parfam_content=="ECO"|data_collection$parfam_content=="SIP"|data_collection$parfam_content=="ETH",1,0)

##################
## Add country-election information
##################

data_collection$country_election <- paste(data_collection$country_name, data_collection$election_date, sep=" ")

##################
## Add previous party-election information
##################

# Order the data.
data_collection <- arrange(data_collection, country_name, election_date, party_number)

# Reduce mpd to countries covered in data_collection
mpd_sub <- mpd[which(mpd$countryname %in% unique(data_collection$country_name)),]

# Split mpd_sub into list by country
mpd_sub.list <- split(mpd_sub, mpd_sub$countryname)

# Create prev_election variable in mpd_sub.list
for(i in 1:length(mpd_sub.list)){
  dates <- unique(mpd_sub.list[[i]]$date)
  lookup <- as.data.frame(cbind(date=dates,prev_date=c(NA,dates[-length(dates)])))
  mpd_sub.list[[i]]$prev_election <- mapvalues(mpd_sub.list[[i]]$date, lookup$date, lookup$prev_date)
}

# Combine back mpd_sub.list to mpd_sub
mpd_sub <- ldply(mpd_sub.list, .id=NULL)

# Order by country, date, and party
mpd_sub <- arrange(mpd_sub, countryname, date, party)

# Use the variable 'party_date' in mpd_sub to assign the previous election variable to data_collection
data_collection$prev_election <- as.numeric(as.character(mapvalues(data_collection$party_election, mpd_sub$party_date, mpd_sub$prev_election)))

#################
#### In data_collection, create variable for previous_country_election in cses format
#####################

# Create prev_election_year in data_collection
data_collection$prev_country_election <- paste(data_collection$country_name,data_collection$prev_election)

# Create prev_election_year in cses format in data_collection
data_collection$prev_country_election_cses <- mapvalues(data_collection$prev_country_election, unique(data_collection$prev_country_election),c("AUS_2001","AUS_2004","AUS_2007","AUS_2010","CAN_2011","IRL_2002","IRL_2007","NZL_1981","NZL_1993","NZL_1996","NZL_1999","NZL_2002","NZL_2005","NZL_2008","GBR_1992","GBR_1997","GBR_2010","USA_1988","USA_2000","USA_2004","USA_2008"))

#################
###### Organize CLEA and use it to create enp variable
##########################

clea <- read.dta13("clea_enp_20170530_national.level.dta")

# Reduce clea to relevant countries
countries <- c(unique(clea$ctr_n[which(clea$ctr_n %in% unique(data_collection$country_name))]), "UK","US")
clea_sub <- subset(clea, ctr_n==countries[1] | ctr_n==countries[2] | ctr_n==countries[3] | ctr_n==countries[4] | ctr_n==countries[5] | ctr_n==countries[6])

# Combine ctr_n and yr in clea_sub
clea_sub$ctr_n_yr <- paste(clea_sub$ctr_n, clea_sub$yr)

# Add prev_country_election in clea format to data_collection
data_collection$prev_country_election_clea <- mapvalues(data_collection$prev_country_election_cses, unique(data_collection$prev_country_election_cses), c("Australia 2001","Australia 2004","Australia 2007","Australia 2010","Canada 2011","Ireland 2002","Ireland 2007","New Zealand 1981","New Zealand 1993","New Zealand 1996","New Zealand 1999","New Zealand 2002","New Zealand 2005","New Zealand 2008","UK 1992","UK 1997","UK 2010","US 1988","US 2000","US 2004","US 2008"))

# Reduce clea_sub to the relevant elections
clea_sub <- clea_sub[which(clea_sub$ctr_n_yr %in% unique(data_collection$prev_country_election_clea)),]

# Assign enp_nat in clea_sub to data_collection using ctr_n_yr
data_collection$prev_enp_nat <- mapvalues(data_collection$prev_country_election_clea, clea_sub$ctr_n_yr, clea_sub$ENP_nat)
# Turn -990 to NA
data_collection$prev_enp_nat[data_collection$prev_enp_nat=="-990"] <- NA
data_collection$prev_enp_nat <- as.numeric(data_collection$prev_enp_nat)

# Create country_election_cses variable
data_collection$country_election_cses <- mapvalues(data_collection$country_election, unique(data_collection$country_election),c("AUS_2004","AUS_2007","AUS_2010","AUS_2013","CAN_2015","IRL_2007","IRL_2011","NZL_1984","NZL_1996","NZL_1999","NZL_2002","NZL_2005","NZL_2008","NZL_2011","GBR_1997","GBR_2001","GBR_2015","USA_1992","USA_2004","USA_2008","USA_2012"))

# Organize
data_collection$election_date <- as.numeric(data_collection$election_date)
data_collection$party_number <- as.numeric(data_collection$party_number)
data_collection$parfam <- as.numeric(as.character(data_collection$parfam))
data_collection$rile <- as.numeric(as.character(data_collection$rile))
data_collection$country_name <- as.character(data_collection$country_name)
data_collection$party_election <- as.character(data_collection$party_election)
data_collection$party_name <- as.character(data_collection$party_name)
data_collection$party_abbrev <- as.character(data_collection$party_abbrev)
data_collection$parfam_content <- as.character(data_collection$parfam_content)

# Save the data
write.csv(data_collection, file="Data_supplementary.csv",row.names=F)


