# Libraries
install.packages(c("readstata13","plyr"))
library(readstata13)
library(plyr)

##########################
### Read in supplmentary data
##########################

party_election_dat <- read.csv("Data_supplementary.csv",stringsAsFactors = F)

#######################
### Add information on two party versus multiparty systems
#######################

# Read in data from ParlGov
elect_dat <- read.csv("view_election.csv")
elect_param_dat <- read.csv("viewcalc_election_parameter.csv")

# Merge the two datasets
temp <- merge(elect_dat, elect_param_dat, by.x="election_id")

# Delete countries that are not in the supplementary dataset
temp <- temp[which(temp$country_name_short %in% party_election_dat$country_name),]

# Create election year variable
temp$year <- as.numeric(substr(temp$election_date, 1, 4))

# Create country_year variable
temp$country_year <- paste(temp$country_name_short, temp$year, sep="_")

# Create enp variable in party_election_dat
party_election_dat$enp_votes <- as.numeric(mapvalues(party_election_dat$country_year_cses, temp$country_year, temp$enp_votes))

# Create variable for number of leftist parties
left_num <- by(party_election_dat$lr_lib_agr, party_election_dat$country_year_cses, function(x){sum(x=="l",na.rm=T)})
party_election_dat$left_num <- as.numeric(mapvalues(party_election_dat$country_year_cses, names(left_num), as.vector(left_num)))

#######################
### Organize campaign data from CCDD
#######################

# Read in the data
self <- read.dta13("Self_03oct2016.dta")
other <- read.dta13("Other_03oct2016.dta")
journ <- read.dta13("Journalist_03oct2016.dta")

# Delete subject column in other. Then change other column to subject.
other <- other[,-11]
colnames(other)[11] <- "subject"

# Add variable indicating data type
self <- cbind(type="self",self)
other <- cbind(type="other",other)
journ <- cbind(type="journ",journ)

# Combine the three datasets.  
all <- rbind(self, other, journ)

# Create variable for country_year
all$country_year <- paste(all$country, all$year_month, sep="_")

# Create variable for country_year_party
all$country_year_party <- paste(all$country_year, all$subject, sep="_")

#######################
### Create variable for total statements in party_election_dat
#######################

party_election_dat$total_counts <- as.numeric(mapvalues(party_election_dat$country_year_party, names(table(all$country_year_party)), table(all$country_year_party)))

######################
### Organize party_election_dat
#####################

party_election_dat <- subset(party_election_dat, partyname_filled!="Fidesz")

######################
### Create ideology variables
#####################

# Without special issue and ethnic regional parties.
party_election_dat$lr_lib_agr <- as.factor(party_election_dat$lr_lib_agr)
party_election_dat$lr_lib_agr <- relevel(party_election_dat$lr_lib_agr, "r")

# Without special issue and ethnic regional parties. Liberal parties classified based on rile score.
party_election_dat$lr <- party_election_dat$lr_lib_agr
party_election_dat$lr[which(party_election_dat$parfam_subject=="lib liberal")] <- ifelse(party_election_dat$originalCMP_rile_inverted[which(party_election_dat$parfam_subject=="lib liberal")]>0, "l", "r")

######################
### Create moderation variable
#####################

# Subtract rile in t-1 from rile in t. Then multiply family_AdamsSomerTopcu variable. 
party_election_dat$moderation <- (party_election_dat$originalCMP_rile - party_election_dat$rile_prev)*party_election_dat$family_AdamsSomerTopcu

######################
### Read in the Chapel Hill dataset and add the ideology score to party_election_dat
#####################

# Read in the data
ches <- read.csv("1999-2014_CHES_dataset_means.csv",stringsAsFactors=F)

# Reduce ches data to relevant countries
ches <- subset(ches, country=="cz"|country=="ge"|country=="dk"|country=="esp"|country=="uk"|country=="hun"|country=="nl"|country=="pol"|country=="por"|country=="sv")

# Create ches-like country variable in party_election_dat
party_election_dat$country_ches <- mapvalues(party_election_dat$country_name, c("CZE" ,"DEU", "DNK", "ESP" ,"GBR" ,"HUN", "NLD", "POL" ,"PRT" ,"SWE"), c("cz","ge","dk","esp", "uk", "hun", "nl", "pol", "por", "sv"))

# Create year of election variable in party_election_dat
party_election_dat$year <- substr(party_election_dat$country_year_cses, 5,8)

# Combine cmp code and election year. Use that to assign lrgen to party_election_dat
party_election_dat$cmp_year <- paste(party_election_dat$CMPcode, party_election_dat$year,sep="_")
ches$cmp_year <- paste(ches$cmp_id, ches$electionyear,sep="_")
ches$lrgen[which(ches$lrgen == "center")] <- 5
ches$lrgen[which(ches$lrgen == "extreme right")] <- 10
ches$lrgen <- as.numeric(ches$lrgen)
party_election_dat$lrgen <- as.numeric(mapvalues(party_election_dat$cmp_year, ches$cmp_year, ches$lrgen))
party_election_dat$lrgen_inverted <- -party_election_dat$lrgen

######################
### Create binary left-right variable based on ches.
#####################

# Create lr_ches variable.
party_election_dat$lr_ches <- party_election_dat$lrgen_inverted
party_election_dat$lr_ches[party_election_dat$lrgen_inverted >= -5] <- "l"
party_election_dat$lr_ches[party_election_dat$lrgen_inverted < -5] <- "r"

# Make r the base.
party_election_dat$lr_ches <- as.factor(party_election_dat$lr_ches)
party_election_dat$lr_ches <- relevel(party_election_dat$lr_ches, ref="r")

######################
### Save the data.
#####################

study1a <- party_election_dat
study1a <- study1a[,c("parfam_subject","country","year","country_year","country_year_party","neg_share_all_other_only","pervote_change","lr","total_counts","originalCMP_rile_inverted","CMPcode","lr_ches","lrgen_inverted","votet1","lastgovt","pm_party","gdp","moderation","niche","enp_votes","left_num")]
save(study1a, file="Study1a.RData")
