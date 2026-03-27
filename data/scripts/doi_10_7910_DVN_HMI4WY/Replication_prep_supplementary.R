# Libraries
install.packages(c("readstata13","plyr"))
library(readstata13)
library(plyr)

#######################
### Read in and organize the "other" campaign data.
#######################

# Read in the data.
other <- read.dta13("Other_03oct2016.dta")

# Reduce the dataset to Val and IssueVal in statement_type
other <- subset(other, statement_type=="OtherVal" | statement_type=="OtherIssueVal")

# We don't need columns subject, valen_issue, socialPol, socialPol_spend_dir, dominant_issue, dominant_issue_social. 
# And change other column to subject.
other <- other[,!(colnames(other) %in% c("subject","valen_issue", "socialPol", "socialPol_spend_dir", "dominant_issue", "dominant_issue_social"))]
colnames(other)[11] <- "subject"

# Add variable indicating data type
other <- cbind(type="other",other)

# Create country_year variable
other$country_year <- paste(other$country, other$year_month, sep="_")

# Create country_year_party variable
other$country_year_party <- paste(other$country_year, other$subject, sep="_")

##########################
### For each subject in each country_year, create variables for the total occurrence of positives, negatives, neutrals, and contradictions in the direction variable.
##########################

positive_counts_other_only <- by(other, other$country_year_party, function(x){sum(x$direction==1,na.rm=T)})
positives_other_only <- data.frame(country_year_party=names(positive_counts_other_only), positive_counts_other_only=as.vector(positive_counts_other_only), stringsAsFactors=F)

negative_counts_other_only <- by(other, other$country_year_party, function(x){sum(x$direction==-1,na.rm=T)})
negatives_other_only <- data.frame(country_year_party=names(negative_counts_other_only), negative_counts_other_only =as.vector(negative_counts_other_only), stringsAsFactors=F)

neutral_counts_other_only <- by(other, other$country_year_party, function(x){sum(x$direction==0,na.rm=T)})
neutrals_other_only <- data.frame(country_year_party=names(neutral_counts_other_only), neutral_counts_other_only =as.vector(neutral_counts_other_only), stringsAsFactors=F)

contradict_counts_other_only <- by(other, other$country_year_party, function(x){sum(is.na(x$direction))})
contradicts_other_only <- data.frame(country_year_party=names(contradict_counts_other_only), contradict_counts_other_only =as.vector(contradict_counts_other_only), stringsAsFactors=F)

##########################
### Combine the count variables into a dataset
##########################

counts_other_only.df <- data.frame(country_year_party=positives_other_only[,1],pos_counts_other_only=positives_other_only[,2],neg_counts_other_only=negatives_other_only[,2], neutral_counts_other_only=neutrals_other_only[,2], contradict_counts_other_only=contradicts_other_only[,2], stringsAsFactors=F)

counts.df <- counts_other_only.df

###################
### Combine counts.df and Somer-Topcu et al. (2017)'s data
###################

# Read in Somer-Topcu et al.'s data
position <- read.dta13("merged_data_18nov2016.dta")

# Create country_year_party variable
position$country_year_party <- paste(position$country, position$year, position$subject, sep="_")
position <- position[,-which(colnames(position)%in%"year")]

# Combine the two by country_year_party
merged <- merge(counts.df, position, all=T)

###################
### Create the valence attack variable in merged data
###################

# Create variable for share of negative counts by other parties. Out of all mentions.
merged$neg_share_all_other_only <- merged$neg_counts_other_only/(merged$pos_counts_other_only+merged$neg_counts_other_only+merged$neutral_counts_other_only+merged$contradict_counts_other_only)

##########################
### Create the outcome variable
##########################

# Create variable for change in vote share using pervote-votet1.
merged$pervote_change <- NA
merged[complete.cases(merged[,c("pervote","votet1")]),"pervote_change"] <- merged[complete.cases(merged[,c("pervote","votet1")]),"pervote"]  - merged[complete.cases(merged[,c("pervote","votet1")]),"votet1"]

##########################
### Create variable for party ideology and niche status
##########################

merged$parfam_subject <- droplevels(merged$parfam_subject)

# Create variable that includes agrarian parties as right wing.
merged$lr_lib_agr <- mapvalues(merged$parfam_subject, c(   "eco ecologist"    ,      "com communist"    ,     
                                                           "soc social democratic",  "lib liberal"       ,    
                                                           "chr christian democrat", "con conservative"   ,   
                                                           "nat nationalist"      ,  "agr agrarian"        ,  
                                                           "eth ethnic-regional"   , "sip special issue"     ), c("l","l","l","r","r","r","r","r",NA,NA))

# Code green, sip, eth, and agr parties as niche. 
merged$niche <- mapvalues(merged$parfam_subject,  c(   "eco ecologist"    ,      "com communist"    ,     
                                                                  "soc social democratic",  "lib liberal"       ,    
                                                                  "chr christian democrat", "con conservative"   ,   
                                                                  "nat nationalist"      ,  "agr agrarian"        ,  
                                                                  "eth ethnic-regional"   , "sip special issue"     ), c(1,0,0,0,0,0,0,1,1,1))

###################
### Create index variables
###################

# Create country_year variable
merged$country_year <- substr(merged$country_year_party, 1, 7)

# Create country_year variable in cses format
merged$country_year_cses <- mapvalues(merged$country_year, c( "CZ_2010", "CZ_2013", "DE_2009", "DE_2013", "DK_2007", "DK_2011",
                                                              "ES_2008" ,"ES_2011" ,"HU_2006", "HU_2010" ,"NL_2010", "NL_2012",
                                                              "PL_2007", "PL_2011" ,"PO_2009" ,"PO_2011", "SV_2010", "SV_2014",
                                                              "UK_2005", "UK_2010", "UK_2015")
                                      ,c( "CZE_2010", "CZE_2013", "DEU_2009", "DEU_2013", "DNK_2007", "DNK_2011",
                                          "ESP_2008" ,"ESP_2011" ,"HUN_2006", "HUN_2010" ,"NLD_2010", "NLD_2012",
                                          "POL_2007", "POL_2011" ,"PRT_2009" ,"PRT_2011", "SWE_2010", "SWE_2014",
                                          "GBR_2005", "GBR_2010", "GBR_2015"))

# Create country_name variable
merged$country_name <- substr(merged$country_year_cses, start=1, stop=3)

# Create subject variable
merged$subject_from_original_dat <- as.numeric(substr(merged$country_year_party, 9, nchar(merged$country_year_party)))

###################
### Invert the rile scale
###################

merged$originalCMP_rile_inverted <- -(merged$originalCMP_rile)

#################
### Fill in missing information in partyname
#####################

# Reorder merged
merged <- arrange(merged, country_year_cses, subject_from_original_dat)

# Add in missing information
merged$partyname_filled <- merged$partyname
merged$partyname_filled[which(is.na(merged$partyname_filled))] <- c( "The government","Public Affairs","The government","The government","The government","The government","The government",NA,"Government","Fidesz","Fidesz-KDNP alliance","MSZP","MIEP-Jobbik","SZDSZ","MDF", "The government", "Fidesz", "KDNP","Fidesz-KDNP alliance","MSZP", "MIEP-Jobbik", "SZDSZ","MDF", "LMP" ,"The government", "The government", "The government", "The government","The government","The government","The government")

#####################
### Save the merged data in csv format.
#####################

# write.csv(merged, file="Data_supplementary.csv",row.names=F)

#####################
### Hand-fill in values that are missing.
#####################

# To the csv file, we handfilled in missing values in votet1, pervote_change, originalCMP_rile_inverted, parfam_subject, lr_lib_agr, CMPcode, partyname, country, rile, and lastgovt columns.

#####################
### Create additional variables
#####################

# We created last_election, lagged_dv, pm_party, gdp, rile_prev, and family_AdamsSomerTopcu variables. The pm_party variable is from the cabinet dataset of ParlGov: view_cabinet.csv (version May 1, 2018). For the gdp variable, we collected economic performance data in time t using gdp per capita growth rate from the World Bank. For the rile_prev variable, we collected information on party's rile score from the previous election using the Manifesto Project Dataset: MPDataset_MPDS2018b.csv. For the family_AdamsSomerTopcu variable, following Adams and Somer-Topcu (2009), we created a variable indicating 1 if left-wing and -1 if right-wing, based on party family information. Left includes social democratic, green, and communist. Right includes nationalist, Christian Democratic, agrarian, and conservative. Liberal parties were coded left if rile score is below 0 and right if 0 or above.

#################
### Create lookup files, which will be used to match parties in the campaign dataset with party codes in the election studies, for both previous and current vote choice.
#####################

# Using merged, we deleted rows that are not relevant (i.e., observations with NA in subject column and observations without data) and left only the country_year_cses, subject, and partyname columns. We then created cses_party_num, cses_party_letter, prev_election, prev_cses_party_num, and prev_cses_party_letter columns in Lookup_party.csv and vote_choice_party_code and id_party_code columns in Lookup_party_non_cses.csv.

