# Libraries
install.packages(c("plyr","foreign","readstata13"))
library(plyr)
library(foreign)
library(readstata13)

#######################
#### Read in cses 4 and keep relevant elections
#######################

# Read in cses4
load("cses4.rdata")

# Keep elections covered in CCDD
cses4_sub <- subset(cses4, D1004 =="CZE_2013" | D1004=="DEU_2013" | D1004=="POL_2011" | D1004=="SWE_2014")

#######################
#### Read in the party lookup file.
#######################

# Read in the lookup file:
lookup <- read.csv("Lookup_party.csv",stringsAsFactors = F)

# Combine country_year_cses and subject
lookup$country_year_cses_subject <- paste(lookup$country_year_cses, lookup$subject, sep="_")

# Split by country_election
lookup.list <- split(lookup, lookup$country_year_cses)

#######################
#### Using the lookup, assign party number in campaign data to previous vote choice in sub-cses.
#######################

# Separate each sub-cses by country election. 
cses4_sub.list <- split(cses4_sub, cses4_sub$D1004)

# Turn irrelavent responses to NA. 89 is independent candidate. 90 is other party/candidate. 91 is none of the parties/candidates. 92 is invalid ballot. 93 is blank ballot.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$D3008_LH_PL[cses4_sub.list[[i]]$D3008_LH_PL>=89] <- NA
}

# Create variable for previous vote choice using campaign data's party number
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$prev_vote_choice_cdd <- mapvalues(cses4_sub.list[[i]]$D3008_LH_PL, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$country_year_cses_subject)
}

# Assign everything in two digits or less to NA
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$prev_vote_choice_cdd[cses4_sub.list[[i]]$prev_vote_choice_cdd<=99] <- NA
  print(unique(cses4_sub.list[[i]]$prev_vote_choice_cdd))
}

#######################
#### Determine whether respondent voted for party he/she previously voted for.
#######################

# 89 is independent candidate. 90 is other party/candidates. 91 is none of the parties/candidates. 92 is invalid ballot. 93 is blank ballot. 97 is refused. 98 is don't know. 99 is missing. Code 97, 98, and 99 as NA.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$D3006_LH_PL[cses4_sub.list[[i]]$D3006_LH_PL==97|cses4_sub.list[[i]]$D3006_LH_PL==98|cses4_sub.list[[i]]$D3006_LH_PL==99] <- NA
}

# Assign 100 to D3006_LH_PL to voters who have 5 for D3005_LH. It indicates voters who did not vote.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$D3006_LH_PL[cses4_sub.list[[i]]$D3005_LH==5] <- 100
}
# Turn observations in current vote choice that has values 6, 7, 8, or 9 in voted or not to NA. These observations will not be analyzed because it is not clear whether they voted or not.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$D3006_LH_PL[cses4_sub.list[[i]]$D3005_LH>=6] <- NA
}

# Create variable for vote choice using campaign data's party number
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$vote_choice_cdd <- mapvalues(cses4_sub.list[[i]]$D3006_LH_PL, lookup.list[[i]]$cses_party_num, as.character(lookup.list[[i]]$country_year_cses_subject))
  print(unique(cses4_sub.list[[i]]$vote_choice_cdd))
}

# If prev_vote_choice_cdd and vote_choice_cdd are the same, assign 1. If not, assign 0.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$loyal_vote <- ifelse(cses4_sub.list[[i]]$prev_vote_choice_cdd==cses4_sub.list[[i]]$vote_choice_cdd,1,0)
}

#######################
#### Create self-placement measure
#######################

# Turn anything larger than 10 to NA
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$ideo <- cses4_sub.list[[i]]$D3014
  cses4_sub.list[[i]]$ideo[cses4_sub.list[[i]]$ideo > 10] <- NA
}

#######################
#### Create distance variable
#######################

# Create a variable indicating party's perceived position.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$cses_party_letter <- mapvalues(cses4_sub.list[[i]]$prev_vote_choice_cdd, lookup.list[[i]]$country_year_cses_subject, lookup.list[[i]]$cses_party_letter)
  print(unique(cses4_sub.list[[i]]$cses_party_letter))
}
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$cses_party_position <- paste("D3013",cses4_sub.list[[i]]$cses_party_letter,sep="_")
  print(unique(cses4_sub.list[[i]]$cses_party_position))
}
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$cses_party_position[which(nchar(cses4_sub.list[[i]]$cses_party_position)!=7)] <- NA
  print(unique(cses4_sub.list[[i]]$cses_party_position))
}
for(i in names(cses4_sub.list)){
  for(j in 1:nrow(cses4_sub.list[[i]])){
    if(!is.na(cses4_sub.list[[i]][j,"cses_party_position"])){
      cses4_sub.list[[i]][j,"cses_party_position"] <- cses4_sub.list[[i]][j,cses4_sub.list[[i]]$cses_party_position[j]]
    }
  }
}

# Turn 95 and above to NA.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$cses_party_position[which(cses4_sub.list[[i]]$cses_party_position>=95)] <- NA
  print(unique(cses4_sub.list[[i]]$cses_party_position))
}

# Create distance variable
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$cses_party_position <- as.numeric(cses4_sub.list[[i]]$cses_party_position)
  print(unique(cses4_sub.list[[i]]$cses_party_position))
}
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$perceived_distance <- abs(cses4_sub.list[[i]]$cses_party_position-cses4_sub.list[[i]]$ideo)
  print(unique(cses4_sub.list[[i]]$perceived_distance))
}

#######################
#### Read in cses 3 and keep only relevant elections
#######################

# Read in cses3
load("cses3.rdata")

# Keep elections covered in CCDD
cses3_sub <- subset(cses3, C1004 =="CZE_2010" | C1004=="DEU_2009" | C1004=="DNK_2007" | C1004=="ESP_2008" | C1004=="NLD_2010" | C1004=="POL_2007" | C1004=="PRT_2009")

#######################
#### Using the lookup, assign party number in campaign data to previous vote choice in sub-cses.
#######################

# Separate each sub cses by country election. 
cses3_sub.list <- split(cses3_sub, cses3_sub$C1004)

# Turn irrelevant responses to NA
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$C3032_LH_PL[cses3_sub.list[[i]]$C3032_LH_PL>=89] <- NA
}

# Create variable for previous vote choice using campaign data's party number
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$prev_vote_choice_cdd <- mapvalues(cses3_sub.list[[i]]$C3032_LH_PL, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$country_year_cses_subject)
}

# Assign everything in two digits or less to NA
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$prev_vote_choice_cdd[cses3_sub.list[[i]]$prev_vote_choice_cdd<=99] <- NA
  print(unique(cses3_sub.list[[i]]$prev_vote_choice_cdd))
}

#######################
#### Determine whether respondent voted for party he/she previously voted for.
#######################

# 89 is independent candidate. 90 is other party/candidates. 91 is none of the parties/candidates. 92 is invalid ballot. 93 is blank ballot. 97 is refused. 98 is don't know. 99 is missing. Code 97, 98, and 99 as NA.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$C3023_LH_PL[cses3_sub.list[[i]]$C3023_LH_PL==97|cses3_sub.list[[i]]$C3023_LH_PL==98|cses3_sub.list[[i]]$C3023_LH_PL==99] <- NA
}

# C3021_1 indicates whether voter cast ballot in current Lower House election. 1 is yes. 5 is no. 7 is refused. 8 is don't know. 9 is missing. 
# Assign 100 to C3023_LH_PL for voters who have 5 for C3021_1. It indicates voters who did not vote.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$C3023_LH_PL[cses3_sub.list[[i]]$C3021_1==5] <- 100
}
# Turn observations in current vote choice that has values 7, 8, or 9 in voted or not to NA. These observations are not analyzed because it is not clear whether they voted or not.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$C3023_LH_PL[cses3_sub.list[[i]]$C3021_1>=7] <- NA
}

# Create variable for vote choice 
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$vote_choice_cdd <- mapvalues(cses3_sub.list[[i]]$C3023_LH_PL, lookup.list[[i]]$cses_party_num, as.character(lookup.list[[i]]$country_year_cses_subject))
  print(unique(cses3_sub.list[[i]]$vote_choice_cdd))
}

# If prev_vote_choice_cdd and vote_choice_cdd are the same, assign 1. If not, assign 0.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$loyal_vote <- ifelse(cses3_sub.list[[i]]$prev_vote_choice_cdd==cses3_sub.list[[i]]$vote_choice_cdd,1,0)
}

#######################
#### Create self-placement measure
#######################

# Turn anything larger than 10 to NA
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$ideo <- cses3_sub.list[[i]]$C3013
  cses3_sub.list[[i]]$ideo[cses3_sub.list[[i]]$ideo > 10] <- NA
}

#######################
#### Create distance variable
#######################

# Create a variable indicating party's perceived position.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$cses_party_letter <- mapvalues(cses3_sub.list[[i]]$prev_vote_choice_cdd, lookup.list[[i]]$country_year_cses_subject, lookup.list[[i]]$cses_party_letter)
  print(unique(cses3_sub.list[[i]]$cses_party_letter))
}
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$cses_party_position <- paste("C3011",cses3_sub.list[[i]]$cses_party_letter,sep="_")
  print(unique(cses3_sub.list[[i]]$cses_party_position))
}
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$cses_party_position[which(nchar(cses3_sub.list[[i]]$cses_party_position)!=7)] <- NA
  print(unique(cses3_sub.list[[i]]$cses_party_position))
}
for(i in names(cses3_sub.list)){
  for(j in 1:nrow(cses3_sub.list[[i]])){
    if(!is.na(cses3_sub.list[[i]][j,"cses_party_position"])){
      cses3_sub.list[[i]][j,"cses_party_position"] <- cses3_sub.list[[i]][j,cses3_sub.list[[i]]$cses_party_position[j]]
    }
  }
}

# Turn 95 and above to NA.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$cses_party_position[which(cses3_sub.list[[i]]$cses_party_position>=95)] <- NA
  print(unique(cses3_sub.list[[i]]$cses_party_position))
}

# Create distance variable
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$cses_party_position <- as.numeric(cses3_sub.list[[i]]$cses_party_position)
  print(unique(cses3_sub.list[[i]]$cses_party_position))
}
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$perceived_distance <- abs(cses3_sub.list[[i]]$cses_party_position-cses3_sub.list[[i]]$ideo)
  print(unique(cses3_sub.list[[i]]$perceived_distance))
}

#######################
#### Read in cses 2 and keep only relevant elections
#######################

# Read in cses2
load("cses2.rdata")

# Keep elections covered in cdd
cses2_sub <- subset(cses2, B1004 =="GBR_2005")

#######################
#### Using the lookup, assign party number in campaign data to previous vote choice in sub-cses.
#######################

# Separate each sub-cses by country election. 
cses2_sub.list <- split(cses2_sub, cses2_sub$B1004)

# Turn irreleavnt responses to NA. 99 is missing.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$B3018_1[cses2_sub.list[[i]]$B3018_1==99] <- NA
}

# Create variable for previous vote choice using campaign data's party number
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$prev_vote_choice_cdd <- mapvalues(cses2_sub.list[[i]]$B3018_1, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$country_year_cses_subject)
}

# Assign everything in two digits or less to NA
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$prev_vote_choice_cdd[cses2_sub.list[[i]]$prev_vote_choice_cdd<=99] <- NA
  print(unique(cses2_sub.list[[i]]$prev_vote_choice_cdd))
}

#######################
#### Determine whether respondent voted for party he/she previously voted for.
#######################

# Code 97, 98, and 99 as NA because it is unclear whether they voted or wanted to vote for their party or some other party. 
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$B3006_1[cses2_sub.list[[i]]$B3006_1==97|cses2_sub.list[[i]]$B3006_1==98|cses2_sub.list[[i]]$B3006_1==99] <- NA
}

# Assign 100 to B3006_1 for voters who have 2 for B3004_1. It indicates voters who did not vote.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$B3006_1[cses2_sub.list[[i]]$B3004_1==2] <- 100
}

# Create variable for vote choice using campaign data's party number
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$vote_choice_cdd <- mapvalues(cses2_sub.list[[i]]$B3006_1, lookup.list[[i]]$cses_party_num, as.character(lookup.list[[i]]$country_year_cses_subject))
  print(unique(cses2_sub.list[[i]]$vote_choice_cdd))
}

# If prev_vote_choice_cdd and vote_choice_cdd are the same, assign 1. If not, assign 0.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$loyal_vote <- ifelse(cses2_sub.list[[i]]$prev_vote_choice_cdd==cses2_sub.list[[i]]$vote_choice_cdd,1,0)
}

#######################
#### Create self-placement measure
#######################

# Turn anything larger than 10 to NA
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$ideo <- cses2_sub.list[[i]]$B3045
  cses2_sub.list[[i]]$ideo[cses2_sub.list[[i]]$ideo > 10] <- NA
}

#######################
#### Create distance variable
#######################

# Create a variable indicating party's perceived position.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$cses_party_letter <- mapvalues(cses2_sub.list[[i]]$prev_vote_choice_cdd, lookup.list[[i]]$country_year_cses_subject, lookup.list[[i]]$cses_party_letter)
  print(unique(cses2_sub.list[[i]]$cses_party_letter))
}
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$cses_party_position <- paste("B3038",cses2_sub.list[[i]]$cses_party_letter,sep="_")
  print(unique(cses2_sub.list[[i]]$cses_party_position))
}
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$cses_party_position[which(nchar(cses2_sub.list[[i]]$cses_party_position)!=7)] <- NA
  print(unique(cses2_sub.list[[i]]$cses_party_position))
}
for(i in names(cses2_sub.list)){
  for(j in 1:nrow(cses2_sub.list[[i]])){
    if(!is.na(cses2_sub.list[[i]][j,"cses_party_position"])){
      cses2_sub.list[[i]][j,"cses_party_position"] <- cses2_sub.list[[i]][j,cses2_sub.list[[i]]$cses_party_position[j]]
    }
  }
}

# Turn 95 and above to NA.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$cses_party_position[which(cses2_sub.list[[i]]$cses_party_position>=95)] <- NA
  print(unique(cses2_sub.list[[i]]$cses_party_position))
}

# Create distance variable
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$cses_party_position <- as.numeric(cses2_sub.list[[i]]$cses_party_position)
  print(unique(cses2_sub.list[[i]]$cses_party_position))
}
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$perceived_distance <- abs(cses2_sub.list[[i]]$cses_party_position-cses2_sub.list[[i]]$ideo)
  print(unique(cses2_sub.list[[i]]$perceived_distance))
}

#################
## Read in the supplementary data
#####################

dat <- read.csv("Data_supplementary.csv", stringsAsFactors=F)

# Create country_year_party variable
dat$country_year_party <- paste(dat$country_year_cses, dat$subject_from_original_dat, sep="_")

#######################
### Add information on two party versus multiparty systems:
#######################

# Read in ParlGov data
elect_dat <- read.csv("view_election.csv")
elect_param_dat <- read.csv("viewcalc_election_parameter.csv")

temp <- merge(elect_dat, elect_param_dat, by.x="election_id")

temp <- temp[which(temp$country_name_short %in% dat$country_name),]

temp$year <- as.numeric(substr(temp$election_date, 1, 4))

temp$country_year <- paste(temp$country_name_short, temp$year, sep="_")

# Create variable for enp
dat$enp_votes <- as.numeric(mapvalues(dat$country_year_cses, temp$country_year, temp$enp_votes))

# Create variable for number of leftist parties
left_num <- by(dat$lr_lib_agr, dat$country_year_cses, function(x){sum(x=="l",na.rm=T)})
dat$left_num <- as.numeric(mapvalues(dat$country_year_cses, names(left_num), as.vector(left_num)))

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign parfam_subject to sub-cses's:
#####################

# cses2
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$parfam_subject <- mapvalues(cses2_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)
}

# cses3
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$parfam_subject <- mapvalues(cses3_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)
}

# cses4
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$parfam_subject <- mapvalues(cses4_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)
}

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign neg_share_all_other_only to sub-cses's:
#####################

# cses2
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$neg_share_all_other_only <- mapvalues(cses2_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)
}

# cses3
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$neg_share_all_other_only <- mapvalues(cses3_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)
}

# cses4
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$neg_share_all_other_only <- mapvalues(cses4_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)
}

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign lastgovt to sub-cses's:
#####################

# cses2
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$lastgovt <- mapvalues(cses2_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)
}

# cses3
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$lastgovt <- mapvalues(cses3_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)
}

# cses4
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$lastgovt <- mapvalues(cses4_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)
}

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign pm_party to sub-cses's:
#####################

# cses2
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$pm_party <- mapvalues(cses2_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)
}

# cses3
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$pm_party <- mapvalues(cses3_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)
}

# cses4
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$pm_party <- mapvalues(cses4_sub.list[[i]]$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)
}

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign lr_lib_agr and originalCMP_rile_inverted to sub-cses's
######################

# Add in the variables:
new <- c("lr_lib_agr", "originalCMP_rile_inverted")

# cses2
for(j in names(cses2_sub.list)){
  for(i in new){
    cses2_sub.list[[j]] <- data.frame(cses2_sub.list[[j]], assign(i, mapvalues(cses2_sub.list[[j]]$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
  }
  colnames(cses2_sub.list[[j]]) <- c(head(colnames(cses2_sub.list[[j]]), ncol(cses2_sub.list[[j]])-length(new)),new)
}

for(i in names(cses2_sub.list)){
  m1 <- as.matrix(cses2_sub.list[[i]][,c("prev_vote_choice_cdd",new)])
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  cses2_sub.list[[i]][new] <- temp[,new]
}

# cses3
for(j in names(cses3_sub.list)){
  for(i in new){
    cses3_sub.list[[j]] <- data.frame(cses3_sub.list[[j]], assign(i, mapvalues(cses3_sub.list[[j]]$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
  }
  colnames(cses3_sub.list[[j]]) <- c(head(colnames(cses3_sub.list[[j]]), ncol(cses3_sub.list[[j]])-length(new)),new)
}

for(i in names(cses3_sub.list)){
  m1 <- as.matrix(cses3_sub.list[[i]][,c("prev_vote_choice_cdd",new)])
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  cses3_sub.list[[i]][new] <- temp[,new]
}

# cses4
for(j in names(cses4_sub.list)){
  for(i in new){
    cses4_sub.list[[j]] <- data.frame(cses4_sub.list[[j]], assign(i, mapvalues(cses4_sub.list[[j]]$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
  }
  colnames(cses4_sub.list[[j]]) <- c(head(colnames(cses4_sub.list[[j]]), ncol(cses4_sub.list[[j]])-length(new)),new)
}

for(i in names(cses4_sub.list)){
  m1 <- as.matrix(cses4_sub.list[[i]][,c("prev_vote_choice_cdd",new)])
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  cses4_sub.list[[i]][new] <- temp[,new]
}

######################
## Bring back the cses lists into dataframe format
#####################

# Bring back cses lists into dataframe
c4_back <- ldply(cses4_sub.list, .id=NULL)
c3_back <- ldply(cses3_sub.list, .id=NULL)
c2_back <- ldply(cses2_sub.list, .id=NULL)

######################
## Organize individual-level factors: education (D2003, C2003, B2003, A2003), gender (D2002, C2002, B2002, A2002), age (C2001, B2001, A2001, D2001_Y), income (D2020, C2020, B2020, A2012)
#####################

# cses4:

unique(c4_back$D2003) # 96 is none. Treat anything larger as missing.
c4_back$educ <- c4_back$D2003
c4_back$educ[c4_back$educ >= 97] <- NA
c4_back$educ[c4_back$educ == 96] <- 0

unique(c4_back$D2002) # 1 male 2 female
c4_back$male <- c4_back$D2002
c4_back$male[c4_back$male == 9] <- NA
c4_back$male[c4_back$male == 2] <- 0

unique(c4_back$D2001_Y) 
c4_back$age <- c4_back$D2001_Y
c4_back$age[c4_back$age >= 9997] <- NA
c4_back$age <- c4_back$D1008-c4_back$age

unique(c4_back$D2020) 
c4_back$income <- c4_back$D2020
c4_back$income[c4_back$income >= 6] <- NA

# cses3:

unique(c3_back$C2003)
c3_back$educ <- c3_back$C2003
c3_back$educ[c3_back$educ >= 9] <- NA

unique(c3_back $C2002) # 1 male 2 female
c3_back $male <- c3_back $C2002
c3_back $male[c3_back $male == 9] <- NA
c3_back $male[c3_back $male == 2] <- 0

unique(c3_back$C2001) 
c3_back$age <- c3_back$C2001
c3_back$age[c3_back$age >= 997] <- NA

unique(c3_back $C2020) 
c3_back $income <- c3_back $C2020
c3_back $income[c3_back $income >= 6] <- NA

# cses2:

unique(c2_back$B2003)
c2_back$educ <- c2_back$B2003
c2_back$educ[c2_back$educ >= 9] <- NA

unique(c2_back $B2002) # 1 male 2 female
c2_back $male <- c2_back $B2002
c2_back $male[c2_back $male == 9] <- NA
c2_back $male[c2_back $male == 2] <- 0

unique(c2_back $B2001) 
c2_back $age <- c2_back $B2001
c2_back $age[c2_back $age >= 997] <- NA

unique(c2_back $B2020) 
c2_back $income <- c2_back $B2020
c2_back $income[c2_back $income >= 6] <- NA

######################
## Combine the cses's using common columns. 
#####################

c2_back$country_election <- c2_back$B1004
c3_back$country_election <- c3_back$C1004
c4_back$country_election <- c4_back$D1004

a <- rbind(
  c2_back[,c("country_election","neg_share_all_other_only", "loyal_vote","ideo", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","lastgovt","pm_party","perceived_distance")], 
  c3_back[,c("country_election","neg_share_all_other_only", "loyal_vote", "ideo", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","lastgovt","pm_party","perceived_distance")], 
  c4_back[,c("country_election","neg_share_all_other_only", "loyal_vote", "ideo", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","lastgovt","pm_party","perceived_distance")])

a$lastgovt <- as.numeric(a$lastgovt)
a$neg_share_all_other_only <- as.numeric(a$neg_share_all_other_only)
a$originalCMP_rile_inverted <- as.numeric(a$originalCMP_rile_inverted)
a$pm_party <- as.numeric(a$pm_party)

#######################
#### Read in non-cses datasets
#######################

# Read in Netherlands 2012
nld2012 <- read.spss("NKO2012.sav",to.data.frame=T)

# Read in Spain 2011
esp2011 <- read.spss("CN4Spain2011.Fin.sav",to.data.frame = T)

# Read in UK 2015
gbr2015 <- read.dta13("bes_f2f_original_v3.0.dta", generate.factors=T, nonint.factors=T)

# Read in UK 2010
gbr2010 <- read.spss("CSES2010MISH10.SAV",to.data.frame = T)

######################
## Organize individual-level factors: education, gender, age, income
#####################

# nld2012:

# education: v344 (1 elementary, 2 lower vocational, 3 secondary, 4 middle level vocational/higher level secondary, 5 higher level vocational/university), v346 (isced code)
nld2012$educ <- as.numeric(as.character(mapvalues(nld2012$V344, levels(nld2012$V344), c(1:5,NA))))
# gender: v341 (1 female, 2 male)
nld2012$male <- as.numeric(as.character(mapvalues(nld2012$V341, c("Female","Male"), c(0,1))))
# age: v340
nld2012$age <- as.character(nld2012$V340)
nld2012$age[nld2012$age=="96 years old"] <- 96
nld2012$age[nld2012$age=="18 years old"] <- 18
nld2012$age <- as.numeric(nld2012$age)
# income: v359 (1~10)
nld2012$income <- as.numeric(as.character(mapvalues(nld2012$V359, levels(nld2012$V359), c(1:100))))

# esp 2011:

# education: L.Education
esp2011$educ <- as.numeric(as.character(mapvalues(esp2011$L.Education, levels(esp2011$L.Education), c(0:7,NA))))
# gender: L.Gender
esp2011$male <- as.numeric(as.character(mapvalues(esp2011$L.Gender, c("2 Female","1 Male"), c(0,1))))
# age: L.Age
esp2011$age <- as.character(esp2011$L.Age)
unique(substr(esp2011$age, 1, nchar(esp2011$age)-6))
esp2011$age <- as.numeric(substr(esp2011$age, 1, nchar(esp2011$age)-6))
# income: L.Income
esp2011$income <- as.numeric(as.character(mapvalues(esp2011$L.Income.Z, levels(esp2011$L.Income.Z), c(1:7,NA))))

# gbr 2015:

# y13a (education): What is the highest qualification you have? Please take your answer from this card and just give me the number next to it.
# 8 postgraduate degree; "Other technical, professional or higher qualification"  
# 7 first degree
# 6 university/poly diploma, teaching qualification, nursing qualification
# 5 HNC/HND, City&Guilds level 4, NVQ/SVQ 4/5; a level or equivalent ; scottish higher or equivalent
# 4 "ONC/OND, City&Guilds level 3, NVQ/SVQ 3"; "GCSE A*-C, CSE grade 1, O level grade A-C"; "Scottish Standard grades, Ordinary bands"; "City&Guilds level 2, NVQ/SVQ 2 and equivalent"           
# 3 "GCSE D-G, CSE grades 2-5, O level D-E"  ;"City&Guilds level 1, NVQ/SVQ 1 and equivalent"   ; "Clerical and commercial qualifications" ;"Recognised trade apprenticeship";"Youth training certificate, skill seekers";
# 2: no qualification
gbr2015$educ <- mapvalues(gbr2015$education, levels(unique(gbr2015$education)), c(NA,2,8,7,6,6,6,5,5,5,4,4,4,3,4,3,3,3,3,8))
# y9: gender (1 male 2 female)
gbr2015$male <- as.character(gbr2015$y09)
gbr2015$male <- as.numeric(mapvalues(gbr2015$male, c("Male","Female"),c(1,0)))
# Age
gbr2015$age <- gbr2015$Age
gbr2015$age[gbr2015$age==-1] <- NA
# y01: Which of the letters on this card represents the total income of your household from all sources before tax - including benefits, savings and so on?
gbr2015$income <- mapvalues(gbr2015$y01, levels(gbr2015$y01), c(NA,NA,NA,1:15))
gbr2015$income <- as.numeric(as.character(gbr2015$income))

# gbr 2010:

# education: q159
gbr2010$educ <- as.numeric(as.character(mapvalues(gbr2010$Q159, c("Postgraduate degree",
                                                                  "City & Guilds level 2 (Craft/Intermediate/Ordinary) or Scotv" ,               
                                                                  "University or CNAA first degree, e.g, BA, BSc"  , 
                                                                  "GCE A level, S level, A2 level, AS level, International Bacc"                    , 
                                                                  "Other technical, professional or higher qualification",
                                                                  "City & Guilds level 1, Scotvec National Certificate Modules,"                            ,
                                                                  "Teaching qualification (not degree)"                   ,
                                                                  "GCSE A*-C, CSE grade 1, GCE O level grade A -C, School Cer"   , 
                                                                  "University or CNAA diploma"                                   , 
                                                                  "University or CNAA higher degree (eg M.Sc, Ph.D)"      ,
                                                                  "Recognised trade apprenticeship completed"            , 
                                                                  "No formal qualifications"                              ,
                                                                  "Scottish Higher/Higher Still Grades, Scottish SLC/SUPE at Hi"                           ,
                                                                  "GCSE grades D-G, CSE grades 2-5, GCE O level grade D –E, S"                                        ,
                                                                  "Youth training certificate, skill seekers"               ,
                                                                  "Scottish Ordinary/Lower Certificate"                   ,
                                                                  "Clerical and commercial qualification (e.g., typing, shortha)"                               ,
                                                                  "Nursing qualification (e.g., SEN, SRC, SCM, RGC)" ,
                                                                  "Higher National Certificate (HNC) or Higher National Diploma",
                                                                  "Ordinary National Certificate (ONC) or Diploma (OND), City &",
                                                                  "Scottish Ordinary Bands A-C or pass, Scottish Standard Grade"),
                                                  c(8,4,7,5,8,3,6,4,6,8,3,2,5,3,3,4,3,6,5,4,4))))
# gender: Q186
gbr2010$male <- as.numeric(as.character(mapvalues(gbr2010$Q186, c("Female","Male"), c(0,1))))
# age: Q151
gbr2010$Q151[gbr2010$Q151=="Abstain"] <- NA
gbr2010$age <- 2017-as.numeric(as.character(gbr2010$Q151))
# income: Q166
gbr2010$income <- as.numeric(as.character(mapvalues(gbr2010$Q166, levels(gbr2010$Q166), c(1:16,NA))))

######################
## Organize ideology
#####################

# gbr2015
gbr2015$ideo <- gbr2015$e01
gbr2015$ideo <- as.character(gbr2015$ideo)
gbr2015$ideo[gbr2015$ideo=="Refused"] <- NA
gbr2015$ideo[gbr2015$ideo=="Don't know"] <- NA
gbr2015$ideo[gbr2015$ideo=="10 Right"] <- "10"
gbr2015$ideo[gbr2015$ideo=="0 Left"] <- "0"
gbr2015$ideo <- as.numeric(gbr2015$ideo)

# gbr2010
gbr2010$ideo <- as.character(gbr2010$P39Q1)
gbr2010$ideo[gbr2010$ideo=="10-Right"] <- "10"
gbr2010$ideo[gbr2010$ideo=="0-Left"] <- "0"
gbr2010$ideo <- as.numeric(gbr2010$ideo)

# esp2011
esp2011$ideo <- as.character(esp2011$C.LRSelf)
esp2011$ideo[esp2011$ideo=="1 Left"] <- "1"
esp2011$ideo[esp2011$ideo=="10 Right"] <- "10"
esp2011$ideo[esp2011$ideo=="999 Don't know or does not understand left right"] <- NA
esp2011$ideo <- as.numeric(esp2011$ideo)

# nld2012
nld2012$ideo <- as.numeric(as.character(mapvalues(nld2012$V130, levels(nld2012$V130), c(0:10, NA))))

#######################
#### Read in the party lookup file.
#######################

# Read in the lookup file:
lookup <- read.csv("Lookup_party_non_cses.csv",stringsAsFactors = F)

# Combine country_year_cses and subject
lookup$country_year_cses_subject <- paste(lookup$country_year_cses, lookup$subject, sep="_")

# Split by country_election
lookup.list <- split(lookup, lookup$country_year_cses)

#######################
#### Organize previous vote choice variable.
#######################

# nld2012: v151 party voted for in 2010
nld2012$prev_vote_choice <- as.character(nld2012$V151)
nld2012$prev_vote_choice[nld2012$prev_vote_choice=="Don't know"] <- NA
nld2012$prev_vote_choice[nld2012$prev_vote_choice=="Other party"] <- NA 
nld2012$prev_vote_choice[nld2012$prev_vote_choice=="Invalid"] <- NA

# esp 2011: H.VoteWhichRecent
esp2011$prev_vote_choice <- as.character(esp2011$H.VoteWhichPrevious)
esp2011$prev_vote_choice[esp2011$prev_vote_choice=="993 Did not vote, or don't know party"] <- NA
esp2011$prev_vote_choice[esp2011$prev_vote_choice=="998 Missing"] <- NA
# Get rid of numbers in front
esp2011$prev_vote_choice <- sub(".*? (.+)", "\\1", esp2011$prev_vote_choice)

# gbr 2015: u05 which party did you vote for in 2010
gbr2015$prev_vote_choice <- as.character(gbr2015$u05)
gbr2015$prev_vote_choice[gbr2015$prev_vote_choice=="Did not vote"] <- NA
gbr2015$prev_vote_choice[gbr2015$prev_vote_choice=="Don't know"] <- NA
gbr2015$prev_vote_choice[gbr2015$prev_vote_choice=="Not eligible/too young to vote"] <- NA
gbr2015$prev_vote_choice[gbr2015$prev_vote_choice=="Refused"] <- NA
gbr2015$prev_vote_choice[gbr2015$prev_vote_choice=="Other"] <- NA
gbr2015$prev_vote_choice[which(gbr2015$prev_vote_choice=="Scottish National Party")] <- "Scottish National Party (SNP)"

# gbr 2010: p64q1
gbr2010$prev_vote_choice <- as.character(gbr2010$P64Q1)
gbr2010$prev_vote_choice[gbr2010$prev_vote_choice=="Did not vote"] <- NA
gbr2010$prev_vote_choice[gbr2010$prev_vote_choice=="Can’t remember/Don’t know"] <- NA
gbr2010$prev_vote_choice[gbr2010$prev_vote_choice=="Too young to vote, not eligible"] <- NA
gbr2010$prev_vote_choice[gbr2010$prev_vote_choice=="Other party"] <- NA

#######################
#### Using the lookup, assign party number in campaign data to the surveys.
#######################

gbr2015$prev_vote_choice_cdd <- as.character(mapvalues(gbr2015$prev_vote_choice, lookup.list[["GBR_2015"]]$id_party_code, lookup.list[["GBR_2015"]]$country_year_cses_subject))

gbr2010$prev_vote_choice_cdd <- as.character(mapvalues(gbr2010$prev_vote_choice, lookup.list[["GBR_2010"]]$id_party_code, lookup.list[["GBR_2010"]]$country_year_cses_subject))

esp2011$prev_vote_choice_cdd <- as.character(mapvalues(esp2011$prev_vote_choice, lookup.list[["ESP_2011"]]$id_party_code, lookup.list[["ESP_2011"]]$country_year_cses_subject))

nld2012$prev_vote_choice_cdd <- as.character(mapvalues(nld2012$prev_vote_choice, lookup.list[["NLD_2012"]]$id_party_code, lookup.list[["NLD_2012"]]$country_year_cses_subject))

#######################
#### Organize vote choice variable by figuring out whether respondent voted for the same party
#######################

# gbr 2015: b02 which party did you vote for
gbr2015$b02 <- as.character(gbr2015$b02)
gbr2015$b02[gbr2015$b02=="Refused"|gbr2015$b02=="Don't know"] <- NA
gbr2015$b02[which(gbr2015$b01=="No, did not vote")] <- "Did not vote"

# gbr 2010: p52q1
gbr2010$P52Q1 <- as.character(gbr2010$P52Q1)
gbr2010$P52Q1[which(gbr2010$P51Q1=="No, I did not vote")] <- "Did not vote"

# esp 2011: H.VoteWhichRecent
# Get rid of the numbers in front of each value
esp2011$H.VoteWhichRecent <- sub(".*? (.+)", "\\1", esp2011$H.VoteWhichRecent)

# nld 2012:
nld2012$V212 <- as.character(nld2012$V212)
nld2012$V212[nld2012$V210=="No"] <- "Did not vote"

gbr2015$vote_choice_cdd <- as.character(mapvalues(gbr2015$b02, lookup.list[["GBR_2015"]]$vote_choice_party_code, lookup.list[["GBR_2015"]]$country_year_cses_subject))
gbr2010$vote_choice_cdd <- as.character(mapvalues(gbr2010$P52Q1, lookup.list[["GBR_2010"]]$vote_choice_party_code, lookup.list[["GBR_2010"]]$country_year_cses_subject))
esp2011$vote_choice_cdd <- as.character(mapvalues(esp2011$H.VoteWhichRecent, lookup.list[["ESP_2011"]]$vote_choice_party_code, lookup.list[["ESP_2011"]]$country_year_cses_subject))
nld2012$vote_choice_cdd <- as.character(mapvalues(nld2012$V212, lookup.list[["NLD_2012"]]$vote_choice_party_code, lookup.list[["NLD_2012"]]$country_year_cses_subject))

# If prev_vote_choice_cdd and vote_choice_cdd are the same, assign 1. If not, assign 0.
gbr2015$loyal_vote <- ifelse(gbr2015$prev_vote_choice_cdd==gbr2015$vote_choice_cdd,1,0)
gbr2010$loyal_vote <- ifelse(gbr2010$prev_vote_choice_cdd==gbr2010$vote_choice_cdd,1,0)
esp2011$loyal_vote <- ifelse(esp2011$prev_vote_choice_cdd==esp2011$vote_choice_cdd,1,0)
nld2012$loyal_vote <- ifelse(nld2012$prev_vote_choice_cdd==nld2012$vote_choice_cdd,1,0)

#######################
#### Create distance variable
#######################

# nld2012

# Turn observations into numbers
for(i in paste("V",120:129,sep="")){
  nld2012[,i] <- as.numeric(as.character(mapvalues(nld2012[,i], levels(nld2012[,i]), c(0:10,NA))))
}

# Change variable names in nld2012
colnames(nld2012)[which(colnames(nld2012) %in% paste("V",120:129,sep=""))] <- c("CDA","PvdA","VVD","D66","GroenLinks","SP","Partij voor de Vrijheid","ChristenUnie","SGP","Partij voor de Dieren")

# Create variable indicating variable name
nld2012$variable <- mapvalues(nld2012$prev_vote_choice_cdd, lookup.list[["NLD_2012"]]$country_year_cses_subject, lookup.list[["NLD_2012"]]$vote_choice_party_code)
nld2012$variable[which(nld2012$variable=="Blanco ")] <- NA
nld2012$variable[which(nld2012$variable=="Trots op Nederland")] <- NA

# Assign party position that correspond to the party one voted for
nld2012$party_position <- NA
for(j in 1:nrow(nld2012)){
  if(!is.na(nld2012[j,"variable"])){
    nld2012[j,"party_position"] <- nld2012[j,nld2012$variable[j]]
  }
}

# Create distance variable
nld2012$perceived_distance <- abs(nld2012$party_position-nld2012$ideo)

# esp2011

# Turn observations into numbers
for(i in colnames(esp2011)[15:28]){
  esp2011[,i] <- as.numeric(as.character(mapvalues(esp2011[,i], levels(esp2011[,i]), c(0:10,NA))))
}

# Change variable names in esp2011
colnames(esp2011)[15:20] <- substring(colnames(esp2011)[15:20],5)

# Create variable indicating variable name
esp2011$variable <- mapvalues(esp2011$prev_vote_choice_cdd, lookup.list[["ESP_2011"]]$country_year_cses_subject, lookup.list[["ESP_2011"]]$vote_choice_party_code)
esp2011$variable[which(esp2011$variable=="OppOther")] <- NA
esp2011$variable[which(esp2011$variable=="OppNew5")] <- NA
esp2011$variable[which(esp2011$variable=="OppNew3")] <- NA
esp2011$variable[which(esp2011$variable=="Opp4")] <- NA
esp2011$variable[which(esp2011$variable=="Opp6")] <- NA
esp2011$variable[which(esp2011$variable=="Opp7")] <- NA

# Assign party position that correspond to the party one voted for
esp2011$party_position <- NA
for(j in 1:nrow(esp2011)){
  if(!is.na(esp2011[j,"variable"])){
    esp2011[j,"party_position"] <- esp2011[j,esp2011$variable[j]]
  }
}

# Create distance variable
esp2011$perceived_distance <- abs(esp2011$party_position-esp2011$ideo)

# gbr2015

# Turn observations into numbers
for(i in colnames(gbr2015)[375:381]){
  gbr2015[,i] <- as.numeric(as.character(mapvalues(gbr2015[,i], levels(gbr2015[,i]), c(NA,NA,0:10))))
}

# Change variable names in gbr2015
colnames(gbr2015)[375:381] <- c("Conservatives","Labour","Liberal Democrats","United Kingdom Independence Party (UKIP)","Green Party","Scottish National Party (SNP)","Plaid Cymru")

# Create variable indicating variable name
gbr2015$variable <- mapvalues(gbr2015$prev_vote_choice_cdd, lookup.list[["GBR_2015"]]$country_year_cses_subject, lookup.list[["GBR_2015"]]$vote_choice_party_code)
gbr2015$variable[which(gbr2015$variable=="British National Party (BNP)")] <- NA
gbr2015$variable[which(gbr2015$variable=="Green Party")] <- NA
gbr2015$variable[which(gbr2015$variable=="Plaid Cymru")] <- NA

# Assign party position that correspond to the party one voted for
gbr2015$party_position <- NA
for(j in 1:nrow(gbr2015)){
  if(!is.na(gbr2015[j,"variable"])){
    gbr2015[j,"party_position"] <- gbr2015[j,gbr2015$variable[j]]
  }
}

# Create distance variable
gbr2015$perceived_distance <- abs(gbr2015$party_position-gbr2015$ideo)

# gbr2010

# Turn observations into numbers
for(i in colnames(gbr2010)[48:52]){
  gbr2010[,i] <- as.numeric(as.character(mapvalues(gbr2010[,i], levels(gbr2010[,i]), 0:10)))
}

# Change variable names in gbr2010
colnames(gbr2010)[48:52] <- c("Labour","Conservative","Liberal Democrat","Scottish National Party (SNP)","Plaid Cymru")

# Create variable indicating variable name
gbr2010$variable <- mapvalues(gbr2010$prev_vote_choice_cdd, lookup.list[["GBR_2010"]]$country_year_cses_subject, lookup.list[["GBR_2010"]]$vote_choice_party_code)
gbr2010$variable[which(gbr2010$variable=="United Kingdom Independence Party (UKIP)")] <- NA
gbr2010$variable[which(gbr2010$variable=="Green Party")] <- NA
gbr2010$variable[which(gbr2010$variable=="Scottish National Party (SNP)")] <- NA
gbr2010$variable[which(gbr2010$variable=="Plaid Cymru")] <- NA
gbr2010$variable[which(gbr2010$variable=="British National Party (BNP)")] <- NA

# Assign party position that correspond to the party one voted for
gbr2010$party_position <- NA
for(j in 1:nrow(gbr2010)){
  if(!is.na(gbr2010[j,"variable"])){
    gbr2010[j,"party_position"] <- gbr2010[j,gbr2010$variable[j]]
  }
}

# Create distance variable
gbr2010$perceived_distance <- abs(gbr2010$party_position-gbr2010$ideo)

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign parfam_subject to the surveys:
#####################

gbr2015$parfam_subject <- mapvalues(gbr2015$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)
gbr2010$parfam_subject <- mapvalues(gbr2010$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)
esp2011$parfam_subject <- mapvalues(esp2011$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)
nld2012$parfam_subject <- mapvalues(nld2012$prev_vote_choice_cdd, dat$country_year_party, dat$parfam_subject)

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign neg_share_all_other_only to the surveys:
#####################

gbr2015$neg_share_all_other_only <- mapvalues(gbr2015$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)
gbr2010$neg_share_all_other_only <- mapvalues(gbr2010$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)
esp2011$neg_share_all_other_only <- mapvalues(esp2011$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)
nld2012$neg_share_all_other_only <- mapvalues(nld2012$prev_vote_choice_cdd, dat$country_year_party, dat$neg_share_all_other_only)

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign lastgovt to the surveys:
#####################

gbr2015$lastgovt <- mapvalues(gbr2015$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)
gbr2010$lastgovt <- mapvalues(gbr2010$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)
esp2011$lastgovt <- mapvalues(esp2011$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)
nld2012$lastgovt <- mapvalues(nld2012$prev_vote_choice_cdd, dat$country_year_party, dat$lastgovt)

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign pm_party to the surveys:
#####################

gbr2015$pm_party <- mapvalues(gbr2015$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)
gbr2010$pm_party <- mapvalues(gbr2010$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)
esp2011$pm_party <- mapvalues(esp2011$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)
nld2012$pm_party <- mapvalues(nld2012$prev_vote_choice_cdd, dat$country_year_party, dat$pm_party)

######################
## Use country_year_party in dat and prev_vote_choice_cdd in the election studies to assign lr_lib_agr and originalCMP_rile_inverted to the surveys.
######################

# Add in the variables:
new <- c("lr_lib_agr", "originalCMP_rile_inverted")

for(i in new){
  gbr2015 <- data.frame(gbr2015, assign(i, mapvalues(gbr2015$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
}
colnames(gbr2015) <- c(head(colnames(gbr2015), ncol(gbr2015)-length(new)),new)

for(i in new){
  gbr2010 <- data.frame(gbr2010, assign(i, mapvalues(gbr2010$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
}
colnames(gbr2010) <- c(head(colnames(gbr2010), ncol(gbr2010)-length(new)),new)

for(i in new){
  esp2011 <- data.frame(esp2011, assign(i, mapvalues(esp2011$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
}
colnames(esp2011) <- c(head(colnames(esp2011), ncol(esp2011)-length(new)),new)

for(i in new){
  nld2012 <- data.frame(nld2012, assign(i, mapvalues(nld2012$prev_vote_choice_cdd, dat$country_year_party, dat[,i])))
}
colnames(nld2012) <- c(head(colnames(nld2012), ncol(nld2012)-length(new)),new)

######################
## Combine the surveys using common columns. 
#####################

gbr2015$country_election <- "GBR_2015"
gbr2010$country_election <- "GBR_2010"
esp2011$country_election <- "ESP_2011"
nld2012$country_election <- "NLD_2012"

b <- rbind(gbr2015[,c("country_election","neg_share_all_other_only", "loyal_vote", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","ideo","lastgovt","pm_party","perceived_distance")], 
           gbr2010[,c("country_election","neg_share_all_other_only", "loyal_vote", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","ideo","lastgovt","pm_party","perceived_distance")],
           esp2011[,c("country_election","neg_share_all_other_only", "loyal_vote", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","ideo","lastgovt","pm_party","perceived_distance")],
           nld2012[,c("country_election","neg_share_all_other_only", "loyal_vote", "prev_vote_choice_cdd",new,"educ","age","male","income","parfam_subject","ideo","lastgovt","pm_party","perceived_distance")])

b$neg_share_all_other_only <- as.numeric(b$neg_share_all_other_only)
b$originalCMP_rile_inverted <- as.numeric(b$originalCMP_rile_inverted)
b$lastgovt <- as.numeric(b$lastgovt)
b$pm_party <- as.numeric(b$pm_party)

######################
## Combine cses and non-cses data
#####################

combined <- merge(a,b,all=T)

######################
## Standardize education, income, and ideology variables
#####################

# Turn educ to numeric
combined$educ <- as.numeric(combined$educ)

# Split combined to list
temp <- split(combined, combined$country_election)

# Standardize educ variable to range from 0 to 1, for each country election
for(i in names(temp)){
  u <- temp[[i]]$educ
  temp[[i]]$educ_scaled <- (u - min(u,na.rm=T))/diff(range(u, na.rm=T))
}

# Standardize income variable to range from 0 to 1, for each country election
for(i in names(temp)){
  u <- temp[[i]]$income
  temp[[i]]$income_scaled <- (u - min(u,na.rm=T))/diff(range(u, na.rm=T))
}

for(i in names(temp)){
  u <- temp[[i]]$ideo
  temp[[i]]$ideo_scaled <- (u - min(u,na.rm=T))/diff(range(u, na.rm=T))
}

# Bring temp back together
combined <- ldply(temp, .id=NULL)

##########################
######## Add votet1 to combined
##########################

combined$votet1 <- as.numeric(mapvalues(combined$prev_vote_choice_cdd, dat$country_year_party, dat$votet1))

##########################
######## Add gdp to combined
##########################

combined$gdp <- as.numeric(mapvalues(combined$country_election, dat$country_year_cses, dat$gdp))

##########################
######## Add number of leftist parties
##########################

combined$left_num <- as.numeric(mapvalues(combined$country_election, dat$country_year_cses, dat$left_num))

##########################
######## Add enp to combined
##########################

combined$enp_votes <- as.numeric(mapvalues(combined$country_election, dat$country_year_cses, dat$enp_votes))

##########################
######  Check and organize the data
##########################

# Get rid of those with NA in prev_vote_choice_cdd.
combined <- subset(combined, !is.na(combined$prev_vote_choice_cdd))

# Relevel factor variables to indicate left.
combined$lr_lib_agr <- as.factor(combined$lr_lib_agr)
combined$lr_lib_agr <- relevel(combined$lr_lib_agr, "r")

# Make age squared
combined$age_sq <- (combined$age)^2

# Create combined variable for country
combined$country_name <- substr(combined$country_election, start=1, stop=3)

#######################
### Organize the CCDD datasets
#######################

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

# Create variable country_year_party
all$country_year_party <- paste(all$country_year, all$subject, sep="_")

#######################
### Calculate variables for total statements
#######################

combined$country_name_two_char <- mapvalues(combined$country_name, c("GBR", "CZE" ,"DEU" ,"DNK", "ESP" ,"NLD", "POL" ,"PRT", "SWE"), c("UK","CZ","DE","DK","ES","NL","PL","PO","SV"))
combined$country_year_party_two_char <- paste(combined$country_name_two_char, substr(combined$prev_vote_choice_cdd, 4, nchar(combined$prev_vote_choice_cdd)), sep="")

combined$total_counts <- as.numeric(mapvalues(combined$country_year_party_two_char, names(table(all$country_year_party)), table(all$country_year_party)))

#####################
###### Create variable for party position change
######################

dat$abs_rile_change <- abs(dat$originalCMP_rile - dat$rile_prev)

combined$abs_rile_change <- as.numeric(mapvalues(combined$prev_vote_choice_cdd, dat$country_year_party, dat$abs_rile_change))

#####################
###### Organize ideology variables
######################

# Invert scaled ideo variable so that variable ranges from rightist to leftist
combined$ideo_scaled_inverted <- -combined$ideo_scaled

# Turn ideo_scaled_inverted to range from 0 to 1.
combined$ideo_scaled_inverted_0to1 <- combined$ideo_scaled_inverted + 1

#####################
###### Save the data 
######################

study1b_incl_abstentions <- combined
study1b_incl_abstentions <- study1b_incl_abstentions[,c("country_election","country_name","country_year_party_two_char","prev_vote_choice_cdd","neg_share_all_other_only","loyal_vote","ideo_scaled_inverted_0to1","age","age_sq","male","total_counts","income_scaled","educ_scaled","votet1","perceived_distance","abs_rile_change","gdp","enp_votes","left_num","lastgovt","pm_party")]
save(study1b_incl_abstentions, file="Study1b_including_abstentions.RData")
