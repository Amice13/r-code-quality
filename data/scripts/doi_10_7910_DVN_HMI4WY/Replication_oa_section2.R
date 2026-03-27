# Read in the raw data from UK.
raw <- read.csv("2015_merged_no_labels_cleaned.csv")

# Leave in only relevant variables
raw_sub <- raw[,c("year_month","newspaper_title","article_date","title","subject1","subject_confidence", "other1_yn","other_actor1", "issue1_other1", "valence_issue1_yesno_other1","valence_issue1_other1","valence_issue1_other_other1","valence_issue1_direc_other1","quote_issue1_other1", "valence_yesno_other1","valence1_other1","valence1_other_other1","valence1_direc_other1","quote_valence1_other1")]

# Further subset the data to attacks
raw_sub <- subset(raw_sub, valence_issue1_direc_other1==-1 | valence1_direc_other1==-1)

# Pick out random set of examples by valence content for policy-related valence.

set.seed(2011)
party_character <- subset(raw_sub, valence_issue1_other1==1 & valence_issue1_direc_other1==-1)
party_character[sample(1:nrow(party_character),1),]

party_competence <- subset(raw_sub, valence_issue1_other1==2 & valence_issue1_direc_other1==-1)
party_competence[sample(1:nrow(party_competence),1),]

leader_character <- subset(raw_sub, valence_issue1_other1==4 & valence_issue1_direc_other1==-1)
leader_character[sample(1:nrow(leader_character),1),]

leader_competence <- subset(raw_sub, valence_issue1_other1==5 & valence_issue1_direc_other1==-1)
leader_competence[sample(1:nrow(leader_competence),1),]

leader_charisma <- subset(raw_sub, valence_issue1_other1==6 & valence_issue1_direc_other1==-1)
leader_charisma[sample(1:nrow(leader_charisma),1),]

other <- subset(raw_sub, valence_issue1_other1==7 & valence_issue1_direc_other1==-1)
other[sample(1:nrow(other),1),]

# Pick out random set of examples by valence content for non-policy-related valence.

party_character <- subset(raw_sub, valence1_other1==1 & valence1_direc_other1==-1)
party_character[sample(1:nrow(party_character),1),]

party_competence <- subset(raw_sub, valence1_other1==2 & valence1_direc_other1==-1)
party_competence[sample(1:nrow(party_competence),1),]

party_unity <- subset(raw_sub, valence1_other1==3 & valence1_direc_other1==-1)
party_unity[sample(1:nrow(party_unity),1),]

leader_character <- subset(raw_sub, valence1_other1==4 & valence1_direc_other1==-1)
leader_character[sample(1:nrow(leader_character),1),]

leader_competence <- subset(raw_sub, valence1_other1==5 & valence1_direc_other1==-1)
leader_competence[sample(1:nrow(leader_competence),1),]

leader_charisma <- subset(raw_sub, valence1_other1==6 & valence1_direc_other1==-1)
leader_charisma[sample(1:nrow(leader_charisma),1),]

other <- subset(raw_sub, valence1_other1==7 & valence1_direc_other1==-1)
other[sample(1:nrow(other),1),]

##############################
###### Table OA2.1
##############################

# Pick out random set of examples by valence content for regardless of whether it's policy-specific or general valence.

party_character <- subset(raw_sub, valence1_other1==1 & other_actor1==1 | valence_issue1_other1==1 & other_actor1==1)
party_character[sample(1:nrow(party_character),1),] 

party_competence <- subset(raw_sub, valence1_other1==2 & other_actor1==2 | valence_issue1_other1==2 & other_actor1==2)
party_competence[sample(1:nrow(party_competence),1),]

party_unity <- subset(raw_sub, valence1_other1==3 & other_actor1==3 | valence_issue1_other1==3 & other_actor1==3)
party_unity[sample(1:nrow(party_unity),1),]
party_unity <- subset(raw_sub, valence1_other1==3 | valence_issue1_other1==3 & other_actor1==3)
party_unity[sample(1:nrow(party_unity),1),]

leader_character <- subset(raw_sub, valence1_other1==4 & other_actor1==4 | valence_issue1_other1==4 & other_actor1==4)
leader_character[sample(1:nrow(leader_character),1),]

leader_competence <- subset(raw_sub, valence1_other1==5 & other_actor1==5| valence_issue1_other1==5& other_actor1==5)
leader_competence[sample(1:nrow(leader_competence),1),]

leader_charisma <- subset(raw_sub, valence1_other1==6 & other_actor1==3 | valence_issue1_other1==6 & other_actor1==3)
leader_charisma[sample(1:nrow(leader_charisma),1),]
