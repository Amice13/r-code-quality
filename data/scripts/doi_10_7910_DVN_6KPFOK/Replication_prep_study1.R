# Libraries
install.packages(c("plyr","foreign"))
library(plyr)
library(foreign)

# Read in supplementary data
supp <- read.csv("Data_supplementary.csv")

#######################
#### Read in cses4 and subset to relevant elections
#######################

# Read in cses4
load("cses4.rdata")

# Reduce to relevant elections
cses4_sub <- subset(cses4, D1004 =="AUS_2013" | D1004=="IRL_2011" | D1004=="NZL_2011" | D1004=="USA_2012")

#######################
#### Read in the party lookup file and organize it.
#######################

# Read in the lookup file:
lookup <- read.csv("Lookup_party.csv")

# Split by country_election
lookup.list <- split(lookup, lookup$country_election_cses)

#######################
#### Using the lookup, assign mpd's party number to sub cses.
#######################

# Separate each sub cses by country election. 
cses4_sub.list <- split(cses4_sub, cses4_sub$D1004)

# D3018_3 is which party do you feel closest to. Create variable for closest party using mpd's party number.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$which_close_mpd <- mapvalues(cses4_sub.list[[i]]$D3018_3, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$party_number)
}

#  89 is independent candidate. 90 is other party. 91 is none of the parties/candidates. 97 refused. 98 don't know. 99 missing. Assign everything from 91 to 99 and 89 to NA.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$which_close_mpd[cses4_sub.list[[i]]$which_close_mpd>=91 & cses4_sub.list[[i]]$which_close_mpd<=99 | cses4_sub.list[[i]]$which_close_mpd==89] <- NA
  print(unique(cses4_sub.list[[i]]$which_close_mpd))
}

# Assign party_election in lookup form to sub cses.
for(i in names(cses4_sub.list)){
  cses4_sub.list[[i]]$which_close_party_election <- mapvalues(cses4_sub.list[[i]]$which_close_mpd, lookup.list[[i]]$party_number, as.character(lookup.list[[i]]$party_election))
}

#######################
#### Read in cses3 and subset to relevant elections
#######################

# Read in cses3
load("cses3.rdata")

# Reduce to relevant elections
cses3_sub <- subset(cses3, C1004 =="AUS_2007" | C1004 =="IRL_2007" | C1004 =="NZL_2008" | C1004=="USA_2008")

#######################
#### Using the lookup, assign mpd's party number to sub cses.
#######################

# Separate each sub cses by country election. 
cses3_sub.list <- split(cses3_sub, cses3_sub$C1004)

# C3020_3 is which party do you feel closest to. Create variable for closest party using mpd's party number
for(i in names(lookup.list)){
  cses3_sub.list[[i]]$which_close_mpd <- mapvalues(cses3_sub.list[[i]]$C3020_3, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$party_number)
}

# 89 is independent candidate. 90 is other party. 91 is none of the parties/candidates. 97 refused. 98 don't know. 99 missing. Assign everything from 91 to 99 and 89 to NA
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$which_close_mpd[cses3_sub.list[[i]]$which_close_mpd>=91 & cses3_sub.list[[i]]$which_close_mpd<=99 | cses3_sub.list[[i]]$which_close_mpd==89 ] <- NA
  print(unique(cses3_sub.list[[i]]$which_close_mpd))
}

# Assign party_election in lookup form to sub cses.
for(i in names(cses3_sub.list)){
  cses3_sub.list[[i]]$which_close_party_election <- mapvalues(cses3_sub.list[[i]]$which_close_mpd, lookup.list[[i]]$party_number, as.character(lookup.list[[i]]$party_election))
}

#######################
#### Read in cses2 and subset to relevant elections
#######################

# Read in cses2
load("cses2.rdata")

# Reduce to relevant elections
cses2_sub <- subset(cses2, B1004 =="AUS_2004" | B1004 =="NZL_2002" | B1004 =="USA_2004")

#######################
#### Using the lookup, assign mpd's party number to sub cses.
#######################

# Separate each sub cses by country election. 
cses2_sub.list <- split(cses2_sub, cses2_sub$B1004)

# B3029_1 is which party do you feel closest to. Create variable for closest party using mpd's party number.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$which_close_mpd <- mapvalues(cses2_sub.list[[i]]$B3029_1, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$party_number)
}

# 97 is refused. 98 is don't know. 99 is missing. Assign everything 97 and above to NA. But the mpd party numbers are numeric. So set a limit.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$which_close_mpd[cses2_sub.list[[i]]$which_close_mpd>=97 & cses2_sub.list[[i]]$which_close_mpd<=99] <- NA
  print(unique(cses2_sub.list[[i]]$which_close_mpd))
}

# Assign party_election in lookup form to sub cses.
for(i in names(cses2_sub.list)){
  cses2_sub.list[[i]]$which_close_party_election <- mapvalues(cses2_sub.list[[i]]$which_close_mpd, lookup.list[[i]]$party_number, as.character(lookup.list[[i]]$party_election))
  print(unique(cses2_sub.list[[i]]$which_close_party_election))
}

#######################
#### Read in cses1 and subset to relevant elections
#######################

# Read in cses1
load("cses1.rdata")

# Reduce to relevant elections
cses1_sub <- subset(cses1,A1004 =="NZL_1996" | A1004 =="GBR_1997")

#######################
#### Using the lookup, assign mpd's party number to sub cses.
#######################

# Separate each sub cses by country election. 
cses1_sub.list <- split(cses1_sub, cses1_sub$A1004)

# A3005_1 is which party do you feel closest to. Create variable for closest party using mpd's party number.
for(i in names(cses1_sub.list)){
  cses1_sub.list[[i]]$which_close_mpd <- mapvalues(cses1_sub.list[[i]]$A3005_1, lookup.list[[i]]$cses_party_num, lookup.list[[i]]$party_number)
}

# 0 is not applicable. 98 is don't know or no party mentioned. 99 is not available. Assign NA to 0, 98, and 99.
for(i in names(cses1_sub.list)){
  cses1_sub.list[[i]]$which_close_mpd[cses1_sub.list[[i]]$which_close_mpd==0|cses1_sub.list[[i]]$which_close_mpd==98|cses1_sub.list[[i]]$which_close_mpd==99] <- NA
  print(unique(cses1_sub.list[[i]]$which_close_mpd))
}

# Assign party_election in lookup form to sub cses.
for(i in names(cses1_sub.list)){
  cses1_sub.list[[i]]$which_close_party_election <- mapvalues(cses1_sub.list[[i]]$which_close_mpd, lookup.list[[i]]$party_number, as.character(lookup.list[[i]]$party_election))
  print(unique(cses1_sub.list[[i]]$which_close_party_election))
}

######################
## Use party_election in supp and which_close_party_election in sub cses's to assign niche and prev_enp_nat and rile to sub cses's.
#####################

# Create vector of variable names and add in variables.
new <- c("rile","niche","prev_enp_nat")

# cses1
for(j in names(cses1_sub.list)){
  for(i in new){
    cses1_sub.list[[j]] <- data.frame(cses1_sub.list[[j]], assign(i, mapvalues(cses1_sub.list[[j]]$which_close_party_election, supp$party_election, supp[,i])))
  }
  colnames(cses1_sub.list[[j]]) <- c(head(colnames(cses1_sub.list[[j]]), ncol(cses1_sub.list[[j]])-length(new)),new)
}

for(i in names(cses1_sub.list)){
  m1 <- as.matrix(cses1_sub.list[[i]][,c("which_close_party_election",new)])
  m1[!duplicated(m1),]
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[!duplicated(temp),]	
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  temp[!duplicated(temp),]		
  cses1_sub.list[[i]][new] <- temp[,new]
  print(cses1_sub.list[[i]][!duplicated(cses1_sub.list[[i]][,c("which_close_party_election",new)]),c("which_close_party_election",new)])
}

# cses2
for(j in names(cses2_sub.list)){
  for(i in new){
    cses2_sub.list[[j]] <- data.frame(cses2_sub.list[[j]], assign(i, mapvalues(cses2_sub.list[[j]]$which_close_party_election, supp$party_election, supp[,i])))
  }
  colnames(cses2_sub.list[[j]]) <- c(head(colnames(cses2_sub.list[[j]]), ncol(cses2_sub.list[[j]])-length(new)),new)
}

for(i in names(cses2_sub.list)){
  m1 <- as.matrix(cses2_sub.list[[i]][,c("which_close_party_election",new)])
  m1[!duplicated(m1),]
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[!duplicated(temp),]	
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  temp[!duplicated(temp),]		
  cses2_sub.list[[i]][new] <- temp[,new]
  print(cses2_sub.list[[i]][!duplicated(cses2_sub.list[[i]][,c("which_close_party_election",new)]),c("which_close_party_election",new)])
}

# cses3
for(j in names(cses3_sub.list)){
  for(i in new){
    cses3_sub.list[[j]] <- data.frame(cses3_sub.list[[j]], assign(i, mapvalues(cses3_sub.list[[j]]$which_close_party_election, supp$party_election, supp[,i])))
  }
  colnames(cses3_sub.list[[j]]) <- c(head(colnames(cses3_sub.list[[j]]), ncol(cses3_sub.list[[j]])-length(new)),new)
}

for(i in names(cses3_sub.list)){
  m1 <- as.matrix(cses3_sub.list[[i]][,c("which_close_party_election",new)])
  m1[!duplicated(m1),]
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[!duplicated(temp),]	
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  temp[!duplicated(temp),]		
  cses3_sub.list[[i]][new] <- temp[,new]
  print(cses3_sub.list[[i]][!duplicated(cses3_sub.list[[i]][,c("which_close_party_election",new)]),c("which_close_party_election",new)])
}

# cses4
for(j in names(cses4_sub.list)){
  for(i in new){
    cses4_sub.list[[j]] <- data.frame(cses4_sub.list[[j]], assign(i, mapvalues(cses4_sub.list[[j]]$which_close_party_election, supp$party_election, supp[,i])))
  }
  colnames(cses4_sub.list[[j]]) <- c(head(colnames(cses4_sub.list[[j]]), ncol(cses4_sub.list[[j]])-length(new)),new)
}

for(i in names(cses4_sub.list)){
  m1 <- as.matrix(cses4_sub.list[[i]][,c("which_close_party_election",new)])
  m1[!duplicated(m1),]
  logic <- apply(m1, 1, function(x){length(unique(x))==1})
  temp <- cbind(m1, logic)
  temp[!duplicated(temp),]	
  temp[which(temp[,"logic"]=="TRUE"), new] <- NA
  temp[!duplicated(temp),]		
  cses4_sub.list[[i]][new] <- temp[,new]
  print(cses4_sub.list[[i]][!duplicated(cses4_sub.list[[i]][,c("which_close_party_election",new)]),c("which_close_party_election",new)])
}

######################
## Bring back the cses lists into dataframe format
#####################

c4_back <- ldply(cses4_sub.list, .id=NULL)
c3_back <- ldply(cses3_sub.list, .id=NULL)
c2_back <- ldply(cses2_sub.list, .id=NULL)
c1_back <- ldply(cses1_sub.list, .id=NULL)

######################
## Organize questions on turnout.
#####################

# cses4:

c4_back$voted <- c4_back$D3005_LH
c4_back$voted[c4_back$voted >= 6] <- NA

# cses3:

c3_back $voted <- c3_back $C3021_1
c3_back $voted[c3_back $voted >= 6] <- NA

# cses2:

c2_back $voted <- c2_back $B3004_1
c2_back $voted[c2_back $voted >= 6] <- NA

# cses1:

c1_back $voted <- c1_back $A2028
c1_back $voted[c1_back $voted >= 6 | c1_back $voted ==0] <- NA

######################
## Organize individual-level variables: education (D2003, C2003, B2003, A2003), gender (D2002, C2002, B2002, A2002), age (C2001, B2001, A2001, D2001_Y), income (D2020, C2020, B2020, A2012)
#####################

# cses4:

c4_back$educ <- c4_back$D2003
c4_back$educ[c4_back$educ >= 97] <- NA
c4_back$educ[c4_back$educ == 96] <- 0

c4_back$male <- c4_back$D2002
c4_back$male[c4_back$male == 9] <- NA
c4_back$male[c4_back$male == 2] <- 0

c4_back$age <- c4_back$D2001_Y
c4_back$age[c4_back$age >= 9997] <- NA
c4_back$age <- c4_back$D1008-c4_back$age

c4_back$income <- c4_back$D2020
c4_back$income[c4_back$income >= 6] <- NA

# cses3:

c3_back$educ <- c3_back$C2003
c3_back$educ[c3_back$educ >= 9] <- NA

c3_back $male <- c3_back $C2002
c3_back $male[c3_back $male == 9] <- NA
c3_back $male[c3_back $male == 2] <- 0

c3_back$age <- c3_back$C2001
c3_back$age[c3_back$age >= 997] <- NA

c3_back $income <- c3_back$C2020
c3_back $income[c3_back$income >= 6] <- NA

# cses2:

c2_back$educ <- c2_back$B2003
c2_back$educ[c2_back$educ >= 9] <- NA

c2_back $male <- c2_back $B2002
c2_back $male[c2_back $male == 9] <- NA
c2_back $male[c2_back $male == 2] <- 0

c2_back $age <- c2_back$B2001
c2_back $age[c2_back $age >= 997] <- NA

c2_back $income <- c2_back$B2020
c2_back $income[c2_back $income >= 6] <- NA

# cses1:

c1_back $educ <- c1_back $A2003
c1_back $educ[c1_back $educ == 9] <- NA

c1_back $male <- c1_back $A2002
c1_back $male[c1_back $male == 9] <- NA
c1_back $male[c1_back $male == 2] <- 0

c1_back $age <- c1_back$A2001
c1_back $age[c1_back $age >= 998] <- NA

c1_back $income <- c1_back $A2012
c1_back $income[c1_back$income >= 8] <- NA

######################
## Combine the cses's using common columns and organize the variables.
#####################

c1_back$country_election <- c1_back$A1004
c2_back$country_election <- c2_back$B1004
c3_back$country_election <- c3_back$C1004
c4_back$country_election <- c4_back$D1004

a <- rbind(c1_back[,c("country_election", "voted", "which_close_party_election",new,"educ","age","male","income")], 
           c2_back[,c("country_election", "voted", "which_close_party_election",new,"educ","age","male","income")], 
           c3_back[,c("country_election", "voted", "which_close_party_election",new,"educ","age","male","income")], 
           c4_back[,c("country_election", "voted", "which_close_party_election",new,"educ","age","male","income")])

a$niche <- as.numeric(as.character(a$niche))
a$prev_enp_nat <- as.numeric(as.character(a$prev_enp_nat))
a$rile <- as.numeric(as.character(a$rile))

a$country_name <- substr(a$country_election, 1, 3)

# Change 5 and 2 to 0, which is did not vote
a$voted[a$voted==2 | a$voted==5] <- 0

#######################
####### aes 2010
#######################

# Read in aes 2010
aes10 <- read.spss("aes_2010_01228.sav", to.data.frame=T)

# Clean up variables:

aes10$id <- aes10$b1
aes10$id <- as.character(aes10$id)
aes10$id[aes10 $id=="No party"] <- NA
aes10$id <- mapvalues(aes10$id, c("Liberal","National (Country) Party","Labor","Greens"), c(1,3,2,4))

aes10$voted <- aes10$b9reps
aes10$voted <- as.character(aes10$voted)
aes10$voted[aes10$voted =="Voted informal/Did not vote"] <- 0
aes10$voted[aes10$voted !="Voted informal/Did not vote" & !is.na(aes10$voted) & aes10$voted!="0"] <- 1
aes10$voted <- as.numeric(aes10$voted)

#######################
#### Assign mpd's party number to aes10.
#######################

# Create variable for closest party using mpd's party number
aes10$which_id_mpd <- mapvalues(aes10$id, lookup.list[["AUS_2010"]]$aes_b1_party_num, lookup.list[["AUS_2010"]]$party_number)
aes10$which_id_mpd[aes10 $which_id_mpd== 63621] <- NA

# Assign party_election in lookup form to aes10.
aes10$which_id_party_election <- mapvalues(aes10$which_id_mpd, lookup.list[["AUS_2010"]]$party_number, as.character(lookup.list[["AUS_2010"]]$party_election))

#######################
####### nzes 1999
#######################

# Read in the data
load("nzes99.rdata")

# Clean up variables:

nzes99$id <- nzes99$sptisan
nzes99$id[which(nzes99$id==0)] <- NA

nzes99$voted <- nzes99$VOT99P
nzes99$voted[nzes99$voted!=0 & !is.na(nzes99$voted)] <- 1

#######################
#### Assign mpd's party number to nzes99.
#######################

# Create variable for closest party using mpd's party number
nzes99$which_id_mpd <- mapvalues(nzes99 $id, lookup.list[["NZL_1999"]]$nzes_d1_party_num, lookup.list[["NZL_1999"]]$party_number)

# Assign party_election in lookup form to nzes99.
nzes99 $which_id_party_election <- mapvalues(nzes99 $which_id_mpd, lookup.list[["NZL_1999"]]$party_number, as.character(lookup.list[["NZL_1999"]]$party_election))

#######################
####### nzes 2005
#######################

# Read in 
nzes05 <- read.spss("NZES_Release_05.sav", to.data.frame=T) 

# Clean up variables:

nzes05$id <- nzes05$yptisan
nzes05 $id <- as.character(nzes05 $id)
nzes05 $id[nzes05 $id=="Nonpartisan"] <- NA
nzes05$id <- mapvalues(nzes05$id, c("Labour","National","Green","Progress","Act"), c(1,2,3,8,5))

nzes05 $voted <- nzes05 $yvot05p
nzes05 $voted <- as.character(nzes05 $voted)
nzes05 $voted[nzes05 $voted =="Did Not"] <- 0
nzes05 $voted[nzes05 $voted != "0" & !is.na(nzes05$voted)] <- 1
nzes05 $voted <- as.numeric(nzes05 $voted)

#######################
#### Assign mpd's party number to nzes05.
#######################

# Create variable for closest party using mpd's party number
nzes05 $which_id_mpd <- mapvalues(nzes05$id, lookup.list[["NZL_2005"]]$nzes_d1_party_num, lookup.list[["NZL_2005"]]$party_number)

# Assign party_election in lookup form to nzes05.
nzes05 $which_id_party_election <- mapvalues(nzes05 $which_id_mpd, lookup.list[["NZL_2005"]]$party_number, as.character(lookup.list[["NZL_2005"]]$party_election))

#######################
####### ces 2015
#######################

# Read in
load("CES2015_Combined_R.RData") 
ces15 <- CES2015_Combined

# Clean up variables:

ces15 $id <- ces15 $partyid
ces15 $id[ces15 $id==1000] <- NA
ces15 $id[ces15 $id==9] <- NA
ces15 $id[ces15 $id==8] <- NA
ces15 $id[ces15 $id==6] <- NA

ces15 $voted <- ces15 $p_voted
ces15 $voted[!is.na(ces15$p_votedlong)] <- ces15$p_votedlong[!is.na(ces15$p_votedlong)]
ces15 $voted[ces15 $voted ==1000] <- NA
ces15 $voted[ces15 $voted ==8] <- NA
ces15 $voted[ces15 $voted ==9] <- NA
ces15 $voted[ces15 $voted ==5] <- 0

#######################
#### Assign mpd's party number to ces15.
#######################

# Create variable for closest party using mpd's party number
ces15$which_id_mpd <- mapvalues(ces15 $id, lookup.list[["CAN_2015"]]$ces_partyid_party_num, lookup.list[["CAN_2015"]]$party_number)

# Assign party_election in lookup form to ces15.
ces15 $which_id_party_election <- mapvalues(ces15 $which_id_mpd, lookup.list[["CAN_2015"]]$party_number, as.character(lookup.list[["CAN_2015"]]$party_election))

#######################
####### anes 1992
#######################

# Read in 
anes92 <- read.dta("NES1992.dta")

# Clean up variables:

anes92$id <- anes92$V923631
anes92$id <- as.character(anes92$id)
anes92 $id[anes92 $id=="3. NO PREFERENCE"] <- NA
anes92 $id[anes92 $id=="8. DK"] <- NA
anes92 $id[anes92 $id=="2. INDEPENDENT"] <- NA
anes92 $id[anes92 $id=="9. NA"] <- NA
anes92 $id[anes92 $id=="5. DEMOCRAT"] <- 5
anes92 $id[anes92 $id=="1. REPUBLICAN"] <- 1

anes92 $voted <- anes92 $V925601
anes92$voted <- as.character(anes92 $voted)
anes92 $voted[anes92 $voted =="9. NA"] <- NA
anes92 $voted[anes92 $voted =="0. INAP"] <- NA
anes92 $voted[anes92 $voted =="5. NO, DID NOT VOTE"] <- 0
anes92 $voted[anes92 $voted =="1. YES, DID VOTE"] <- 1
anes92$voted <- as.numeric(anes92 $voted)

#######################
#### Assign mpd's party number to anes92. 
#######################

# Create variable for closest party using mpd's party number
anes92$which_id_mpd <- mapvalues(anes92 $id, lookup.list[["USA_1992"]]$anes_900317_party_num, lookup.list[["USA_1992"]]$party_number)

# Assign party_election in lookup form to anes92.
anes92 $which_id_party_election <- mapvalues(anes92 $which_id_mpd, lookup.list[["USA_1992"]]$party_number, as.character(lookup.list[["USA_1992"]]$party_election))

######################
## Use party_election in supp and which_id_party_election in election studies to assign niche and prev_enp_nat and rile to them.
#####################

# Add in the variables:

new <- c("rile","niche","prev_enp_nat")

# ces15
for(i in new){
  ces15 <- data.frame(ces15, assign(i, mapvalues(ces15 $which_id_party_election, supp$party_election, supp[,i])))
}
colnames(ces15) <- c(head(colnames(ces15), ncol(ces15)-length(new)),new)
m1 <- as.matrix(ces15[,c("which_id_party_election",new)])
logic <- apply(m1, 1, function(x){length(unique(x))==1})
temp <- cbind(m1, logic)
temp[which(temp[,"logic"]=="TRUE"), new] <- NA
ces15[new] <- temp[,new]

# aes10
for(i in new){
  aes10 <- data.frame(aes10, assign(i, mapvalues(aes10 $which_id_party_election, supp$party_election, supp[,i])))
}
colnames(aes10) <- c(head(colnames(aes10), ncol(aes10)-length(new)),new)
values <- c("Other party (please specify)")
m1 <- as.matrix(aes10[,new])
m1[m1 %in% values] <- NA
aes10[new] <- m1

# nzes05
for(i in new){
  nzes05 <- data.frame(nzes05, assign(i, mapvalues(nzes05 $which_id_party_election, supp$party_election, supp[,i])))
}
colnames(nzes05) <- c(head(colnames(nzes05), ncol(nzes05)-length(new)),new)
values <- c("NZ First","Maori","United F","Another","Destiny","Nat Front","Social Credit")
m1 <- as.matrix(nzes05[,new])
m1[m1 %in% values] <- NA
nzes05[new] <- m1

# anes92
for(i in new){
  anes92 <- data.frame(anes92, assign(i, mapvalues(anes92 $which_id_party_election, supp$party_election, supp[,i])))
}
colnames(anes92) <- c(head(colnames(anes92), ncol(anes92)-length(new)),new)
m1 <- as.matrix(anes92[,c("which_id_party_election",new)])
logic <- apply(m1, 1, function(x){length(unique(x))==1})
temp <- cbind(m1, logic)
temp[which(temp[,"logic"]=="TRUE"), new] <- NA
anes92[new] <- temp[,new]

# nzes99
for(i in new){
  nzes99 <- data.frame(nzes99, assign(i, mapvalues(nzes99 $which_id_party_election, supp$party_election, supp[,i])))
}
colnames(nzes99) <- c(head(colnames(nzes99), ncol(nzes99)-length(new)),new)
m1 <- as.matrix(nzes99[,c("which_id_party_election",new)])
logic <- apply(m1, 1, function(x){length(unique(x))==1})
temp <- cbind(m1, logic)
temp[which(temp[,"logic"]=="TRUE"), new] <- NA
nzes99[new] <- temp[,new]

######################
## Organize individual-level variables
#####################

# ces15

ces15$male <- ces15$sex_r
ces15$male[ces15$male==5] <- 0

ces15$educ <- ces15$education
ces15$educ <- mapvalues(ces15$educ, c(1:11,98,99,1000), c(1:11,NA,NA,NA))

ces15$age <- 2015-ces15$age
ces15$age[ces15$age < 0] <- NA
ces15$age[ces15$age > 1000] <- NA

ces15$income <- ces15$income_grp
ces15$income[ces15$income == 1000] <- NA

# aes10

aes10$male <- as.character(aes10$h1)
aes10$male <- as.numeric(mapvalues(aes10$male, c("Female","Male"), c(0,1)))

aes10$educ <- aes10 $g2
aes10$educ <- as.character(aes10$educ)
aes10$educ[which(aes10$educ=="9 or more")] <- 9

unique(aes10$age)

aes10$income <- as.numeric(mapvalues(aes10$h15, levels(aes10$h15), 1:22))

# nzes05

nzes05 $male <- as.character(nzes05$ysex)
nzes05$male <- as.numeric(mapvalues(nzes05$male, c("Male","Female"), c(1,0)))

nzes05$educ <- mapvalues(nzes05$yeducate, levels(nzes05 $yeducate), 1:8)
unique(nzes05$educ)
nzes05$educ <- as.numeric(as.character(nzes05$educ))

nzes05$age <- nzes05$YAGE

nzes05$income <- mapvalues(nzes05$yhincum, levels(nzes05$yhincum), c(1:8, NA))
nzes05$income <- as.numeric(as.character(nzes05$income))

# anes92

anes92$male <- as.character(anes92 $V924201)
anes92$male[anes92$male=="1. MALE"] <- 1
anes92$male[anes92$male=="2. FEMALE"] <- 0
anes92$male <- as.numeric(anes92$male)

anes92$educ <- mapvalues(anes92 $V923908, levels(anes92 $V923908), c(NA,1:7,NA,NA))
anes92$educ <- as.numeric(as.character(anes92$educ))

anes92$age <- anes92 $V923903

anes92$income <- mapvalues(anes92 $V924104, levels(anes92 $V924104), c(1:24, rep(NA,5)))
anes92$income <- as.numeric(as.character(anes92$income))

# nzes99

nzes99$male <- nzes99$ssex
nzes99$male[nzes99$male==2] <- 0

nzes99$age <- nzes99$sage

nzes99$educ <- nzes99$sedqual

nzes99$income <- nzes99$shincum
nzes99$income <- mapvalues(nzes99$income, 1:9, c(NA,1:8))

######################
## Combine the data using common columns and organize variables.
#####################

ces15$country_election <- "CAN_2015"
aes10$country_election <- "AUS_2010"
nzes05$country_election <- "NZL_2005"
anes92$country_election <- "USA_1992"
nzes99$country_election <- "NZL_1999"

b <- rbind(ces15[,c("country_election", "voted", "which_id_party_election",new,"educ","age","male","income")], 
           aes10[,c("country_election", "voted", "which_id_party_election",new,"educ","age","male","income")], 
           nzes05[,c("country_election", "voted", "which_id_party_election",new,"educ","age","male","income")],
           anes92[,c("country_election", "voted", "which_id_party_election",new,"educ","age","male","income")], 
           nzes99[,c("country_election", "voted", "which_id_party_election",new,"educ","age","male","income")])

b$niche <- as.numeric(as.character(b$niche))
b$prev_enp_nat <- as.numeric(as.character(b$prev_enp_nat))
b$rile <- as.numeric(as.character(b$rile))

b$country_name <- substr(b$country_election, 1, 3)

#######################
####### Combine the non-cses data with cses data
###########################

# Turn which_close_party_election to which_id_party_election. Leave other columns in a as is.
colnames(a) <- colnames(b)
combined <- merge(a,b,all=T)

dat <- combined

###############################
# Make country_election variables in same format
###############################

supp$country_election <- as.character(supp$country_election)
supp$country_election <- substr(supp$country_election, 1, nchar(supp$country_election)-2)

dat$country_name <- mapvalues(dat$country_name, unique(dat$country_name), c("Australia","Canada","United Kingdom","Ireland","New Zealand","United States"))
dat$country_election <- paste(dat$country_name, substr(dat$country_election, nchar(dat$country_election)-3, nchar(dat$country_election)))

##########################
###### Standardize the educ variable and income variable
##########################

# Split dat to list
temp <- split(dat, dat$country_election)

# Change all to numeric
for(i in names(temp)){
  temp[[i]]$educ <- as.numeric(as.character(temp[[i]]$educ))
}

# Normalize educ variable to range from 0 to 1, for each country election

# For cses 4:

u <- temp[["Australia 2013"]]$educ
temp[["Australia 2013"]]$educ_scaled <- (u - 0)/diff(c(0,9))

u <- temp[["Ireland 2011"]]$educ
temp[["Ireland 2011"]]$educ_scaled <- (u - 2)/diff(c(2,7))

u <- temp[["New Zealand 2011"]]$educ
temp[["New Zealand 2011"]]$educ_scaled <- (u - 1)/diff(c(1,8))

u <- temp[["United States 2012"]]$educ
temp[["United States 2012"]]$educ_scaled <- (u - 1)/diff(c(1,9))

# For cses 3:

u <- temp[["Australia 2007"]]$educ
temp[["Australia 2007"]]$educ_scaled <- (u - 1)/diff(c(1,8))

u <- temp[["Ireland 2007"]]$educ
temp[["Ireland 2007"]]$educ_scaled <- (u - 1)/diff(c(1,8))

u <- temp[["New Zealand 2008"]]$educ
temp[["New Zealand 2008"]]$educ_scaled <- (u - 2)/diff(c(2,8))

u <- temp[["United States 2008"]]$educ
temp[["United States 2008"]]$educ_scaled <- (u - 1)/diff(c(1,8))

# For cses 2: 

u <- temp[["Australia 2004"]]$educ
temp[["Australia 2004"]]$educ_scaled <- (u - 1)/diff(c(1,8))

u <- temp[["New Zealand 2002"]]$educ
temp[["New Zealand 2002"]]$educ_scaled <- (u - 2)/diff(c(2,8))

u <- temp[["United States 2004"]]$educ
temp[["United States 2004"]]$educ_scaled <- (u - 1)/diff(c(1,8))

# For cses 1:

u <- temp[["United Kingdom 1997"]]$educ
temp[["United Kingdom 1997"]]$educ_scaled <- (u - 1)/diff(c(1,8))

u <- temp[["New Zealand 1996"]]$educ
temp[["New Zealand 1996"]]$educ_scaled <- (u - 2)/diff(c(2,8))

# For non-cses:

u <- temp[["Australia 2010"]]$educ
temp[["Australia 2010"]]$educ_scaled <-  (u - 0)/diff(c(0,9))

u <- temp[["Canada 2015"]]$educ
temp[["Canada 2015"]]$educ_scaled <- (u - 1)/diff(c(1,11))

u <- temp[["New Zealand 2005"]]$educ
temp[["New Zealand 2005"]]$educ_scaled <- (u - 1)/diff(c(1,8))

u <- temp[["United States 1992"]]$educ
temp[["United States 1992"]]$educ_scaled <- (u - 1)/diff(c(1,7))

u <- temp[["New Zealand 1999"]]$educ
temp[["New Zealand 1999"]]$educ_scaled <- (u - 1)/diff(c(1,6))

# Normalize income variable to range from 0 to 1, for each country election
for(i in names(temp)){
  u <- temp[[i]]$income
  temp[[i]]$income_scaled <- (u - min(u,na.rm=T))/diff(range(u, na.rm=T))
}

# Bring back list to data frame
dat <- ldply(temp, .id=NULL)

##########################
###### Create party variable and create age variable in decades
##########################

dat$which_id_party <- substr(dat$which_id_party_election, start=1, stop=nchar(dat$which_id_party_election)-7)

dat$age <- as.numeric(dat$age)
dat$age_decades <- (dat$age * 0.1)

##########################
###### Add mr measure to dat
##########################

# Read in the measure
mr <- read.csv("Data_mr.csv")

# Assign the measure.
dat$mr <- as.numeric(mapvalues(dat$which_id_party_election, mr$party_election, mr$prop_moral_qs))
dat$mr[!(grepl("_",dat$which_id_party_election))] <- NA

##########################
###### Delete observations with missing mr values
##########################

dat <- dat[-which(is.na(dat$mr)),]

##########################
###### Delete columns that are not going to be analyzed
##########################

dat <- dat[,-which(colnames(dat) %in% c("age","income","educ"))]

##########################
###### Save the data 
##########################

study1 <- dat
save(study1, file="Study1.RData")
