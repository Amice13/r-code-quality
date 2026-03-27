# Library
install.packages(c("gdata","plyr","texreg"))
library(gdata)
library(plyr)
library(texreg)

# Read in wvs Wave 6.
load("WV6_Data_R_v_2016_01_01.rdata")
wvs6 <- WV6_Data_R
rm(WV6_Data_R)

################
#### ORGANIZE VARIABLES
###################

# Ideological self-placement (V95, 1 left ~ 10 right)
wvs6$ideo <- wvs6$V95
wvs6$ideo[wvs6$ideo < 0]  <- NA

# Party preference if national election tomorrow (V228, CS Codes)
wvs6$party_pref <- wvs6$V228

# Country (V2)
wvs6$country <- wvs6$V2

# BFI-10 items:
# is reserved: V160A
# is generally trusting: V160B
# tends to be lazy: V160C
# is relaxed, handles stress well: V160D
# has few artistic interests: V160E
# is outgoing, sociable (V160F)
# tends to find fault with others (V160G)
# does a thorough job (V160H)
# gets nervous easily (V160I)
# has an active imagination (V160J)

# Code missing values to NA
trait.vars <- c("V160A", "V160B", "V160C", "V160D", "V160E", "V160F", "V160G", "V160H", "V160I", "V160J")
wvs6[trait.vars][wvs6[trait.vars] < 0] <- NA

# Reverse code and save variables (1 disagree strongly ~ 5 agree strongly)
wvs6$o1 <- wvs6$V160J
wvs6$o2 <- (wvs6$V160E-6)*-1

wvs6$c1 <- wvs6$V160H
wvs6$c2 <- (wvs6$V160C-6)*-1

wvs6$e1 <- wvs6 $V160F
wvs6$e2 <- (wvs6 $V160A-6)*-1

wvs6$a1 <-wvs6 $V160B
wvs6$a2 <-(wvs6 $V160G-6)*-1

wvs6$s1 <-wvs6 $V160D
wvs6$s2 <-(wvs6$V160I-6)*-1

# Average the two items.
wvs6$o <- (wvs6$o1 + wvs6$o2)/2
wvs6$c <- (wvs6$c1 + wvs6$c2)/2
wvs6$e <- (wvs6$e1 + wvs6$e2)/2
wvs6$a <- (wvs6$a1 + wvs6$a2)/2
wvs6$s <- (wvs6$s1 + wvs6$s2)/2

# Scale to range from 0 to 1. Higher values indicate more of the trait.
wvs6$o_std <- (wvs6$o-1)/(5-1)
wvs6$c_std <- (wvs6$c-1)/(5-1)
wvs6$e_std <- (wvs6$e-1)/(5-1)
wvs6$a_std <- (wvs6$a-1)/(5-1)
wvs6$s_std <- (wvs6$s-1)/(5-1)

# Sex
wvs6$male <-wvs6 $V240
wvs6$male[wvs6 $male <0] <-NA
wvs6$male[wvs6 $male ==2] <- 0

# Age
wvs6$age <- wvs6$V242
wvs6$age[wvs6$age < 0] <- NA

# Age squared
wvs6$age_sq <- (wvs6$age)^2

# Survey year
wvs6$year <- wvs6$V262

# Education
wvs6$educ <- wvs6$V248
wvs6$educ[wvs6$educ < 0] <- NA

# Income
wvs6$income <- wvs6$V239
wvs6$income[wvs6$income < 0] <- NA

################
#### SUBSET DATA
###################

# Reduce to European countries in the data
wvs6_europe <- subset(wvs6, country==51 | country==31 | country == 112 | country == 196 | country==233 | country==268 | country==276 | country==528 |country==616 |country==642 |country==643 |country==705 |country==724 |country==752|country==792|country==804)

# Reduce to countries with personality measures
wvs6_personality <- subset(wvs6, country==268 | country==276 | country == 528)

################
#### ORGANIZE party_pref CODINGS
###################

# Read in the codebook (party codes for WVS)
lookup <- read.xls("F00003843_WVS_EVS_Integrated_Dictionary_Codebook_v_2014_09_22.xls", sheet=6, header=F)

# Reduce the lookup to Georgia 268, Germany 276, and Netherlands 528
lookup_personality <- lookup[substr(lookup[,1],start=1,stop=3) %in% c("268","276","528"),]

# Reduce the lookup to parties covered in the WVS
# Only save parties in lookup that exist in wvs6_personality$party_pref
lookup_personality <- lookup_personality[lookup_personality $V1 %in% unique(wvs6_personality$party_pref),]

# Add survey year to lookup
lookup_personality$survey_year <- mapvalues(substr(lookup_personality$V2,start=1,stop=2), c("GE","DE","NL"),c(2014,2013,2012))

# Save the lookup for countries with personality measures
# write.csv(lookup_personality, file="Lookup_party_&_ideology_personality.csv",row.names=F)

# We've filled up the ches_party_id column of the lookup_personality csv file based on the CHES codebook

# Read in the CHES dataset
ches <- read.csv("1999-2014_CHES_dataset_means.csv",stringsAsFactors=F)

# Reduce ches to Germany and the Netherlands. Georgia is not included in ches.
ches_personality <- subset(ches, country=="ge"|country=="nl")

# Check the years for both countries in the ches
ches_personality <- subset(ches_personality, year==2014)

# Read in the modified csv file
lookup_personality_filled <- read.csv("Lookup_party_&_ideology_personality.csv",stringsAsFactors=F)

# Assign cmp_id, family, and lrgen (ranges from 0 to 10) to lookup_personality_filled
lookup_personality_filled $cmp_id <- mapvalues(lookup_personality_filled $ches_party_id, ches_personality$party_id, ches_personality $cmp_id)
lookup_personality_filled $ches_family <- mapvalues(lookup_personality_filled $ches_party_id, ches_personality $party_id, ches_personality $family)
lookup_personality_filled $ches_lrgen <- mapvalues(lookup_personality_filled $ches_party_id, ches_personality $party_id, ches_personality $lrgen)

# Create variable called left/right in lookup_personality_filled
lookup_personality_filled $right_from_family <- as.numeric(mapvalues(lookup_personality_filled $ches_family, unique(lookup_personality_filled $ches_family), c(NA,1,0,1,0,0,1,NA,1)))
lookup_personality_filled$ches_lrgen[lookup_personality_filled$ches_lrgen=="extreme right"] <- NA
lookup_personality_filled $right_from_lrgen <- ifelse(lookup_personality_filled $ches_lrgen >5, 1, 0) # In CHES, the variable lrgen ranges from 0 to 10. 5 is center.

# Switch single digits to NA
wvs6_personality$party_pref[which(wvs6_personality$party_pref<10)] <- NA

# Assign NA to party_pref for observations of parties not covered in ches
wvs6_personality$party_pref[!(wvs6_personality$party_pref %in% lookup_personality_filled$V1)] <- NA

# Add right_from_family and right_from_lrgen to party_pref in wvs6_personality
wvs6_personality$party_pref_right_from_ches_family <- mapvalues(wvs6_personality $party_pref, lookup_personality_filled$V1, lookup_personality_filled$right_from_family)
wvs6_personality$party_pref_right_from_ches_lrgen <- mapvalues(wvs6_personality $party_pref, lookup_personality_filled$V1, lookup_personality_filled $right_from_lrgen)

################
#### Table OA1.1
###################

ideo.all <- lm(ideo ~ o + c + e + a + s + male + age + age_sq + educ + income, data=wvs6)

ideo.europe <- lm(ideo ~ o + c + e + a + s + male + age + age_sq + educ + income, data= wvs6_europe)

texreg(list(ideo.all, ideo.europe), stars=c(0.01,0.05,0.1))

##############
###### Table OA1.2
#######################

biv.pref.family.all <- glm(party_pref_right_from_ches_family ~ o + c + e + a + s + male + age + age_sq + educ + income, data=wvs6_personality, family="binomial")

biv.pref.lrgen.all <- glm(party_pref_right_from_ches_lrgen ~ o + c + e + a + s + male + age + age_sq + educ + income, data=wvs6_personality, family="binomial")

texreg(list(biv.pref.family.all,biv.pref.lrgen.all),stars=c(0.01,0.05,0.1))

#################
##### Table OA1.3
########################

cor.test(wvs6$o, wvs6$ideo)
cor.test(wvs6$c, wvs6$ideo) 
cor.test(wvs6$e, wvs6$ideo) 
cor.test(wvs6$a, wvs6$ideo)
cor.test(wvs6$s, wvs6$ideo)

cor.test(wvs6_europe$o, wvs6_europe$ideo) 
cor.test(wvs6_europe$c, wvs6_europe$ideo) 
cor.test(wvs6_europe$e, wvs6_europe$ideo) 
cor.test(wvs6_europe$a, wvs6_europe$ideo)
cor.test(wvs6_europe$s, wvs6_europe$ideo)