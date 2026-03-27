# Libraries
install.packages(c("reporttools","texreg","foreign","plyr","readstata13","MASS"))
library(reporttools)
library(texreg)
library(foreign)
library(plyr)
library(readstata13)
library(MASS)

##############################
##### Read in the organized data
##############################

load("Study3.RData")

##############################
#### Table SI7.1
##############################

tableContinuous(vars=data.frame(study3$age_dec, study3$educ, study3$income, study3$male, study3$id_strength, study3$pride, study3$hope, study3$turnout, study3$any_mobil), longtable=F, cap="Descriptive Statistics", stats=c("min","max","median","mean","s"),pre=2)

##############
##### Models 1 and 2 in Table SI7.2
################

cheap_glm <- glm(did_cheap ~ pride+hope+ age_dec + educ + income + male+id_strength, data=study3, family="binomial") 
costly_glm <- glm(did_costly ~ pride+hope+ age_dec + educ + income + male+id_strength, data=study3, family="binomial") 

###############
##### Models 3 and 4 in Table SI7.2
################

turnout_mod_incl_anger <- glm(turnout ~ pride+hope+ avg_angry + age_dec + educ + income + male+id_strength, data=study3, family="binomial") 
mobil_mod_incl_anger <- glm(any_mobil ~ pride+hope+ avg_angry + age_dec + educ + income + male+id_strength, data=study3, family="binomial") 

###############
##### Models 5 and 6 in Table SI7.2
################

turnout_without_id_strength <- glm(turnout ~ pride+hope + age_dec + educ + income + male, data=study3, family="binomial") 
mobil_without_id_strength <- glm(any_mobil ~ pride+hope + age_dec + educ + income + male, data=study3, family="binomial")

##############################
##### Read in the BES to create data with partisanship from wave 3
##############################

bes15 <- read.spss("BES2015_W6_Panel_v1.2.sav", to.data.frame=T)

##############################
#### Organize sociodemographic variables
##############################

bes15$age <- bes15$Age
bes15$age_dec <- (bes15$age)*0.1

bes15$male <- ifelse(bes15$gender=="Male",1,0)

bes15$educ <- as.character(bes15$education)
bes15$educ[bes15$educ=="Don't know"] <- NA
bes15$educ[bes15$educ=="Prefer not to say"] <- NA
bes15$educ <- as.numeric(mapvalues(bes15$educ, c("GCE A level or Higher Certificate","City and Guild certificate - advanced" ,"University or CNAA first degree (eg BA, B.Sc, B.Ed)", "CSE grade 1, GCE O level, GCSE, School Certificate", "City and Guild certificate" ,"No formal qualifications", "University diploma", "Other technical, professional or higher qualification","onc", "Teaching qualification (not degree)" ,"University or CNAA higher degree (eg M.Sc, Ph.D)" , "Youth training certificate/skillseekers", "Recognised trade apprenticeship completed","Nursing qualification (eg SEN, SRN, SCM, RGN)","Clerical and commercial","CSE grades 2-5", "Scottish Ordinary/ Lower Certificate", "Scottish Higher Certificate" ), c(5,4,7,4,3,2,6,8,4,6,8,3,3,6,3,3,4,5)))

bes15$income <- as.numeric(as.character(mapvalues(bes15$profile_gross_household, levels(bes15$profile_gross_household), c(1:15,NA,NA))))

##############################
#### Organize turnout
##############################

bes15$turnout <- as.numeric(as.character(mapvalues(bes15$genElecTurnoutRetroW6, levels(bes15$genElecTurnoutRetroW6), c(0,1,NA))))

##############################
#### Organize activism questions
##############################

bes15$work <- as.numeric(as.character(mapvalues(bes15$participation_1W5, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$money <- as.numeric(as.character(mapvalues(bes15$participation_2W5, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$display <- as.numeric(as.character(mapvalues(bes15$participation_3W5, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$persuade <- as.numeric(as.character(mapvalues(bes15$participation_6W5, c("Yes","No","Don't know"), c(1,0,NA))))

bes15$mobil_num_total <- apply(bes15[,c("work","money","display","persuade")], 1, sum)

bes15$any_mobil <- ifelse(bes15$mobil_num_total>=1,1,0)

bes15$did_costly <- apply(bes15[,c("work","money")], 1, function(x){
  if(any(x %in% 1)){
    1
  }else{if(any(x %in% 0)){
    0
  }else{
    NA
  }
  }})

bes15$did_cheap <- apply(bes15[,c("display","persuade")], 1, function(x){
  if(any(x %in% 1)){
    1
  }else{if(any(x %in% 0)){
    0
  }else{
    NA
  }
  }})

##############################
#### Organize party id strength, using id from wave 3.
##############################

bes15$id_strength_w3 <- as.numeric(as.character(mapvalues(bes15$partyIdStrengthW3, levels(bes15$partyIdStrengthW3), c(3,2,1,NA))))

##############################
##### Subset the data to identifiers, using data from wave 3.
##############################

bes15_w3 <- subset(bes15,partyIdW3!="No - none" & !is.na(partyIdW3) & partyIdW3!="Don't know" & partyIdW3!="Other" & partyIdW3!="British National Party (BNP)") # BNP is excluded because emotion question was not asked for BNP.
bes15_w3$partyIdW3 <- droplevels(bes15_w3$partyIdW3)

##############################
#### Create data for hope
##############################

green <- subset(bes15_w3, partyIdW3=="Green Party")
green$hope <- subset(bes15_w3, partyIdW3=="Green Party")$grnHopeW4

con <- subset(bes15_w3, partyIdW3=="Conservative")
con$hope <- subset(bes15_w3, partyIdW3=="Conservative")$conHopeW4

lab <- subset(bes15_w3, partyIdW3=="Labour")
lab$hope <- subset(bes15_w3, partyIdW3=="Labour")$labHopeW4

ld <- subset(bes15_w3, partyIdW3=="Liberal Democrat")
ld$hope <- subset(bes15_w3, partyIdW3=="Liberal Democrat")$ldHopeW4

snp <- subset(bes15_w3, partyIdW3=="Scottish National Party (SNP)")
snp$hope <- subset(bes15_w3, partyIdW3=="Scottish National Party (SNP)")$snpHopeW4

pc <- subset(bes15_w3, partyIdW3=="Plaid Cymru")
pc$hope <- subset(bes15_w3, partyIdW3=="Plaid Cymru")$pcHopeW4

ukip <- subset(bes15_w3, partyIdW3=="United Kingdom Independence Party (UKIP)")
ukip$hope <- subset(bes15_w3, partyIdW3=="United Kingdom Independence Party (UKIP)")$ukipHopeW4

##############################
#### Create data for pride
##############################

green$pride <- subset(bes15_w3, partyIdW3=="Green Party")$grnPrideW4

con$pride <- subset(bes15_w3, partyIdW3=="Conservative")$conPrideW4

lab$pride <- subset(bes15_w3, partyIdW3=="Labour")$labPrideW4

ld$pride <- subset(bes15_w3, partyIdW3=="Liberal Democrat")$ldPrideW4

snp$pride <- subset(bes15_w3, partyIdW3=="Scottish National Party (SNP)")$snpPrideW4

pc$pride <- subset(bes15_w3, partyIdW3=="Plaid Cymru")$pcPrideW4

ukip$pride <- subset(bes15_w3, partyIdW3=="United Kingdom Independence Party (UKIP)")$ukipPrideW4

##############################
#### Combine and organize the data
##############################

study3_w3 <- rbind(con, lab, ld, snp, pc, ukip, green)

study3_w3$hope <- as.numeric(as.character(mapvalues(study3_w3$hope, c("Yes","No","Don't know"), c(1,0,NA))))
study3_w3$pride <- as.numeric(as.character(mapvalues(study3_w3$pride, c("Yes","No","Don't know"), c(1,0,NA))))

###############
##### Models 7 and 8 in Table SI7.2
################

turnout_wave3 <- glm(turnout ~ pride+hope + age_dec + educ + income + male+id_strength_w3, data=study3_w3, family="binomial") 
mobil_wave3 <- glm(any_mobil ~ pride+hope + age_dec + educ + income + male+id_strength_w3, data=study3_w3, family="binomial") 

###############
##### Table SI7.2
################

texreg(list(cheap_glm,costly_glm, turnout_mod_incl_anger, mobil_mod_incl_anger, turnout_without_id_strength, mobil_without_id_strength, turnout_wave3, mobil_wave3), stars=c(0.05))

##############################
##### Read in the BES data that has values questions
##############################

bes15 <- read.dta13("BES2017_W13_Panel_v1.2.dta")

##############################
#### Organize values variables
##############################

# Greater values mean more individualizing morality
bes15$lr3 <- as.numeric(as.character(mapvalues(bes15$lr3W1W2W3W4W5, levels(bes15$lr3W1W2W3W4W5), c(1:5,NA))))

# Greater values mean more binding morality
bes15$al3 <- as.numeric(as.character(mapvalues(bes15$al3W1W2W3W4W5, levels(bes15$al3W1W2W3W4W5), c(1:5,NA))))

##############################
#### Organize demographic variables
##############################

bes15$age <- bes15$Age
bes15$age_dec <- (bes15$Age)*0.1

bes15$male <- ifelse(bes15$gender=="Male",1,0)

bes15$educ <- as.character(bes15$educationW1_W6)
bes15$educ[bes15$educ=="Don't know"] <- NA
bes15$educ[bes15$educ=="Prefer not to say"] <- NA
bes15$educ <- as.numeric(mapvalues(bes15$educ, c("GCE A level or Higher Certificate","City and Guild certificate - advanced" ,"University or CNAA first degree (eg BA, B.Sc, B.Ed)", "CSE grade 1, GCE O level, GCSE, School Certificate", "City and Guild certificate" ,"No formal qualifications", "University diploma", "Other technical, professional or higher qualification","onc", "Teaching qualification (not degree)" ,"University or CNAA higher degree (eg M.Sc, Ph.D)" , "Youth training certificate/skillseekers", "Recognised trade apprenticeship completed","Nursing qualification (eg SEN, SRN, SCM, RGN)","Clerical and commercial","CSE grades 2-5", "Scottish Ordinary/ Lower Certificate", "Scottish Higher Certificate" ), c(5,4,7,4,3,2,6,8,4,6,8,3,3,6,3,3,4,5)))
unique(bes15$educ)

bes15$income <- as.numeric(as.character(mapvalues(bes15$profile_gross_household, levels(bes15$profile_gross_household), c(1:15,NA,NA))))

##############################
#### Organize party id strength from wave 1
##############################

bes15$id_strength <- as.numeric(as.character(mapvalues(bes15$partyIdStrengthW1, levels(bes15$partyIdStrengthW1), c(3,2,1,NA))))

##############################
#### Subset the data to identifiers, using wave 1
##############################

bes15 <- subset(bes15,partyIdW1!="No - none" & !is.na(partyIdW1) & partyIdW1!="Don't know" & partyIdW1!="Other" & partyIdW1!="British National Party (BNP)") # BNP is excluded because emotion question was not asked for BNP.
bes15$partyIdW1 <- droplevels(bes15$partyIdW1)

##############################
#### Create data for pride
##############################

green <- subset(bes15, partyIdW1=="Green Party")
green$pride <- subset(bes15, partyIdW1=="Green Party")$grnPrideW4

con <- subset(bes15, partyIdW1=="Conservative")
con$pride <- subset(bes15, partyIdW1=="Conservative")$conPrideW4

lab <- subset(bes15, partyIdW1=="Labour")
lab$pride <- subset(bes15, partyIdW1=="Labour")$labPrideW4

ld <- subset(bes15, partyIdW1=="Liberal Democrat")
ld$pride <- subset(bes15, partyIdW1=="Liberal Democrat")$ldPrideW4

snp <- subset(bes15, partyIdW1=="Scottish National Party (SNP)")
snp$pride <- subset(bes15, partyIdW1=="Scottish National Party (SNP)")$snpPrideW4

pc <- subset(bes15, partyIdW1=="Plaid Cymru")
pc$pride <- subset(bes15, partyIdW1=="Plaid Cymru")$pcPrideW4

ukip <- subset(bes15, partyIdW1=="United Kingdom Independence Party (UKIP)")
ukip$pride <- subset(bes15, partyIdW1=="United Kingdom Independence Party (UKIP)")$ukipPrideW4

##############################
#### Create data for hope
##############################

green$hope <- subset(bes15, partyIdW1=="Green Party")$grnHopeW4

con$hope <- subset(bes15, partyIdW1=="Conservative")$conHopeW4

lab$hope <- subset(bes15, partyIdW1=="Labour")$labHopeW4

ld$hope <- subset(bes15, partyIdW1=="Liberal Democrat")$ldHopeW4

snp$hope <- subset(bes15, partyIdW1=="Scottish National Party (SNP)")$snpHopeW4

pc$hope <- subset(bes15, partyIdW1=="Plaid Cymru")$pcHopeW4

ukip$hope <- subset(bes15, partyIdW1=="United Kingdom Independence Party (UKIP)")$ukipHopeW4

##############################
#### Combine and organize the data
##############################

study3_si <- rbind(con, lab, ld, snp, pc, ukip, green)

study3_si$hope <- as.numeric(as.character(mapvalues(study3_si$hope, c("Yes","No","Don't know"), c(1,0,NA))))
study3_si$pride <- as.numeric(as.character(mapvalues(study3_si$pride, c("Yes","No","Don't know"), c(1,0,NA))))

##############################
#### Split the data by party and create emotion variable (ranges from 0 to 2)
##############################

lab <- subset(study3_si, partyIdW1=="Labour")
cons <- subset(study3_si, partyIdW1=="Conservative")

lab$emo <- apply(lab[,c("hope","pride")],1, function(x){sum(x%in%1)})
lab$emo[which(is.na(lab$pride)|is.na(lab$hope))] <- NA

cons$emo <- apply(cons[,c("hope","pride")],1, function(x){sum(x%in%1)})
cons$emo[which(is.na(cons$pride)|is.na(cons$hope))] <- NA

##############################
#### Table SI7.3
##############################

lab_mod <- polr(as.factor(emo) ~ lr3 + al3+  age_dec + educ + income + male + id_strength, data=lab, method="logistic")
cons_mod <- polr(as.factor(emo) ~ lr3 + al3+  age_dec + educ + income + male + id_strength, data=cons, method="logistic")

texreg(list(lab_mod, cons_mod), stars=c(0.05))
