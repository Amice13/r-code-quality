# Libraries
install.packages(c("plyr","foreign"))
library(foreign)
library(plyr)

##############################
##### Read in the data
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
#### Turn answers to anger questions into numeric
##############################

bes15$grnAngryW4 <- as.numeric(as.character(mapvalues(bes15$grnAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$conAngryW4 <- as.numeric(as.character(mapvalues(bes15$conAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$labAngryW4 <- as.numeric(as.character(mapvalues(bes15$labAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$ldAngryW4 <- as.numeric(as.character(mapvalues(bes15$ldAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$snpAngryW4 <- as.numeric(as.character(mapvalues(bes15$snpAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$pcAngryW4 <- as.numeric(as.character(mapvalues(bes15$pcAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))
bes15$ukipAngryW4 <- as.numeric(as.character(mapvalues(bes15$ukipAngryW4, c("Yes","No","Don't know"), c(1,0,NA))))

##############################
#### Organize party id strength
##############################

bes15$id_strength <- as.numeric(as.character(mapvalues(bes15$partyIdStrengthW4, levels(bes15$partyIdStrengthW4), c(3,2,1,NA))))

##############################
##### Subset the data to identifiers
##############################

bes15 <- subset(bes15,partyIdW4!="No - none" & !is.na(partyIdW4) & partyIdW4!="Don't know" & partyIdW4!="Other" & partyIdW4!="British National Party (BNP)") # BNP is excluded because emotion question was not asked for BNP.
bes15$partyIdW4 <- droplevels(bes15$partyIdW4)

##############################
#### Calculate average hostility toward other parties, excluding NA answers
##############################

green <- subset(bes15, partyIdW4=="Green Party")
green$avg_angry <- apply(green[,c("conAngryW4","labAngryW4","ldAngryW4","snpAngryW4","pcAngryW4","ukipAngryW4")],1,function(x){mean(x,na.rm=T)})

con <- subset(bes15, partyIdW4=="Conservative")
con$avg_angry <- apply(con[,c("grnAngryW4","labAngryW4","ldAngryW4","snpAngryW4","pcAngryW4","ukipAngryW4")],1,function(x){mean(x,na.rm=T)})

lab <- subset(bes15, partyIdW4=="Labour")
lab$avg_angry <- apply(lab[,c("grnAngryW4","conAngryW4","ldAngryW4","snpAngryW4","pcAngryW4","ukipAngryW4")],1,function(x){mean(x,na.rm=T)})

ld <- subset(bes15, partyIdW4=="Liberal Democrat")
ld$avg_angry <- apply(ld[,c("grnAngryW4","conAngryW4","labAngryW4","snpAngryW4","pcAngryW4","ukipAngryW4")],1,function(x){mean(x,na.rm=T)})

snp <- subset(bes15, partyIdW4=="Scottish National Party (SNP)")
snp$avg_angry <- apply(snp[,c("grnAngryW4","conAngryW4","labAngryW4","ldAngryW4","pcAngryW4","ukipAngryW4")],1,function(x){mean(x,na.rm=T)})

pc <- subset(bes15, partyIdW4=="Plaid Cymru")
pc$avg_angry <- apply(pc[,c("grnAngryW4","conAngryW4","labAngryW4","ldAngryW4","snpAngryW4","ukipAngryW4")],1,function(x){mean(x,na.rm=T)})

ukip <- subset(bes15, partyIdW4=="United Kingdom Independence Party (UKIP)")
ukip$avg_angry <- apply(ukip[,c("grnAngryW4","conAngryW4","labAngryW4","ldAngryW4","snpAngryW4","pcAngryW4")],1,function(x){mean(x,na.rm=T)})

##############################
#### Create variables for hope
##############################

green$hope <- subset(bes15, partyIdW4=="Green Party")$grnHopeW4

con$hope <- subset(bes15, partyIdW4=="Conservative")$conHopeW4

lab$hope <- subset(bes15, partyIdW4=="Labour")$labHopeW4

ld$hope <- subset(bes15, partyIdW4=="Liberal Democrat")$ldHopeW4

snp$hope <- subset(bes15, partyIdW4=="Scottish National Party (SNP)")$snpHopeW4

pc$hope <- subset(bes15, partyIdW4=="Plaid Cymru")$pcHopeW4

ukip$hope <- subset(bes15, partyIdW4=="United Kingdom Independence Party (UKIP)")$ukipHopeW4

##############################
#### Create variables for pride
##############################

green$pride <- subset(bes15, partyIdW4=="Green Party")$grnPrideW4

con$pride <- subset(bes15, partyIdW4=="Conservative")$conPrideW4

lab$pride <- subset(bes15, partyIdW4=="Labour")$labPrideW4

ld$pride <- subset(bes15, partyIdW4=="Liberal Democrat")$ldPrideW4

snp$pride <- subset(bes15, partyIdW4=="Scottish National Party (SNP)")$snpPrideW4

pc$pride <- subset(bes15, partyIdW4=="Plaid Cymru")$pcPrideW4

ukip$pride <- subset(bes15, partyIdW4=="United Kingdom Independence Party (UKIP)")$ukipPrideW4

##############################
#### Combine and organize the data
##############################

study3 <- rbind(con, lab, ld, snp, pc, ukip, green)

study3$hope <- as.numeric(as.character(mapvalues(study3$hope, c("Yes","No","Don't know"), c(1,0,NA))))
study3$pride <- as.numeric(as.character(mapvalues(study3$pride, c("Yes","No","Don't know"), c(1,0,NA))))

##############################
#### Subset the data to variables I will use
##############################

study3 <- study3[,c("age_dec","male","educ","income","id_strength","hope","pride","turnout","any_mobil","did_cheap","did_costly","avg_angry")]

##############################
#### Save the data
##############################

save(study3, file="Study3.RData")
