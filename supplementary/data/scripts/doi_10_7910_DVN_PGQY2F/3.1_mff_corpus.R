#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script creates a corpus for the MFF debates
# then it does the same for the API version

##########################
# Intro
library(quanteda)#This code was built with quanteda version 1.3.13
library(stringr)#This code was built with stringr version 1.3.1
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(readtext)#This code was built with readtext version 0.71

# Check if all packages are the ones we used
if(packageVersion("quanteda")=="1.3.13"&
   packageVersion("stringr")=="1.3.1"&
   packageVersion("tidyverse")=="1.2.1"&
   packageVersion("readtext")=="0.71"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

# Budget Contributions Dataset
budget_contr=read.csv("data/eu_budgetcontr.csv",header=TRUE,sep=";",dec=",",stringsAsFactors = F)
budget_contr$government=toupper(budget_contr$government)

#Import speech data
mff.text <- readtext("data/mff/Youtube/*.txt")
mff <- corpus_segment(corpus(mff.text), "##*")
#Add document-level identifiers
meetings <- row.names(docvars(mff))
docvars(mff, "no_mff_meeting") <- as.integer(substring(meetings, 1, 4))
docvars(mff, "no_debate") <- as.integer(str_sub(meetings, -5, -5)) 
docvars(mff, "no_suffix") <- gsub("_", "", substring(meetings, 5, 5))
docvars(mff, "presidency") <- 0
docvars(mff, "actor") <- gsub("##","",docvars(mff, "pattern"))

meetingsno <- docvars(mff, "no_mff_meeting")
actorname <- docvars(mff, "actor")
docvars(mff, "actormeeting") <- paste(docvars(mff, "no_mff_meeting"),docvars(mff, "actor"))

# Designate Presidency speeches
table(docvars(mff,"no_mff_meeting"))
docvars(mff, "presidency")[meetingsno%in%c(3125,3132)&actorname=="PL"] <- 1
docvars(mff, "presidency")[meetingsno%in%c(3143,3158,3160,3168,3180)&actorname=="DK"] <- 1
docvars(mff, "presidency")[meetingsno%in%c(3184,3187)&actorname=="CY"] <- 1
docvars(mff, "presidency")[meetingsno%in%c(3235,3240)&actorname=="IE"] <- 1
docvars(mff, "presidency")[meetingsno%in%c(3368)&actorname=="LV"] <- 1
docvars(mff, "presidency")[meetingsno%in%c(3484,3494)&actorname=="SK"] <- 1
#Get rid of METAINFO observations
mff <- corpus_subset(mff, pattern != "##METAINFO")
#Get rid of Presidency, Croatia and NA actors
table(docvars(mff, "no_mff_meeting"))
mff.pre <- corpus_subset(mff, presidency==0&actor!="HR"&actor!="NA")
#Construct placeholder to gather results
speechdata<-data.frame(actor=docvars(mff.pre, "actor"),
                       meeting=as.numeric(docvars(mff.pre, "no_mff_meeting")),stringsAsFactors = F)
speechdata<-speechdata%>%
  mutate(
    Date=case_when(
      meeting==3125 ~ "2011-11-15",
      meeting==3132 ~ "2011-12-05",
      meeting==3143 ~ "2012-01-27",
      meeting==3158 ~ "2012-03-26",
      meeting==3160 ~ "2012-04-24",
      meeting==3168 ~ "2012-05-29",
      meeting==3180 ~ "2012-06-26",
      meeting==3184 ~ "2012-07-24",
      meeting==3187 ~ "2012-09-24",
      meeting==3235 ~ "2013-04-22",
      meeting==3240 ~ "2013-05-21",
      meeting==3368 ~ "2015-02-15",
      meeting==3484 ~ "2016-09-20",
      meeting==3494 ~ "2016-10-18",
      TRUE ~ "Other"
    )
  )
#Define time periods for Dynamic IRT
speechdata$Date=as.Date(speechdata$Date)
speechdata$Year=as.numeric(format(speechdata$Date,'%Y'))
speechdata$index=paste(speechdata$meeting,speechdata$actor)
speechdata=speechdata[!duplicated(speechdata$index),]

save(speechdata,mff.pre,budget_contr,file="generated_data/mffdata.RData")

#####################################################
# Do it all again for the API
#####################################################

#Import speech data
mff.text.api <- readtext("data/mff/API/*.txt")
mff.api <- corpus_segment(corpus(mff.text.api), "##*")
#Add document-level identifiers
meetings <- row.names(docvars(mff.api))
docvars(mff.api, "no_mff_meeting") <- as.integer(substring(meetings, 1, 4))
docvars(mff.api, "no_suffix") <- gsub("_", "", substring(meetings, 5, 5))
docvars(mff.api, "presidency") <- 0
docvars(mff.api, "actor") <- gsub("##","",docvars(mff.api, "pattern"))
docvars(mff.api, "actor") <- gsub("#","",docvars(mff.api, "actor"))

meetingsno <- docvars(mff.api, "no_mff_meeting")
actorname <- docvars(mff.api, "actor")
docvars(mff.api, "actormeeting") <- paste(docvars(mff.api, "no_mff_meeting"),docvars(mff.api, "actor"))

# Designate Presidency speeches
table(docvars(mff.api,"no_mff_meeting"))
docvars(mff.api, "presidency")[meetingsno%in%c(3125,3132)&actorname=="PL"] <- 1
docvars(mff.api, "presidency")[meetingsno%in%c(3143,3158,3160,3168,3180)&actorname=="DK"] <- 1
docvars(mff.api, "presidency")[meetingsno%in%c(3184,3187)&actorname=="CY"] <- 1
docvars(mff.api, "presidency")[meetingsno%in%c(3235,3240)&actorname=="IE"] <- 1
docvars(mff.api, "presidency")[meetingsno%in%c(3368)&actorname=="LV"] <- 1
docvars(mff.api, "presidency")[meetingsno%in%c(3484,3494)&actorname=="SK"] <- 1
table(docvars(mff.api, "no_mff_meeting"))
#Get rid of METAINFO observations
mff.api <- corpus_subset(mff.api, pattern != "##METAINFO")
#Get rid of Presidency, Croatia and NA actors
mff.api.pre <- corpus_subset(mff.api, presidency==0&actor!="HR"&actor!="NA")
#Construct placeholder to gather results
speechdata.api<-data.frame(actor=docvars(mff.api.pre, "actor"),
                           meeting=as.numeric(docvars(mff.api.pre, "no_mff_meeting")),stringsAsFactors = F)
speechdata.api<-speechdata.api%>%
  mutate(
    Date=case_when(
      meeting==3125 ~ "2011-11-15",
      meeting==3132 ~ "2011-12-05",
      meeting==3143 ~ "2012-01-27",
      meeting==3158 ~ "2012-03-26",
      meeting==3160 ~ "2012-04-24",
      meeting==3168 ~ "2012-05-29",
      meeting==3180 ~ "2012-06-26",
      meeting==3184 ~ "2012-07-24",
      meeting==3187 ~ "2012-09-24",
      meeting==3235 ~ "2013-04-22",
      meeting==3240 ~ "2013-05-21",
      meeting==3368 ~ "2015-02-15",
      meeting==3484 ~ "2016-09-20",
      meeting==3494 ~ "2016-10-18",
      TRUE ~ "Other"
    )
  )
#Define time periods for Dynamic IRT
speechdata.api$Date=as.Date(speechdata.api$Date)
speechdata.api$Year=as.numeric(format(speechdata.api$Date,'%Y'))
speechdata.api$index=paste(speechdata.api$meeting,speechdata.api$actor)
speechdata.api=speechdata.api[!duplicated(speechdata.api$index),]

table(docvars(mff,"actor"),docvars(mff,"no_mff_meeting"))
table(docvars(mff.api,"actor"),docvars(mff.api,"no_mff_meeting"))

table(docvars(mff,"actor"))
table(docvars(mff.api,"actor"))

save(speechdata.api,mff.api.pre,budget_contr,file="generated_data/mffapidata.RData")