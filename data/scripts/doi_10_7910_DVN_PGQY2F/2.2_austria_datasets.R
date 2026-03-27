#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script runs Wordfish models on the corpora created in 2.1.
# It then adds in CHES data for the model in different ways (means, medians)
# Then it does the same for the appendix version

##########################
# Intro
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(austin)#This code was built with austin version 0.3.0
library(quanteda)#This code was built with quanteda version 1.3.13

# Check if all packages are the ones we used
if(packageVersion("tidyverse")=="1.2.1"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("austin")=="0.3.0"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

load("generated_data/Austria_Out.RData")

########
# Get the 2014 CHES file here and store it in the "data" folder: https://www.chesdata.eu/2014-chapel-hill-expert-survey
chesexp=read.csv("data/2014_CHES_dataset_expert-level.csv",stringsAsFactors = F)
chesexp=chesexp%>%filter(cname=="aus")
chesexp=chesexp%>%select(party_name,galtan,immigrate_policy,lrgen)

chesexp$lrgen[chesexp$lrgen=="extreme left"]=0
chesexp$lrgen[chesexp$lrgen=="center"]=5
chesexp$lrgen[chesexp$lrgen=="extreme right"]=10
chesexp$lrgen=as.numeric(chesexp$lrgen)

chesexp$immigrate_policy[chesexp$immigrate_policy=="fully supports restrictive immigration policy"]=10
chesexp$immigrate_policy[chesexp$immigrate_policy=="fully opposed to restrictive immigration policy"]=0
chesexp$immigrate_policy=as.numeric(chesexp$immigrate_policy)

chesexp$galtan[chesexp$galtan=="libertarian/postmaterialist"]=0
chesexp$galtan[chesexp$galtan=="center"]=5
chesexp$galtan[chesexp$galtan=="traditional/authoritarian"]=10
chesexp$galtan=as.numeric(chesexp$galtan)

AustriaLR=chesexp%>%group_by(party_name)%>%
  summarise(lrgen=mean(lrgen,na.rm=T),
            immigrate_policy=mean(immigrate_policy,na.rm=T),
            galtan=mean(galtan,na.rm=T))

##############################################################
# Wordfish Analysis of whole corpus
##############################################################
summary(textcorpus.austria)
DFM.austria <- dfm(textcorpus.austria, remove=c(stopwords("german")), 
                   remove_punct = TRUE,remove_numbers=TRUE)
DFM.austria=dfm_trim(DFM.austria,min_termfreq  =  3)
wf.austria<-austin::wordfish(quanteda::as.wfm(DFM.austria),dir=c(3,4),verbose=T)
sum.wf.austria=data.frame(theta=predict(wf.austria,interval="confidence"))
names(sum.wf.austria)=c("theta","se","lower","upper")
info<-str_split(row.names(sum.wf.austria),"_")
info <- do.call(rbind,info)
info[,3]<-gsub(".txt","",info[,3])

sum.wf.austria$Name<-info[,3]
sum.wf.austria$date<-as.Date(info[,2],"%d%m%Y")
sum.wf.austria$debate<-info[,1]
sum.wf.austria$rownames=row.names(sum.wf.austria)

sum.wf.austria<-sum.wf.austria%>%
  mutate(
    mode=case_when(
      grepl("elefanten",sum.wf.austria$debate) ~ "full",
      grepl("sommer",sum.wf.austria$debate) ~ "single",
      TRUE ~ "duel"
    )
  )

sum.wf.austria=sum.wf.austria%>%
  mutate(theta.abs=abs(sum.wf.austria$theta))

sum.wf.austria<-sum.wf.austria%>%
  mutate(
    party_name=case_when(
      Name=="kern" ~ "SPO",
      Name=="kurz" ~ "OVP",
      Name=="lunacek" ~ "GRUNE",
      Name=="strache" ~ "FPO",
      Name=="strolz" ~ "NEOS",
      TRUE ~ "other"
    )
  )

sum.wf.austria<-sum.wf.austria%>%filter(Name!="pilz")

sum.wf.austria<-sum.wf.austria%>%
  mutate(
    oppo_party=case_when(
      grepl("kern",debate)&party_name!="SPO" ~ "SPO",
      (grepl("kurz",debate)|grepl("moser",debate))&party_name!="OVP" ~ "OVP",
      grepl("lunacek",debate)&party_name!="GRUNE" ~ "GRUNE",
      (grepl("strache",debate)|grepl("hofer",debate))&party_name!="FPO" ~ "FPO",
      (grepl("strolz",debate)|grepl("griss",debate))&party_name!="NEOS" ~ "NEOS",
      TRUE ~ "ALL"
    )
  )

AustriaLR.to.join=AustriaLR[,c("lrgen","immigrate_policy","party_name","galtan")]

sum.wf.austria=left_join(x=sum.wf.austria,y=AustriaLR.to.join)

lr=data.frame(aggregate(sum.wf.austria$lrgen,by=list(sum.wf.austria$party_name),mean))
lr[6,]=c("ALL_NO_FPO",mean(as.numeric(lr[c(2,3,4,5),2]),na.rm=T))
lr[7,]=c("ALL_NO_GRUNE",mean(as.numeric(lr[c(1,3,4,5),2]),na.rm=T))
lr[8,]=c("ALL_NO_NEOS",mean(as.numeric(lr[c(1,2,4,5),2]),na.rm=T))
lr[9,]=c("ALL_NO_OVP",mean(as.numeric(lr[c(1,2,3,5),2]),na.rm=T))
lr[10,]=c("ALL_NO_SPO",mean(as.numeric(lr[c(1,2,3,4),2]),na.rm=T))

migr=data.frame(aggregate(sum.wf.austria$immigrate_policy,by=list(sum.wf.austria$party_name),mean))
migr[6,]=c("ALL_NO_FPO",mean(as.numeric(migr[c(2,3,4,5),2]),na.rm=T))
migr[7,]=c("ALL_NO_GRUNE",mean(as.numeric(migr[c(1,3,4,5),2]),na.rm=T))
migr[8,]=c("ALL_NO_NEOS",mean(as.numeric(migr[c(1,2,4,5),2]),na.rm=T))
migr[9,]=c("ALL_NO_OVP",mean(as.numeric(migr[c(1,2,3,5),2]),na.rm=T))
migr[10,]=c("ALL_NO_SPO",mean(as.numeric(migr[c(1,2,3,4),2]),na.rm=T))

galtan=data.frame(aggregate(sum.wf.austria$galtan,by=list(sum.wf.austria$party_name),mean))
galtan[6,]=c("ALL_NO_FPO",mean(as.numeric(galtan[c(2,3,4,5),2]),na.rm=T))
galtan[7,]=c("ALL_NO_GRUNE",mean(as.numeric(galtan[c(1,3,4,5),2]),na.rm=T))
galtan[8,]=c("ALL_NO_NEOS",mean(as.numeric(galtan[c(1,2,4,5),2]),na.rm=T))
galtan[9,]=c("ALL_NO_OVP",mean(as.numeric(galtan[c(1,2,3,5),2]),na.rm=T))
galtan[10,]=c("ALL_NO_SPO",mean(as.numeric(galtan[c(1,2,3,4),2]),na.rm=T))

sum.wf.austria<-sum.wf.austria%>%
  mutate(
    oppo_lr=case_when(
      oppo_party=="SPO" ~ lr$x[lr$Group.1=="SPO"],
      oppo_party=="FPO" ~ lr$x[lr$Group.1=="FPO"],
      oppo_party=="OVP" ~ lr$x[lr$Group.1=="OVP"],
      oppo_party=="GRUNE" ~ lr$x[lr$Group.1=="GRUNE"],
      oppo_party=="NEOS" ~ lr$x[lr$Group.1=="NEOS"],
      oppo_party=="ALL"&party_name=="SPO" ~ lr$x[lr$Group.1=="ALL_NO_SPO"],
      oppo_party=="ALL"&party_name=="FPO" ~ lr$x[lr$Group.1=="ALL_NO_FPO"],
      oppo_party=="ALL"&party_name=="OVP" ~ lr$x[lr$Group.1=="ALL_NO_OVP"],
      oppo_party=="ALL"&party_name=="GRUNE" ~ lr$x[lr$Group.1=="ALL_NO_GRUNE"],
      oppo_party=="ALL"&party_name=="NEOS" ~ lr$x[lr$Group.1=="ALL_NO_NEOS"],
      TRUE ~ "ALL"
    ))
sum.wf.austria<-sum.wf.austria%>%
  mutate(
    oppo_migr=case_when(
      oppo_party=="SPO" ~ migr$x[migr$Group.1=="SPO"],
      oppo_party=="FPO" ~ migr$x[migr$Group.1=="FPO"],
      oppo_party=="OVP" ~ migr$x[migr$Group.1=="OVP"],
      oppo_party=="GRUNE" ~ migr$x[migr$Group.1=="GRUNE"],
      oppo_party=="NEOS" ~ migr$x[migr$Group.1=="NEOS"],
      oppo_party=="ALL"&party_name=="SPO" ~ migr$x[migr$Group.1=="ALL_NO_SPO"],
      oppo_party=="ALL"&party_name=="FPO" ~ migr$x[migr$Group.1=="ALL_NO_FPO"],
      oppo_party=="ALL"&party_name=="OVP" ~ migr$x[migr$Group.1=="ALL_NO_OVP"],
      oppo_party=="ALL"&party_name=="GRUNE" ~ migr$x[migr$Group.1=="ALL_NO_GRUNE"],
      oppo_party=="ALL"&party_name=="NEOS" ~ migr$x[migr$Group.1=="ALL_NO_NEOS"],
      TRUE ~ "ALL"
    ))

sum.wf.austria$oppo_galtan=NA
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="SPO"]=galtan$x[galtan$Group.1=="SPO"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="FPO"]=galtan$x[galtan$Group.1=="FPO"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="OVP"]=galtan$x[galtan$Group.1=="OVP"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="GRUNE"]=galtan$x[galtan$Group.1=="GRUNE"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="NEOS"]=galtan$x[galtan$Group.1=="NEOS"]

sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="SPO"]=galtan$x[galtan$Group.1=="ALL_NO_SPO"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="FPO"]=galtan$x[galtan$Group.1=="ALL_NO_FPO"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="OVP"]=galtan$x[galtan$Group.1=="ALL_NO_OVP"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="GRUNE"]=galtan$x[galtan$Group.1=="ALL_NO_GRUNE"]
sum.wf.austria$oppo_galtan[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="NEOS"]=galtan$x[galtan$Group.1=="ALL_NO_NEOS"]

sum.wf.austria$oppo_lr=as.numeric(sum.wf.austria$oppo_lr)
sum.wf.austria$oppo_migr=as.numeric(sum.wf.austria$oppo_migr)
sum.wf.austria$oppo_galtan=as.numeric(sum.wf.austria$oppo_galtan)

################################################
# Median

chesexp.med=chesexp%>%select(party_name,galtan,immigrate_policy,lrgen)

chesexp.med$lrgen[chesexp.med$lrgen=="extreme left"]=0
chesexp.med$lrgen[chesexp.med$lrgen=="center"]=5
chesexp.med$lrgen[chesexp.med$lrgen=="extreme right"]=10
chesexp.med$lrgen=as.numeric(chesexp.med$lrgen)

chesexp.med$immigrate_policy[chesexp.med$immigrate_policy=="fully supports restrictive immigration policy"]=10
chesexp.med$immigrate_policy[chesexp.med$immigrate_policy=="fully opposed to restrictive immigration policy"]=0
chesexp.med$immigrate_policy=as.numeric(chesexp.med$immigrate_policy)

chesexp.med$galtan[chesexp.med$galtan=="libertarian/postmaterialist"]=0
chesexp.med$galtan[chesexp.med$galtan=="center"]=5
chesexp.med$galtan[chesexp.med$galtan=="traditional/authoritarian"]=10
chesexp.med$galtan=as.numeric(chesexp.med$galtan)

chesexp.med.to.join=chesexp.med%>%group_by(party_name)%>%
  summarise(med_lrgen=median(lrgen,na.rm=T),
            med_immigrate=median(immigrate_policy,na.rm=T),
            med_galtan=median(galtan,na.rm=T))

sum.wf.austria=left_join(x=sum.wf.austria,y=chesexp.med.to.join)

lr.med=data.frame(aggregate(sum.wf.austria$med_lrgen,by=list(sum.wf.austria$party_name),mean))
lr.med[6,]=c("ALL_NO_FPO",mean(as.numeric(lr.med[c(2,3,4,5),2]),na.rm=T))
lr.med[7,]=c("ALL_NO_GRUNE",mean(as.numeric(lr.med[c(1,3,4,5),2]),na.rm=T))
lr.med[8,]=c("ALL_NO_NEOS",mean(as.numeric(lr.med[c(1,2,4,5),2]),na.rm=T))
lr.med[9,]=c("ALL_NO_OVP",mean(as.numeric(lr.med[c(1,2,3,5),2]),na.rm=T))
lr.med[10,]=c("ALL_NO_SPO",mean(as.numeric(lr.med[c(1,2,3,4),2]),na.rm=T))

migr.med=data.frame(aggregate(sum.wf.austria$med_immigrate,by=list(sum.wf.austria$party_name),mean))
migr.med[6,]=c("ALL_NO_FPO",mean(as.numeric(migr.med[c(2,3,4,5),2]),na.rm=T))
migr.med[7,]=c("ALL_NO_GRUNE",mean(as.numeric(migr.med[c(1,3,4,5),2]),na.rm=T))
migr.med[8,]=c("ALL_NO_NEOS",mean(as.numeric(migr.med[c(1,2,4,5),2]),na.rm=T))
migr.med[9,]=c("ALL_NO_OVP",mean(as.numeric(migr.med[c(1,2,3,5),2]),na.rm=T))
migr.med[10,]=c("ALL_NO_SPO",mean(as.numeric(migr.med[c(1,2,3,4),2]),na.rm=T))

galtan.med=data.frame(aggregate(sum.wf.austria$med_galtan,by=list(sum.wf.austria$party_name),mean))
galtan.med[6,]=c("ALL_NO_FPO",mean(as.numeric(galtan.med[c(2,3,4,5),2]),na.rm=T))
galtan.med[7,]=c("ALL_NO_GRUNE",mean(as.numeric(galtan.med[c(1,3,4,5),2]),na.rm=T))
galtan.med[8,]=c("ALL_NO_NEOS",mean(as.numeric(galtan.med[c(1,2,4,5),2]),na.rm=T))
galtan.med[9,]=c("ALL_NO_OVP",mean(as.numeric(galtan.med[c(1,2,3,5),2]),na.rm=T))
galtan.med[10,]=c("ALL_NO_SPO",mean(as.numeric(galtan.med[c(1,2,3,4),2]),na.rm=T))

sum.wf.austria<-sum.wf.austria%>%
  mutate(
    oppo_lr_med=case_when(
      oppo_party=="SPO" ~ lr.med$x[lr.med$Group.1=="SPO"],
      oppo_party=="FPO" ~ lr.med$x[lr.med$Group.1=="FPO"],
      oppo_party=="OVP" ~ lr.med$x[lr.med$Group.1=="OVP"],
      oppo_party=="GRUNE" ~ lr.med$x[lr.med$Group.1=="GRUNE"],
      oppo_party=="NEOS" ~ lr.med$x[lr.med$Group.1=="NEOS"],
      oppo_party=="ALL"&party_name=="SPO" ~ lr.med$x[lr.med$Group.1=="ALL_NO_SPO"],
      oppo_party=="ALL"&party_name=="FPO" ~ lr.med$x[lr.med$Group.1=="ALL_NO_FPO"],
      oppo_party=="ALL"&party_name=="OVP" ~ lr.med$x[lr.med$Group.1=="ALL_NO_OVP"],
      oppo_party=="ALL"&party_name=="GRUNE" ~ lr.med$x[lr.med$Group.1=="ALL_NO_GRUNE"],
      oppo_party=="ALL"&party_name=="NEOS" ~ lr.med$x[lr.med$Group.1=="ALL_NO_NEOS"],
      TRUE ~ "ALL"
    ))
sum.wf.austria<-sum.wf.austria%>%
  mutate(
    oppo_migr_med=case_when(
      oppo_party=="SPO" ~ migr.med$x[migr.med$Group.1=="SPO"],
      oppo_party=="FPO" ~ migr.med$x[migr.med$Group.1=="FPO"],
      oppo_party=="OVP" ~ migr.med$x[migr.med$Group.1=="OVP"],
      oppo_party=="GRUNE" ~ migr.med$x[migr.med$Group.1=="GRUNE"],
      oppo_party=="NEOS" ~ migr.med$x[migr.med$Group.1=="NEOS"],
      oppo_party=="ALL"&party_name=="SPO" ~ migr.med$x[migr.med$Group.1=="ALL_NO_SPO"],
      oppo_party=="ALL"&party_name=="FPO" ~ migr.med$x[migr.med$Group.1=="ALL_NO_FPO"],
      oppo_party=="ALL"&party_name=="OVP" ~ migr.med$x[migr.med$Group.1=="ALL_NO_OVP"],
      oppo_party=="ALL"&party_name=="GRUNE" ~ migr.med$x[migr.med$Group.1=="ALL_NO_GRUNE"],
      oppo_party=="ALL"&party_name=="NEOS" ~ migr.med$x[migr.med$Group.1=="ALL_NO_NEOS"],
      TRUE ~ "ALL"
    ))

sum.wf.austria$oppo_galtan_med=NA
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="SPO"]=galtan.med$x[galtan.med$Group.1=="SPO"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="FPO"]=galtan.med$x[galtan.med$Group.1=="FPO"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="OVP"]=galtan.med$x[galtan.med$Group.1=="OVP"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="GRUNE"]=galtan.med$x[galtan.med$Group.1=="GRUNE"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="NEOS"]=galtan.med$x[galtan.med$Group.1=="NEOS"]

sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="SPO"]=galtan.med$x[galtan.med$Group.1=="ALL_NO_SPO"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="FPO"]=galtan.med$x[galtan.med$Group.1=="ALL_NO_FPO"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="OVP"]=galtan.med$x[galtan.med$Group.1=="ALL_NO_OVP"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="GRUNE"]=galtan.med$x[galtan.med$Group.1=="ALL_NO_GRUNE"]
sum.wf.austria$oppo_galtan_med[sum.wf.austria$oppo_party=="ALL"&sum.wf.austria$party_name=="NEOS"]=galtan.med$x[galtan.med$Group.1=="ALL_NO_NEOS"]

sum.wf.austria$oppo_lr_med=as.numeric(sum.wf.austria$oppo_lr_med)
sum.wf.austria$oppo_migr_med=as.numeric(sum.wf.austria$oppo_migr_med)
sum.wf.austria$oppo_galtan_med=as.numeric(sum.wf.austria$oppo_galtan_med)

sum.wf.austria$debate_party_unordered=paste(sum.wf.austria$party_name,sum.wf.austria$oppo_party)

sum.wf.austria<-sum.wf.austria%>%
  mutate(
    debate_party=case_when(
      debate_party_unordered=="SPO OVP" ~ "OVP SPO",
      debate_party_unordered=="SPO GRUNE" ~ "GRUNE SPO",
      debate_party_unordered=="FPO GRUNE" ~ "GRUNE FPO",
      debate_party_unordered=="FPO NEOS" ~ "NEOS FPO",
      debate_party_unordered=="FPO OVP" ~ "OVP FPO",
      debate_party_unordered=="FPO SPO" ~ "SPO FPO",
      debate_party_unordered=="GRUNE NEOS" ~ "NEOS GRUNE",
      debate_party_unordered=="GRUNE OVP" ~ "OVP GRUNE",
      debate_party_unordered=="NEOS OVP" ~ "OVP NEOS",
      debate_party_unordered=="NEOS SPO" ~ "SPO NEOS",
      TRUE ~ debate_party_unordered
    )
  )

sum.wf.austria$debate_party_all<-sum.wf.austria$debate_party
sum.wf.austria$debate_party_all[grepl("ALL",sum.wf.austria$debate_party)]="ALL"

##############################################################
# Wordfish Analysis of whole corpus for Appendix with Griss, Hofer, Moser
##############################################################

DFM.austria.app <- dfm(textcorpus.austria.appendix, remove=c(stopwords("german")), 
                       remove_punct = TRUE,remove_numbers=TRUE)
DFM.austria.app=dfm_trim(DFM.austria.app,min_termfreq  =  3)
wf.austria.app<-austin::wordfish(quanteda::as.wfm(DFM.austria.app),dir=c(3,4),verbose=T)
sum.wf.austria.app=data.frame(theta=predict(wf.austria.app,interval="confidence"))
names(sum.wf.austria.app)=c("theta","se","lower","upper")
info<-str_split(row.names(sum.wf.austria.app),"_")
info <- do.call(rbind,info)
info[,3]<-gsub(".txt","",info[,3])

sum.wf.austria.app$Name<-info[,3]
sum.wf.austria.app$date<-as.Date(info[,2],"%d%m%Y")
sum.wf.austria.app$debate<-info[,1]
sum.wf.austria.app$rownames=row.names(sum.wf.austria.app)

sum.wf.austria.app<-sum.wf.austria.app%>%
  mutate(
    mode=case_when(
      grepl("elefanten",sum.wf.austria.app$debate) ~ "full",
      grepl("sommer",sum.wf.austria.app$debate) ~ "single",
      TRUE ~ "duel"
    )
  )

sum.wf.austria.app=sum.wf.austria.app%>%
  mutate(theta.abs=abs(sum.wf.austria.app$theta))


sum.wf.austria.app<-sum.wf.austria.app%>%
  mutate(
    party_name=case_when(
      Name=="kern" ~ "SPO",
      Name%in%c("kurz","moser") ~ "OVP",
      Name=="lunacek" ~ "GRUNE",
      Name%in%c("strache","hofer") ~ "FPO",
      Name%in%c("strolz","griss") ~ "NEOS",
      TRUE ~ "other"
    )
  )

sum.wf.austria.app<-sum.wf.austria.app%>%filter(Name!="pilz")

sum.wf.austria.app<-sum.wf.austria.app%>%
  mutate(
    oppo_party=case_when(
      grepl("kern",debate)&party_name!="SPO" ~ "SPO",
      (grepl("kurz",debate)|grepl("moser",debate))&party_name!="OVP" ~ "OVP",
      grepl("lunacek",debate)&party_name!="GRUNE" ~ "GRUNE",
      (grepl("strache",debate)|grepl("hofer",debate))&party_name!="FPO" ~ "FPO",
      (grepl("strolz",debate)|grepl("griss",debate))&party_name!="NEOS" ~ "NEOS",
      TRUE ~ "ALL"
    )
  )

AustriaLR.to.join=AustriaLR[,c("lrgen","immigrate_policy","party_name","galtan")]

sum.wf.austria.app=left_join(x=sum.wf.austria.app,y=AustriaLR.to.join)

lr=data.frame(aggregate(sum.wf.austria.app$lrgen,by=list(sum.wf.austria.app$party_name),mean))
lr[6,]=c("ALL_NO_FPO",mean(as.numeric(lr[c(2,3,4,5),2]),na.rm=T))
lr[7,]=c("ALL_NO_GRUNE",mean(as.numeric(lr[c(1,3,4,5),2]),na.rm=T))
lr[8,]=c("ALL_NO_NEOS",mean(as.numeric(lr[c(1,2,4,5),2]),na.rm=T))
lr[9,]=c("ALL_NO_OVP",mean(as.numeric(lr[c(1,2,3,5),2]),na.rm=T))
lr[10,]=c("ALL_NO_SPO",mean(as.numeric(lr[c(1,2,3,4),2]),na.rm=T))

migr=data.frame(aggregate(sum.wf.austria.app$immigrate_policy,by=list(sum.wf.austria.app$party_name),mean))
migr[6,]=c("ALL_NO_FPO",mean(as.numeric(migr[c(2,3,4,5),2]),na.rm=T))
migr[7,]=c("ALL_NO_GRUNE",mean(as.numeric(migr[c(1,3,4,5),2]),na.rm=T))
migr[8,]=c("ALL_NO_NEOS",mean(as.numeric(migr[c(1,2,4,5),2]),na.rm=T))
migr[9,]=c("ALL_NO_OVP",mean(as.numeric(migr[c(1,2,3,5),2]),na.rm=T))
migr[10,]=c("ALL_NO_SPO",mean(as.numeric(migr[c(1,2,3,4),2]),na.rm=T))

galtan=data.frame(aggregate(sum.wf.austria.app$galtan,by=list(sum.wf.austria.app$party_name),mean))
galtan[6,]=c("ALL_NO_FPO",mean(as.numeric(galtan[c(2,3,4,5),2]),na.rm=T))
galtan[7,]=c("ALL_NO_GRUNE",mean(as.numeric(galtan[c(1,3,4,5),2]),na.rm=T))
galtan[8,]=c("ALL_NO_NEOS",mean(as.numeric(galtan[c(1,2,4,5),2]),na.rm=T))
galtan[9,]=c("ALL_NO_OVP",mean(as.numeric(galtan[c(1,2,3,5),2]),na.rm=T))
galtan[10,]=c("ALL_NO_SPO",mean(as.numeric(galtan[c(1,2,3,4),2]),na.rm=T))

sum.wf.austria.app<-sum.wf.austria.app%>%
  mutate(
    oppo_lr=case_when(
      oppo_party=="SPO" ~ lr$x[lr$Group.1=="SPO"],
      oppo_party=="FPO" ~ lr$x[lr$Group.1=="FPO"],
      oppo_party=="OVP" ~ lr$x[lr$Group.1=="OVP"],
      oppo_party=="GRUNE" ~ lr$x[lr$Group.1=="GRUNE"],
      oppo_party=="NEOS" ~ lr$x[lr$Group.1=="NEOS"],
      oppo_party=="ALL"&party_name=="SPO" ~ lr$x[lr$Group.1=="ALL_NO_SPO"],
      oppo_party=="ALL"&party_name=="FPO" ~ lr$x[lr$Group.1=="ALL_NO_FPO"],
      oppo_party=="ALL"&party_name=="OVP" ~ lr$x[lr$Group.1=="ALL_NO_OVP"],
      oppo_party=="ALL"&party_name=="GRUNE" ~ lr$x[lr$Group.1=="ALL_NO_GRUNE"],
      oppo_party=="ALL"&party_name=="NEOS" ~ lr$x[lr$Group.1=="ALL_NO_NEOS"],
      TRUE ~ "ALL"
    ))
sum.wf.austria.app<-sum.wf.austria.app%>%
  mutate(
    oppo_migr=case_when(
      oppo_party=="SPO" ~ migr$x[migr$Group.1=="SPO"],
      oppo_party=="FPO" ~ migr$x[migr$Group.1=="FPO"],
      oppo_party=="OVP" ~ migr$x[migr$Group.1=="OVP"],
      oppo_party=="GRUNE" ~ migr$x[migr$Group.1=="GRUNE"],
      oppo_party=="NEOS" ~ migr$x[migr$Group.1=="NEOS"],
      oppo_party=="ALL"&party_name=="SPO" ~ migr$x[migr$Group.1=="ALL_NO_SPO"],
      oppo_party=="ALL"&party_name=="FPO" ~ migr$x[migr$Group.1=="ALL_NO_FPO"],
      oppo_party=="ALL"&party_name=="OVP" ~ migr$x[migr$Group.1=="ALL_NO_OVP"],
      oppo_party=="ALL"&party_name=="GRUNE" ~ migr$x[migr$Group.1=="ALL_NO_GRUNE"],
      oppo_party=="ALL"&party_name=="NEOS" ~ migr$x[migr$Group.1=="ALL_NO_NEOS"],
      TRUE ~ "ALL"
    ))

sum.wf.austria.app$oppo_galtan=NA
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="SPO"]=galtan$x[galtan$Group.1=="SPO"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="FPO"]=galtan$x[galtan$Group.1=="FPO"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="OVP"]=galtan$x[galtan$Group.1=="OVP"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="GRUNE"]=galtan$x[galtan$Group.1=="GRUNE"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="NEOS"]=galtan$x[galtan$Group.1=="NEOS"]

sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="ALL"&sum.wf.austria.app$party_name=="SPO"]=galtan$x[galtan$Group.1=="ALL_NO_SPO"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="ALL"&sum.wf.austria.app$party_name=="FPO"]=galtan$x[galtan$Group.1=="ALL_NO_FPO"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="ALL"&sum.wf.austria.app$party_name=="OVP"]=galtan$x[galtan$Group.1=="ALL_NO_OVP"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="ALL"&sum.wf.austria.app$party_name=="GRUNE"]=galtan$x[galtan$Group.1=="ALL_NO_GRUNE"]
sum.wf.austria.app$oppo_galtan[sum.wf.austria.app$oppo_party=="ALL"&sum.wf.austria.app$party_name=="NEOS"]=galtan$x[galtan$Group.1=="ALL_NO_NEOS"]


sum.wf.austria.app$oppo_lr=as.numeric(sum.wf.austria.app$oppo_lr)
sum.wf.austria.app$oppo_migr=as.numeric(sum.wf.austria.app$oppo_migr)
sum.wf.austria.app$oppo_galtan=as.numeric(sum.wf.austria.app$oppo_galtan)

sum.wf.austria.app$debate_party_unordered=paste(sum.wf.austria.app$party_name,sum.wf.austria.app$oppo_party)

sum.wf.austria.app<-sum.wf.austria.app%>%
  mutate(
    debate_party=case_when(
      debate_party_unordered=="SPO OVP" ~ "OVP SPO",
      debate_party_unordered=="SPO GRUNE" ~ "GRUNE SPO",
      debate_party_unordered=="FPO GRUNE" ~ "GRUNE FPO",
      debate_party_unordered=="FPO NEOS" ~ "NEOS FPO",
      debate_party_unordered=="FPO OVP" ~ "OVP FPO",
      debate_party_unordered=="FPO SPO" ~ "SPO FPO",
      debate_party_unordered=="GRUNE NEOS" ~ "NEOS GRUNE",
      debate_party_unordered=="GRUNE OVP" ~ "OVP GRUNE",
      debate_party_unordered=="NEOS OVP" ~ "OVP NEOS",
      debate_party_unordered=="NEOS SPO" ~ "SPO NEOS",
      TRUE ~ debate_party_unordered
    )
  )

sum.wf.austria.app$debate_party_all<-sum.wf.austria.app$debate_party
sum.wf.austria.app$debate_party_all[grepl("ALL",sum.wf.austria.app$debate_party)]="ALL"

save(sum.wf.austria,sum.wf.austria.app,file="generated_data/Austria_wf_out")