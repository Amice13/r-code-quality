####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com

############INSTRUCTIONS

#Use this script to run Sup Data DR4A
#This script subsamples OBIS assemblages by PBDB sample size (influencing within-environment fossilization potential) and by number of OBIS assemblages
#This script should be used in isolation as it interacts with data terms used in SupData4B-4F

############INSTRUCTIONS

library(data.table)
library(dplyr)

source("SupDataDR4A.R")
sitedat_joined<-read.csv("SupDataDR2B.csv")

####READ IN PBDB----
pb_mstr<-fread("SupDataDR3A.csv", header = T, sep = ',',select=c("phylum","class","order","family","genus","formation","environment","max_ma","min_ma","geology_comments")) 

#Identify any seamounts
library(stringr)
pb_mstr$environment<-ifelse(pb_mstr$geology_comments=="Seamount","Seamount",pb_mstr$environment)
pb_mstr$environment<-ifelse(pb_mstr$geology_comments=="Pelagic","Pelagic",pb_mstr$environment)

pb_mstr <- pb_mstr %>% mutate(environment = ifelse(stringr::str_detect(pb_mstr$geology_comments, fixed("seamount")), "Seamount", pb_mstr$environment))
pb_mstr <- pb_mstr %>% mutate(environment = ifelse(stringr::str_detect(pb_mstr$geology_comments, fixed("Seamount")), "Seamount", pb_mstr$environment))

pb_mstr <- pb_mstr %>% mutate(environment = ifelse(stringr::str_detect(pb_mstr$geology_comments, fixed("pelagic")), "Pelagic", pb_mstr$environment))
pb_mstr <- pb_mstr %>% mutate(environment = ifelse(stringr::str_detect(pb_mstr$geology_comments, fixed("Pelagic")), "Pelagic", pb_mstr$environment))


laglist<-read.csv("SupDataDR3B.csv")[,1]

pb_baz<-data.frame(
  stringsAsFactors = FALSE,
  environment = c("intrashelf/intraplatform reef","perireef or subreef",
                  "platform/shelf-margin reef","reef, buildup or bioherm",
                  "basin reef","offshore ramp","slope",
                  "basinal (carbonate)","basinal (siliceous)",
                  "basinal (siliciclastic)","deep-water indet.","Pelagic",
                  "pelagic","Seamount","seamount",
                  "coastal indet.","open shallow subtidal",
                  "shallow subtidal indet.","shoreface",
                  "transition zone/lower shoreface","deep subtidal indet.","deep subtidal ramp",
                  "deep subtidal shelf","offshore",
                  "offshore indet.","offshore shelf","fluvial indet.",
                  "fluvial-deltaic indet.","fluvial-lacustrine indet.",
                  "lacustrine - large","lacustrine - small",
                  "lacustrine delta front","lacustrine delta plain",
                  "lacustrine deltaic indet.","lacustrine indet.",
                  "delta front","prodelta","delta plain",
                  "deltaic indet.","estuary/bay","foreshore",
                  "lagoonal/restricted shallow subtidal",
                  "marginal marine indet.","paralic indet.","peritidal",
                  "interdistributary bay","lagoonal"),
  OBISenvs = c("Coral reef",
               "Coral reef","Coral reef","Coral reef",
               "Coral reef","Deep","Deep","Deep","Deep","Deep",
               "Deep","Pelagic","Pelagic","Seamount","Seamount",
               "Shallow","Shallow","Shallow","Shallow",
               "Shallow","Shallow","Shallow","Shallow",
               "Shallow","Shallow","Shallow","terrestrial",
               "terrestrial","terrestrial","terrestrial","terrestrial",
               "terrestrial","terrestrial","terrestrial",
               "terrestrial","terrestrial","terrestrial",
               "terrestrial","terrestrial","terrestrial",
               "terrestrial","terrestrial","terrestrial","terrestrial",
               "terrestrial","terrestrial","terrestrial")
)


pb<-pb_mstr

#pb2<-as.data.frame(pb[!pb$formation %in% laglist,,drop=FALSE])
pb2<-as.data.frame(pb) #Includes lagerstatten taxa
pb2_lag<-as.data.frame(pb[pb$formation %in% laglist,,drop=FALSE])


joined_col5<-c()
joined_col7<-c()


ss_breaks<-c(100,500,1000,2500,5000,7500,10000,20000)
ss_names<-c("coral reef","shallow","deep")

subbing_all<-c()
runs<-1000

tl<-"genus"
  taxonlevel<-tl
  taxnam<-paste("NO_",toupper(taxonlevel),"_SPECIFIED",sep="")
  
  pb<-pb_mstr 
  laglist2<-laglist
  
  pb2<-as.data.frame(pb) #Includes lagerstatten taxa
  pb2_lag<-as.data.frame(pb[pb$formation %in% laglist2,,drop=FALSE])
  
  #Make shelly list
  pb_shelly<-as.data.frame(pb2[,c("phylum",taxonlevel)])
  pb_shelly<-unique(pb_shelly[,])
  pb_shelly$taxlev<-pb_shelly[,taxonlevel]
  pb_shelly$taxlev<-stringr::word(pb_shelly$taxlev,1)
  pb_shelly$taxlev<-tolower(pb_shelly$taxlev)
  pb_shelly<-subset(pb_shelly,taxlev!="" & phylum!="" & phylum!="NO_PHYLUM_SPECIFIED" & taxlev!=taxnam)
  
  #Identify matching environments
  env1<-pb2[,c("phylum",taxonlevel,"environment")]
  env1<-unique(env1[,])
  env1$taxlev<-env1[,taxonlevel]
  env1[,taxonlevel]<-NULL
  env1<-env1[,c("phylum","taxlev","environment")]
  env1$taxlev<-stringr::word(env1$taxlev,1)
  env1$taxlev<-tolower(env1$taxlev)
  env1<-subset(env1,taxlev!="" & phylum!="" & phylum!="NO_PHYLUM_SPECIFIED" & taxlev!=taxnam)
  envtaxpres<-paste(taxonlevel,"EnvPresent",sep="")
  env1$PresenceENV<-envtaxpres
  #Convert PBDB env types to OBIS env types
  env2<-merge(env1,pb_baz,by="environment",all.x=TRUE)
  pb_env<-as.data.frame(env2[!is.na(env2$environment),])
  pb_env<-subset(pb_env,environment!="")
  pb_env<-pb_env[!is.na(pb_env$OBISenvs),]
  #Make OBIS envs the operational type
  pb_env$environment<-NULL
  colnames(pb_env)[which(names(pb_env) == "OBISenvs")] <- "PBDBenvironment"
  pb_env<-subset(pb_env,PBDBenvironment!="terrestrial")
  pb_env<-unique(pb_env[,])
  pb_env$taxlevenv<-paste(tolower(pb_env$taxlev),tolower(pb_env$PBDBenvironment),sep="")
  pb_env$PBDBenvironment<-tolower(pb_env$PBDBenvironment)
  
  
  #Combine Moddata with sitedata
  Moddata_joined3<-Moddata_joined2[,c("phylum","class","order","family","genus",
                                      "taxon","dataset.par")]
  Moddata_joined3$taxlev<-Moddata_joined3[,c(taxonlevel)]
  Moddata_joined3<-Moddata_joined3[,c("phylum","taxlev","dataset.par")]
  joindat<-merge(Moddata_joined3,sitedat_joined,by=c("dataset.par"),all.x=TRUE)
  
  
  #Drop data without taxonomic info
  joindat<-joindat[!is.na(joindat$taxlev),]
  colnames(joindat)[which(names(joindat) == "id_environment")] <- "environment"
  joindat$taxlevenv<-paste(tolower(joindat$taxlev),tolower(joindat$environment),sep="")
  joindat$taxlev<-stringr::word(joindat$taxlev,1)
  joindat$taxlev<-tolower(joindat$taxlev)
  
  for(v in ss_names){
  
  #Drop seamount and pelagic
  pb_env_pre<-pb_env %>% filter(PBDBenvironment==v)
  joindat_pre<-joindat %>% filter(environment==v)
  
  
  n_ass<-length(unique(pb_env_pre$taxlev))
  
  ss_breaks2<-ss_breaks[ss_breaks<n_ass]
  ss_breaks2<-c(ss_breaks2,n_ass)
  
  for(b in ss_breaks2) {
    
  min_pbdb_num<-as.numeric(as.character(b))
  
  subbing_mini<-c()
  
  for(j in 1:runs){
    
    joindat2<-joindat_pre
    pb_env2<-pb_env_pre %>% group_by(PBDBenvironment) %>% sample_n(min_pbdb_num)

    joindat2$presenceTAX<-joindat2$taxlev %in% pb_shelly$taxlev
    joindat2$presenceENV_matching<-joindat2$taxlevenv %in% pb_env2$taxlevenv
    
    joindat2<-unique(joindat2[,])
    joindat2[,c("presenceTAX","presenceENV_matching")] <- sapply(joindat2[,c("presenceTAX","presenceENV_matching")],as.logical)
    
    joindat3<-joindat2 %>%
      group_by(dataset.par,dataset_id,depth.slice,LongBin,LatBin,id_habitat,id_scope,id_plankton,id_sampling,
               id_fishery, environment) %>%
      summarise(NumTotal=n(),
                NumTAX=sum(presenceTAX,na.rm=T),
                NumENV_match=sum(presenceENV_matching,na.rm=T),
                NumSelTax=n_distinct(taxlev))
    
    joindat3<-as.data.frame(joindat3)
    joindat3$Remaining_Tax<-as.numeric(as.character(joindat3$NumTAX/joindat3$NumTotal))
    joindat3$Remaining_Env_match<-as.numeric(as.character(joindat3$NumENV_match/joindat3$NumTotal))
    
    
    ####Rename variables----
    joindat3$id_scope<-stringr::str_to_title(joindat3$id_scope)
    joindat3$id_habitat<-stringr::str_to_title(joindat3$id_habitat)
    joindat3$environment<-stringr::str_to_sentence(joindat3$environment)
    
    #Only retain animals
    joindat4<-subset(joindat3,id_scope=="Animalia")
    joindat4$id_scope[is.na(joindat4$id_scope)]<-"Animalia"
    
    #Only retain animals
    joindat5<-subset(joindat4,id_scope=="Animalia")
    
    #Partition benthic v pelagic data
    joindat5<-as.data.frame(subset(joindat5,id_fishery==FALSE))
    
    joindat6 <- joindat5[!is.na(joindat5$environment),]
    joindat6<-subset(joindat6,environment!="")
    joindat6<-unique(joindat6[,])
    
    joindat6$environment<- factor(joindat6$environment, levels=c('Shallow','Coral reef','Deep'))
    
    row<-cbind(joindat6,run=j,taxlev=tl,sample_size=b,env=v)
  
    
    subbing_mini<-rbind(subbing_mini,row)
    
    print(paste(v,b,j))
  }
  
  
  write.csv(subbing_mini,paste(v,b,".csv",sep=""))
  
  subbing_all<-rbind(subbing_all,subbing_mini)
  
  
  }
  
  print(v)
  
  
  }
  
  
  
  
  write.csv(subbing_all,"subbing_all_july30.csv")


sts<-subbing_all[,c("dataset.par","environment")] %>% distinct()
min_obis<-table(sts$environment)
min_obis<-min_obis[which.min(min_obis)] 
min_obis<-as.numeric(as.character(min_obis))


rard<-c()
runs2<-1000
for(i in 1:runs2){  
  subbing_all2 <-subbing_all %>% group_by(taxlev,env,sample_size,run) %>% sample_n(min_obis) %>% 
    summarise(mean_TFP=mean(Remaining_Tax,na.rm=T),mean_AFP=mean(Remaining_Env_match,na.rm=T))
  rard<-rbind(rard,cbind(as.data.frame(subbing_all2),run=i))
  print(i)
}


subbing_all3<-subbing_all2 %>% pivot_longer(cols=c("mean_TFP","mean_AFP"),names_to="val_type",values_to="values")

ggplot(subbing_all3,aes(x=sample_size,y=values,color=env))+
  geom_point()+
  facet_grid(val_type~.,scales="free")


subbing_all4<-subbing_all3 %>%
  group_by(sample_size,env,val_type) %>%
  summarise(
    avg = mean(values, na.rm = TRUE),
    min = quantile(values, probs = 0.025, na.rm = TRUE), 
    max = quantile(values, probs = 0.975, na.rm = TRUE)
  ) %>%
  filter(!is.na(val_type))

ggplot(subbing_all4,aes(x=sample_size,y=avg,color=env,fill=env))+
  geom_ribbon(aes(ymin=min,ymax=max),alpha=0.5)+
  geom_point(alpha=0.5)+
  facet_grid(val_type~.,scales="free")



