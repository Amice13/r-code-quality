####TITLE: Fossilization potential of marine assemblages and environments
####AUTHOR: Jack O. Shaw
####PERMANENT EMAIL: jackolivershaw@gmail.com


############INSTRUCTIONS

#Run this script via SupDataDR4C.R
#To conduct taxon- or geography-specific analysis change filtering processes in code section "TAXON- AND GEOGRAPHY-SPECIFIC FOSSILIZATION POTENTIAL"

############INSTRUCTIONS




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


####TAXON- AND GEOGRAPHY-SPECIFIC FOSSILIZATION POTENTIAL----
##Use these lines to conduct europe/invert specific analyses

#Cull for europe
#Moddata_joined2<-Moddata_joined2 %>% mutate_at(c("LatBin","LongBin"),as.character) %>% mutate_at(c("LatBin","LongBin"),as.numeric) %>% filter(LatBin<=58 & LatBin>=35 & LongBin<=(22) & LongBin>=(-15))
#pb2<-pb2 %>% mutate_at(c("lat","lng"),as.character) %>% mutate_at(c("lat","lng"),as.numeric)%>% filter(lat<=58 & lat>=35 & lng<=(22) & lng>=(-15))

# #Cull for inverts
# Moddata_joined2<-Moddata_joined2 %>%  filter(phylum!="Chordata")
# pb2<-pb2  %>%  filter(phylum!="Chordata")

####RUN ANALYSIS----

joined_col5<-c()
joined_col7<-c()
for(tl in c("order","family","genus")) {
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
  
  #Match list
  joindat$presenceTAX<-joindat$taxlev %in% pb_shelly$taxlev
  #joindat$presenceLAG<-joindat$taxlev %in% pb_lag$taxlev
  joindat$presenceENV_matching<-joindat$taxlevenv %in% pb_env$taxlevenv
  joindat$presenceENV_unavailable<-!joindat$taxlev %in% pb_env$taxlev & joindat$taxlev %in% pb_shelly$taxlev
  joindat$presenceENV_available<-joindat$taxlev %in% pb_env$taxlev
  joindat$presenceENV_different<- joindat$presenceENV_available != joindat$presenceENV_matching
  joindat$presenceENV_available<-NULL
  
  joindat2<-joindat
  #joindat2[,c("class","order","family")]<-NULL
  joindat2<-unique(joindat2[,])
  joindat2[,c("presenceTAX","presenceENV_matching","presenceENV_unavailable","presenceENV_different")] <- sapply(joindat2[,c("presenceTAX","presenceENV_matching","presenceENV_unavailable","presenceENV_different")],as.logical)
  
  joindat3<-joindat2 %>%
    group_by(dataset.par,dataset_id,depth.slice,LongBin,LatBin,id_habitat,id_scope,id_plankton,id_sampling,
             id_fishery, environment) %>%
    summarise(NumTotal=n(),
              NumTAX=sum(presenceTAX,na.rm=T),
              NumENV_match=sum(presenceENV_matching,na.rm=T),
              NumENV_unavail=sum(presenceENV_unavailable,na.rm=T),
              NumENV_dif=sum(presenceENV_different,na.rm=T),
              NumSelTax=n_distinct(taxlev))
  
  joindat3<-as.data.frame(joindat3)
  joindat3$NumENV_any<-joindat3$NumENV_match+joindat3$NumENV_dif
  #joindat3$NumFossil<-joindat3$NumShelly+joindat3$NumLag
  #joindat3$Remaining_Fossil<-joindat3$NumFossil/joindat3$NumTotal
  joindat3$Remaining_Tax<-joindat3$NumTAX/joindat3$NumTotal #hist(joindat3$Remaining_Shelly)
  joindat3$Remaining_Env_match<-joindat3$NumENV_match/joindat3$NumTotal
  joindat3$Remaining_Env_unavail<-joindat3$NumENV_unavail/joindat3$NumTotal
  joindat3$Remaining_Env_dif<-joindat3$NumENV_dif/joindat3$NumTotal
  joindat3$Remaining_Env_any<-joindat3$NumENV_any/joindat3$NumTotal
  
  cols.num<-c("depth.slice","LongBin","LatBin",
              "Remaining_Tax","Remaining_Env_match","Remaining_Env_unavail","Remaining_Env_dif","Remaining_Env_any",
              "NumTotal","NumTAX","NumENV_match","NumENV_unavail","NumENV_dif","NumENV_any","NumSelTax")
  joindat3[cols.num] <- sapply(joindat3[cols.num],as.character)
  joindat3[cols.num] <- sapply(joindat3[cols.num],as.numeric)
  cols.num2<-c("id_habitat","environment","id_scope","id_plankton"#,"Vert"
  )
  joindat3[cols.num2] <- sapply(joindat3[cols.num2],as.factor)
  
  
  ##Rename variables
  joindat3$id_scope<-stringr::str_to_title(joindat3$id_scope)
  joindat3$id_habitat<-stringr::str_to_title(joindat3$id_habitat)
  joindat3$environment<-stringr::str_to_sentence(joindat3$environment)
  
  
  ####COMPARE ANIMALIA AND PLANKTON----
  
  #Put NAs as animalia
  joindat3$id_scope[is.na(joindat3$id_scope)]<-"Animalia"
  
  #Only retain animals
  joindat4<-subset(joindat3,id_scope=="Animalia")
  #Partition benthic v pelagic data
  joindat4$id_habitat[is.na(joindat4$id_habitat)] <- "Benthic"
  joindat4$id_habitat<-gsub("Benthicandpelagic", "Benthic", joindat4$id_habitat)
  joindat4<-subset(joindat4,id_fishery==FALSE)
  joindat4$id_scope[is.na(joindat4$id_scope)]<-"Animalia"
  
  #Only retain animals
  joindat5<-subset(joindat4,id_scope=="Animalia")
  
  #Partition benthic v pelagic data
  joindat5$id_habitat[is.na(joindat5$id_habitat)] <- "Benthic"
  joindat5$id_habitat<-gsub("Benthicandpelagic", "Benthic", joindat5$id_habitat)
  joindat5<-as.data.frame(subset(joindat5,id_fishery==FALSE))
  
  joined_col5<-rbind(joined_col5,cbind(joindat5,taxlev=tl))
  
  joindat6 <- joindat5[!is.na(joindat5$environment),]
  joindat6<-subset(joindat6,environment!="")
  joindat6<-unique(joindat6[,])
  
  joindat6$environment<- factor(joindat6$environment, levels=c('Pelagic','Shallow','Coral reef','Seamount','Deep'))
  
  joined_col7<-rbind(joined_col7,cbind(joindat6,taxlev=tl))
  
  #bracket for looping data
  print(tl)
}



####COMBINE WITH SUBSTRATE DATA----
library(kknn)
train_dialects_kknn<-readRDS("SupDataDR5C.RDS")


#Combine with OBIS, only benthic data
dia_obis_OG<-subset(joined_col7, environment!="Pelagic" & environment!="Coral reef" & environment!="Seamount")
dia_obis_OG<-dia_obis_OG[,c("dataset.par","taxlev","LatBin","LongBin","Remaining_Tax")]
dia_obis<-dia_obis_OG[,c("LatBin","LongBin")]
colnames(dia_obis)<-c("lat","lon")

pred<-predict(train_dialects_kknn, dia_obis,type="prob")
pred2<-as.data.frame(pred)
pred2$V1<-NULL
pred2$pred_substrate <- colnames(pred2)[max.col(pred2,ties.method="first")]
pred2$prob_select<-apply(pred2[,c("rck","gvl","snd","mud")], 1, FUN=max,na.rm=TRUE)
pred3<-cbind(pred2,dia_obis_OG)

t<-pred3

#Change factor names
t$substrate = factor(t$pred_substrate, levels=c("mud","snd","gvl","rck"))

t$substrate<-gsub("mud","Mud",t$substrate)
t$substrate<-gsub("snd","Sand",t$substrate)
t$substrate<-gsub("gvl","Gravel",t$substrate)
t$substrate<-gsub("rck","Rock",t$substrate)

t_final<-t
t_final$substrate = factor(t_final$substrate, levels=c("Mud","Sand","Gravel","Rock"))

