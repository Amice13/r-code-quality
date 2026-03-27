#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script runs the WERSIM procedure for the Austria analysis.
# It produces Figure 4 in the paper
# Running this script takes a long time. 
# You can skip the simulations themselves and load the data directly below.

##########################
# Intro
#devtools::install_github("jenswaeckerle/wersim")
library(RecordLinkage)#This code was built with RecordLinkage version 0.4-10
library(quanteda)#This code was built with quanteda version 1.3.13
library(stringr)#This code was built with stringr version 1.3.1
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(plm)#This code was built with plm version 1.6-6
library(wersim)#This code was built with wersim version 0.1.0
library(extrafont)#This code was built with extrafont version 0.17

# Check if all packages are the ones we used
if(packageVersion("RecordLinkage")=="0.4-10"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("stringr")=="1.3.1"&
   packageVersion("tidyverse")=="1.2.1"&
   packageVersion("plm")=="1.6-6"&
   packageVersion("wersim")=="0.1.0"&
   packageVersion("extrafont")=="0.17"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

load("generated_data/Austria_Out.RData")
#Load Wordfish Estimates from youtube corpus with independent variables
load("generated_data/Austria_wf_out")

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

load("generated_data/worderrorrates_austria.RData")
(sum(wer.austria$sub)+sum(wer.austria$ins)+sum(wer.austria$del))/sum(wer.austria$words.ref)
round(apply(wer.austria[,2:4],2,sum)/sum(apply(wer.austria[,2:4],2,sum))*100,2)

docvars(textcorpus.austria,"Text")=row.names(docvars(textcorpus.austria))

new_wer_to_est=0.2958+c(0.05,0.1,0.15,0.2)
set.seed(1711)
werse_est_5=wersimtext(textcorpus.austria,measured_wer = 0.2958,new_wer=new_wer_to_est[1],
                   deletions_sim = 0.58,insertions_sim = 0.05,substitutions_sim = 0.37,num_sims = 50,preprocessing = c("stopwords_de","min_term"),
                   groupingvar_sim=docvars(textcorpus.austria,"Text"),method="wordfish",mincount_wersim=3,direction=c(3,4))

werse_est_10=wersimtext(textcorpus.austria,measured_wer = 0.2958,new_wer=new_wer_to_est[2],
                    deletions_sim = 0.58,insertions_sim = 0.05,substitutions_sim = 0.37,num_sims = 50,preprocessing = c("stopwords_de","min_term"),
                    groupingvar_sim=docvars(textcorpus.austria,"Text"),method="wordfish",mincount_wersim=3,direction=c(3,4))

werse_est_15=wersimtext(textcorpus.austria,measured_wer = 0.2958,new_wer=new_wer_to_est[3],
                    deletions_sim = 0.58,insertions_sim = 0.05,substitutions_sim = 0.37,num_sims = 50,preprocessing = c("stopwords_de","min_term"),
                    groupingvar_sim=docvars(textcorpus.austria,"Text"),method="wordfish",mincount_wersim=3,direction=c(3,4))

werse_est_20=wersimtext(textcorpus.austria,measured_wer = 0.2958,new_wer=new_wer_to_est[4],
                    deletions_sim = 0.58,insertions_sim = 0.05,substitutions_sim = 0.37,num_sims = 50,preprocessing = c("stopwords_de","min_term"),
                    groupingvar_sim=docvars(textcorpus.austria,"Text"),method="wordfish",mincount_wersim=3,direction=c(3,4))

save(file="generated_data/austriasimulations.RData",
     werse_est_5,werse_est_10,werse_est_15,werse_est_20)

####
# Read in simulated wordfish positions

#load("generated_data/austriasimulations.RData")

num_to_sim=50
new_wer_to_est=0.2958+c(0.05,0.1,0.15,0.2)

#Rename simulations
names(werse_est_5)[2:(num_to_sim+1)]=paste0("5_errors_simulation",1:num_to_sim)
names(werse_est_10)[2:(num_to_sim+1)]=paste0("10_errors_simulation",1:num_to_sim)
names(werse_est_15)[2:(num_to_sim+1)]=paste0("15_errors_simulation",1:num_to_sim)
names(werse_est_20)[2:(num_to_sim+1)]=paste0("20_errors_simulation",1:num_to_sim)

#Combine all in one data frame
werse_est=cbind(werse_est_5,werse_est_10[,2:(num_to_sim+1)],werse_est_15[,2:(num_to_sim+1)],werse_est_20[,2:(num_to_sim+1)])

new_werse=data.frame(est=as.numeric(werse_est[3,2:201]),wer=c(rep(new_wer_to_est[1],num_to_sim),rep(new_wer_to_est[2],num_to_sim),
                                                              rep(new_wer_to_est[3],num_to_sim),rep(new_wer_to_est[4],num_to_sim)),stringsAsFactors = F)


#Add in independent variables
info<-str_split(werse_est$index,"_")
info <- do.call(rbind,info)
info[,3]<-gsub(".txt","",info[,3])

werse_est$Name<-info[,3]
werse_est$date<-as.Date(info[,2],"%d%m%Y")
werse_est$debate<-info[,1]

werse_est<-werse_est%>%
  mutate(
    mode=case_when(
      grepl("elefanten",werse_est$debate) ~ "full",
      grepl("sommer",werse_est$debate) ~ "single",
      TRUE ~ "duel"
    )
  )

werse_est<-werse_est%>%
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

werse_est<-werse_est%>%filter(Name!="pilz")

werse_est<-werse_est%>%
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

werse_est=left_join(x=werse_est,y=AustriaLR.to.join)

lr=data.frame(aggregate(werse_est$lrgen,by=list(werse_est$party_name),mean))
lr[6,]=c("ALL_NO_FPO",mean(as.numeric(lr[c(2,3,4,5),2]),na.rm=T))
lr[7,]=c("ALL_NO_GRUNE",mean(as.numeric(lr[c(1,3,4,5),2]),na.rm=T))
lr[8,]=c("ALL_NO_NEOS",mean(as.numeric(lr[c(1,2,4,5),2]),na.rm=T))
lr[9,]=c("ALL_NO_OVP",mean(as.numeric(lr[c(1,2,3,5),2]),na.rm=T))
lr[10,]=c("ALL_NO_SPO",mean(as.numeric(lr[c(1,2,3,4),2]),na.rm=T))

migr=data.frame(aggregate(werse_est$immigrate_policy,by=list(werse_est$party_name),mean))
migr[6,]=c("ALL_NO_FPO",mean(as.numeric(migr[c(2,3,4,5),2]),na.rm=T))
migr[7,]=c("ALL_NO_GRUNE",mean(as.numeric(migr[c(1,3,4,5),2]),na.rm=T))
migr[8,]=c("ALL_NO_NEOS",mean(as.numeric(migr[c(1,2,4,5),2]),na.rm=T))
migr[9,]=c("ALL_NO_OVP",mean(as.numeric(migr[c(1,2,3,5),2]),na.rm=T))
migr[10,]=c("ALL_NO_SPO",mean(as.numeric(migr[c(1,2,3,4),2]),na.rm=T))

galtan=data.frame(aggregate(werse_est$galtan,by=list(werse_est$party_name),mean))
galtan[6,]=c("ALL_NO_FPO",mean(as.numeric(galtan[c(2,3,4,5),2]),na.rm=T))
galtan[7,]=c("ALL_NO_GRUNE",mean(as.numeric(galtan[c(1,3,4,5),2]),na.rm=T))
galtan[8,]=c("ALL_NO_NEOS",mean(as.numeric(galtan[c(1,2,4,5),2]),na.rm=T))
galtan[9,]=c("ALL_NO_OVP",mean(as.numeric(galtan[c(1,2,3,5),2]),na.rm=T))
galtan[10,]=c("ALL_NO_SPO",mean(as.numeric(galtan[c(1,2,3,4),2]),na.rm=T))

werse_est<-werse_est%>%
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
werse_est<-werse_est%>%
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

werse_est$oppo_galtan=NA
werse_est$oppo_galtan[werse_est$oppo_party=="SPO"]=galtan$x[galtan$Group.1=="SPO"]
werse_est$oppo_galtan[werse_est$oppo_party=="FPO"]=galtan$x[galtan$Group.1=="FPO"]
werse_est$oppo_galtan[werse_est$oppo_party=="OVP"]=galtan$x[galtan$Group.1=="OVP"]
werse_est$oppo_galtan[werse_est$oppo_party=="GRUNE"]=galtan$x[galtan$Group.1=="GRUNE"]
werse_est$oppo_galtan[werse_est$oppo_party=="NEOS"]=galtan$x[galtan$Group.1=="NEOS"]

werse_est$oppo_galtan[werse_est$oppo_party=="ALL"&werse_est$party_name=="SPO"]=galtan$x[galtan$Group.1=="ALL_NO_SPO"]
werse_est$oppo_galtan[werse_est$oppo_party=="ALL"&werse_est$party_name=="FPO"]=galtan$x[galtan$Group.1=="ALL_NO_FPO"]
werse_est$oppo_galtan[werse_est$oppo_party=="ALL"&werse_est$party_name=="OVP"]=galtan$x[galtan$Group.1=="ALL_NO_OVP"]
werse_est$oppo_galtan[werse_est$oppo_party=="ALL"&werse_est$party_name=="GRUNE"]=galtan$x[galtan$Group.1=="ALL_NO_GRUNE"]
werse_est$oppo_galtan[werse_est$oppo_party=="ALL"&werse_est$party_name=="NEOS"]=galtan$x[galtan$Group.1=="ALL_NO_NEOS"]

werse_est$oppo_lr=as.numeric(werse_est$oppo_lr)
werse_est$oppo_migr=as.numeric(werse_est$oppo_migr)
werse_est$oppo_galtan=as.numeric(werse_est$oppo_galtan)

#########
# Create data frame to store results in 
data_store_austria_reg=data.frame(wer=c(rep(new_wer_to_est[1],num_to_sim),rep(new_wer_to_est[2],num_to_sim),
                                        rep(new_wer_to_est[3],num_to_sim),rep(new_wer_to_est[4],num_to_sim)),
                                  est=NA,stringsAsFactors = F)

######
# Run main model
for(j in 1:nrow(data_store_austria_reg)){
  #Create data frame for one simulation
  werse_est.data <- data.frame(werse_est[,c(ncol(werse_est)-2,ncol(werse_est)-7,j+1)])
  #Rename
  names(werse_est.data)[grepl("simulation",names(werse_est.data))]="theta"
  #Run model as in paper
  boot.res=data.frame(estimate.lr=rep(NA,2000))
  
  for(i in 1:nrow(boot.res)){
    #build clustered data frame
    parties=as.character(sample(unique(werse_est.data$party_name),size = 5,replace = T))
    boot.dat=rbind(werse_est.data[werse_est.data$party_name==parties[1],],
                   werse_est.data[werse_est.data$party_name==parties[2],],
                   werse_est.data[werse_est.data$party_name==parties[3],],
                   werse_est.data[werse_est.data$party_name==parties[4],],
                   werse_est.data[werse_est.data$party_name==parties[5],])
    #estimate model
    boot.dat.pdata <- pdata.frame(boot.dat, index = "party_name")
    if(length(unique(parties))>1){
      mod.lr.boot <- plm(theta~oppo_lr, data = boot.dat.pdata, model = "within", effect="individual")
      # save estimates
      boot.res[i,"estimate.lr"]=coef(mod.lr.boot)
    }
  }
  #Store estimates
  data_store_austria_reg$est[j]=mean(boot.res$estimate.lr,na.rm = T)
  print(paste("Simulation",j,"of",nrow(data_store_austria_reg)))
}

save(file="generated_data/austriasimulations_models.RData",data_store_austria_reg)
#load("generated_data/austriasimulations_models.RData")
aggregate(est~wer,data_store_austria_reg,mean)

#Run model from paper
sum.wf.austria.pdata <- pdata.frame(sum.wf.austria, index = "party_name")
mod.lr <- plm(theta~oppo_lr, data = sum.wf.austria.pdata, model = "within", effect="individual")

graph.austria.sim<-data_store_austria_reg%>%
  ggplot(mapping=aes(y=est, x=wer))+
  geom_boxplot(aes(group = wer))+
  geom_point(aes(y=coef(mod.lr)[1],x=0.2958), color="red")+ 
  annotate("text", label = "Estimate from \nYouTube corpus", x = 0.2958, y = coef(mod.lr)[1]+0.015, size = 5, colour = "red",family="Verdana")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=15,colour="black",family="Verdana"),
        axis.title=element_text(size=15,family="Verdana"),
        legend.position = c(0.25, 0.95),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"), 
        legend.direction = "horizontal",
        legend.text=element_text(size=14),
        legend.title = element_blank())+
  labs(y = 'Simulated estimates for \ncoefficient in model 1', x = 'Word error rate')+
  scale_y_continuous(limits = c(-0.03,0.07))+
  scale_x_continuous(limits = c(0.25,0.53))
graph.austria.sim

pdf("graphs_paper/Figure_4.pdf", width = 10, height = 5) # Open a new pdf file
graph.austria.sim
dev.off()