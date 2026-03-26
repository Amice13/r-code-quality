#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script calculates the Word Error Rate for the SOTEU corpus and produces Figure 1 in the paper. 
# Also produces Figures A1, A2, A3 and A4 in the Appendix

##########################
# Intro
#devtools::install_github("jenswaeckerle/wersim")
library(extrafont)#This code was built with extrafont version 0.17
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(quanteda)#This code was built with quanteda version 1.3.13
library(wersim)#This code was built with wersim version 0.1.0

# Check if all packages are the ones we used
if(packageVersion("tidyverse")=="1.2.1"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("wersim")=="0.1.0"&
   packageVersion("extrafont")=="0.17"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

############################################
# Read in Data

# Read in all the Corpuus 
load("generated_data/Sotu_Out.RData")

############################################
# WER human Youtube
#####
# English
wer.youtube.en=wer(r=human.2011.en,h=youtube.2011.en)
(sum(wer.youtube.en$sub)+sum(wer.youtube.en$ins)+sum(wer.youtube.en$del))/sum(wer.youtube.en$words.ref)
#####
# German
wer.youtube.de=wer(r=human.2011.de,h=youtube.2011.de)
(sum(wer.youtube.de$sub)+sum(wer.youtube.de$ins)+sum(wer.youtube.de$del))/sum(wer.youtube.de$words.ref)
#####
# French
wer.youtube.fr=wer(r=human.2011.fr,h=youtube.2011.fr)
(sum(wer.youtube.fr$sub)+sum(wer.youtube.fr$ins)+sum(wer.youtube.fr$del))/sum(wer.youtube.fr$words.ref)

############################################
# WER human API

#####
# English
wer.api.en=wer(r=human.2011.en,h=api.2011.en)
(sum(wer.api.en$sub)+sum(wer.api.en$ins)+sum(wer.api.en$del))/sum(wer.api.en$words.ref)

#####
# German
wer.api.de=wer(r=human.2011.de,h=api.2011.de)
(sum(wer.api.de$sub)+sum(wer.api.de$ins)+sum(wer.api.de$del))/sum(wer.api.de$words.ref)

#####
# French
wer.api.fr=wer(r=human.2011.fr,h=api.2011.fr)
(sum(wer.api.fr$sub)+sum(wer.api.fr$ins)+sum(wer.api.fr$del))/sum(wer.api.fr$words.ref)

save(file="generated_data/worderrorrates_soteu.RData",
     wer.youtube.en,wer.youtube.de,wer.youtube.fr,
     wer.api.en,wer.api.de,wer.api.fr)

load("generated_data/worderrorrates_soteu.RData")

data.wer=data.frame(system_language=rep(NA,6),sub=NA,ins=NA,del=NA,stringsAsFactors = F)
data.wer[1,]=c("YouTube English",round(apply(wer.youtube.en[,2:4],2,sum)/sum(apply(wer.youtube.en[,2:4],2,sum))*100,2))
data.wer[2,]=c("YouTube German",round(apply(wer.youtube.de[,2:4],2,sum)/sum(apply(wer.youtube.de[,2:4],2,sum))*100,2))
data.wer[3,]=c("YouTube French",round(apply(wer.youtube.fr[,2:4],2,sum)/sum(apply(wer.youtube.fr[,2:4],2,sum))*100,2))
data.wer[4,]=c("API English",round(apply(wer.api.en[,2:4],2,sum)/sum(apply(wer.api.en[,2:4],2,sum))*100,2))
data.wer[5,]=c("API German",round(apply(wer.api.de[,2:4],2,sum)/sum(apply(wer.api.de[,2:4],2,sum))*100,2))
data.wer[6,]=c("API French",round(apply(wer.api.fr[,2:4],2,sum)/sum(apply(wer.api.fr[,2:4],2,sum))*100,2))

data.wer=data.frame(system_language=rep(NA,6),sub=NA,ins=NA,del=NA,stringsAsFactors = F)
data.wer[1,]=c("YouTube English",apply(wer.youtube.en[,2:4],2,sum))
data.wer[2,]=c("YouTube German",apply(wer.youtube.de[,2:4],2,sum))
data.wer[3,]=c("YouTube French",apply(wer.youtube.fr[,2:4],2,sum))
data.wer[4,]=c("API English",apply(wer.api.en[,2:4],2,sum))
data.wer[5,]=c("API German",apply(wer.api.de[,2:4],2,sum))
data.wer[6,]=c("API French",apply(wer.api.fr[,2:4],2,sum))

data.wer.long=gather(data.wer,key="Error",value="Share_of_Error",sub:del)
data.wer.long$Share_of_Error=as.numeric(data.wer.long$Share_of_Error)
data.wer.long$Error[data.wer.long$Error=="del"]="Deletions"
data.wer.long$Error[data.wer.long$Error=="ins"]="Insertions"
data.wer.long$Error[data.wer.long$Error=="sub"]="Substitutions"

##########
# This is Figure 1 in the paper.
p <-ggplot(data.wer.long, aes(system_language, Share_of_Error,fill=Error))+
  geom_bar(stat = "identity",position="dodge")+
  labs(x="",y="Number of Errors")+
  scale_fill_grey(start = 0, end = .75)+
  geom_vline(xintercept=3.5,lty=2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position=c(0.65, 0.75),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title=element_blank(),
        legend.text=element_text(family="Verdana"))
p
pdf("graphs_paper/Figure_1.pdf", height = 4) # Open a new pdf file
p
dev.off()

############################################
# Plots
############################################

maximum.value.wer<-as.numeric(max(wer.youtube.en$wer,
                                  wer.youtube.de$wer,
                                  wer.youtube.fr$wer,
                                  wer.api.en$wer,
                                  wer.api.de$wer,
                                  wer.api.fr$wer))+ 0.1
wer.youtube.en$Language="English"
wer.youtube.de$Language="German"
wer.youtube.fr$Language="French"
wer.api.en$Language="English"
wer.api.de$Language="German"
wer.api.fr$Language="French"

sotu_wer.youtube<-rbind(wer.youtube.en,wer.youtube.de,wer.youtube.fr)
# This is Figure A3 in the Appendix
wer.ori.you <-sotu_wer.youtube%>%
  ggplot(aes(y = wer,x=Language)) +
  geom_boxplot()+geom_point()+
  labs(x="",y="Word error rate")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")+ 
  scale_y_continuous(limits=c(0,maximum.value.wer))

sotu_wer.api<-rbind(wer.api.en,wer.api.de,wer.api.fr)

# This is Figure A4 in the Appendix
wer.ori.api <-sotu_wer.api%>%
  ggplot(aes(y = wer,x=Language)) +
  geom_boxplot()+geom_point()+
  labs(x="",y="Word error rate")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")+ 
  scale_y_continuous(limits=c(0,maximum.value.wer))

pdf("graphs_paper/Figure_A3.pdf", height = 4) # Open a new pdf file
wer.ori.you
dev.off()

pdf("graphs_paper/Figure_A4.pdf", height = 4) # Open a new pdf file
wer.ori.api
dev.off()

# This is the information on Word Error Rates at the beginning of chapter 3.1 
aggregate(sotu_wer.youtube$wer,by=list(sotu_wer.youtube$Language),mean)
aggregate(sotu_wer.api$wer,by=list(sotu_wer.api$Language),mean)


#################################################################
# Cosine Similarity
#################################################################
meta.simil=read.csv("metadata/metadata_similarity_2011.csv",sep=";",stringsAsFactors = F,encoding="UTF-8")
names(meta.simil)[1]="Name"

##################################################################################
# Human vs Youtube
##################################################################################
## Cosine Similarity human corpus with Youtube Transcripts English
# Create DFM with originals and youtube transcripts
DFM.human.youtube.en <- dfm(human.2011.en+youtube.2011.en,groups="Name_type",  
                               remove_punct = TRUE,remove_numbers=TRUE)
DFM.human.youtube.en=dfm_trim(DFM.human.youtube.en,min_termfreq  = 3)
# Calculate Similarity for all combinations of texts
sotu_simil=as.data.frame(as.matrix(textstat_simil(DFM.human.youtube.en,method="cosine",margin="documents",diag=T)))#Similarity matrix for all texts
# Only keep comparisons between humans and youtube
sotu_simil.2=sotu_simil[grepl("human",row.names(sotu_simil)),!grepl("human",row.names(sotu_simil))]
# Collapse to two col data frame
human.youtube.cosine.en=data.frame(matrix(nrow=nrow(sotu_simil.2),ncol=2))
names(human.youtube.cosine.en)=c("speech","cor")
for(i in 1:nrow(human.youtube.cosine.en)){
  human.youtube.cosine.en[i,1]=row.names(sotu_simil.2)[i]
  human.youtube.cosine.en[i,2]=sotu_simil.2[i,i]
}
# Extract info from textname and add as variables
info<-str_split(human.youtube.cosine.en$speech,"_") 
info <- do.call(rbind,info)
human.youtube.cosine.en$token=rowSums(DFM.human.youtube.en[grepl("human",row.names(DFM.human.youtube.en))])
human.youtube.cosine.en$Name=info[,1]
# Add in Metadata: Names, Group, Sex, Native Speaker
human.youtube.cosine.en=left_join(human.youtube.cosine.en,meta.simil)
human.youtube.cosine.en$Language="English"
human.youtube.cosine.en$Translated=ifelse(human.youtube.cosine.en$Speech_Language=="English","No","Yes")

#####################
## Cosine Similarity basic corpus with Youtube Transcripts French
# Create DFM with humans and youtube transcripts
DFM.human.youtube.fr <- dfm(human.2011.fr+youtube.2011.fr,groups="Name_type",  
                               remove_punct = TRUE,remove_numbers=TRUE)
DFM.human.youtube.fr=dfm_trim(DFM.human.youtube.fr,min_termfreq  = 3)
# Calculate Similarity for all combinations of texts
sotu_simil=as.data.frame(as.matrix(textstat_simil(DFM.human.youtube.fr,method="cosine",margin="documents",diag=T)))#Similarity matrix for all texts
# Only keep comparisons between humans and youtube
sotu_simil.2=sotu_simil[grepl("human",row.names(sotu_simil)),!grepl("human",row.names(sotu_simil))]
# Collapse to two col data frame
human.youtube.cosine.fr=data.frame(matrix(nrow=nrow(sotu_simil.2),ncol=2))
names(human.youtube.cosine.fr)=c("speech","cor")
for(i in 1:nrow(human.youtube.cosine.fr)){
  human.youtube.cosine.fr[i,1]=row.names(sotu_simil.2)[i]
  human.youtube.cosine.fr[i,2]=sotu_simil.2[i,i]
}
# Extract info from textname and add as variables
info<-str_split(human.youtube.cosine.fr$speech,"_") 
info <- do.call(rbind,info)
human.youtube.cosine.fr$token=rowSums(DFM.human.youtube.fr[grepl("human",row.names(DFM.human.youtube.fr))])
human.youtube.cosine.fr$Name=info[,1]
# Add in Metadata: Names, Group, Sex, Native Speaker
human.youtube.cosine.fr=left_join(human.youtube.cosine.fr,meta.simil)
human.youtube.cosine.fr$Language="French"
human.youtube.cosine.fr$Translated=ifelse(human.youtube.cosine.fr$Speech_Language=="French","No","Yes")

#####################
## Cosine Similarity basic corpus with Youtube Transcripts German
# Create DFM with humans and youtube transcripts
DFM.human.youtube.de <- dfm(human.2011.de+youtube.2011.de,groups="Name_type",  
                               remove_punct = TRUE,remove_numbers=TRUE)
DFM.human.youtube.de=dfm_trim(DFM.human.youtube.de,min_termfreq  = 3)
# Calculate Similarity for all combinations of texts
sotu_simil=as.data.frame(as.matrix(textstat_simil(DFM.human.youtube.de,method="cosine",margin="documents",diag=T)))#Similarity matrix for all texts
# Only keep comparisons between humans and youtube
sotu_simil.2=sotu_simil[grepl("human",row.names(sotu_simil)),!grepl("human",row.names(sotu_simil))]
# Collapse to two col data frame
human.youtube.cosine.de=data.frame(matrix(nrow=nrow(sotu_simil.2),ncol=2))
names(human.youtube.cosine.de)=c("speech","cor")
for(i in 1:nrow(human.youtube.cosine.de)){
  human.youtube.cosine.de[i,1]=row.names(sotu_simil.2)[i]
  human.youtube.cosine.de[i,2]=sotu_simil.2[i,i]
}
# Extract info from textname and add as variables
info<-str_split(human.youtube.cosine.de$speech,"_") 
info <- do.call(rbind,info)
human.youtube.cosine.de$token=rowSums(DFM.human.youtube.de[grepl("human",row.names(DFM.human.youtube.de))])
human.youtube.cosine.de$Name=info[,1]
# Add in Metadata: Names, Group, Sex, Native Speaker
human.youtube.cosine.de=left_join(human.youtube.cosine.de,meta.simil)
human.youtube.cosine.de$Language="German"
human.youtube.cosine.de$Translated=ifelse(human.youtube.cosine.de$Speech_Language=="German","No","Yes")

##################################################################################
# Human vs API
##################################################################################
## Cosine Similarity basic corpus with api Transcripts English
# Create DFM with humans and api transcripts
DFM.human.api.en <- dfm(human.2011.en+api.2011.en,groups="Name_type",  
                           remove_punct = TRUE,remove_numbers=TRUE)
DFM.human.api.en=dfm_trim(DFM.human.api.en,min_termfreq  = 3)
# Calculate Similarity for all combinations of texts
sotu_simil=as.data.frame(as.matrix(textstat_simil(DFM.human.api.en,method="cosine",margin="documents",diag=T)))#Similarity matrix for all texts
# Only keep comparisons between humans and api
sotu_simil.2=sotu_simil[grepl("human",row.names(sotu_simil)),!grepl("human",row.names(sotu_simil))]
# Collapse to two col data frame
human.api.cosine.en=data.frame(matrix(nrow=nrow(sotu_simil.2),ncol=2))
names(human.api.cosine.en)=c("speech","cor")
for(i in 1:nrow(human.api.cosine.en)){
  human.api.cosine.en[i,1]=row.names(sotu_simil.2)[i]
  human.api.cosine.en[i,2]=sotu_simil.2[i,i]
}
# Extract info from textname and add as variables
info<-str_split(human.api.cosine.en$speech,"_") 
info <- do.call(rbind,info)
human.api.cosine.en$token=rowSums(DFM.human.api.en[grepl("human",row.names(DFM.human.api.en))])
human.api.cosine.en$Name=info[,1]
# Add in Metadata: Names, Group, Sex, Native Speaker
human.api.cosine.en=left_join(human.api.cosine.en,meta.simil)
human.api.cosine.en$Language="English"
human.api.cosine.en$Translated=ifelse(human.api.cosine.en$Speech_Language=="English","No","Yes")

##############################
## Cosine Similarity basic corpus with api Transcripts French
# Create DFM with humans and api transcripts
DFM.human.api.fr <- dfm(human.2011.fr+api.2011.fr,groups="Name_type",  
                           remove_punct = TRUE,remove_numbers=TRUE)
DFM.human.api.fr=dfm_trim(DFM.human.api.fr,min_termfreq  = 3)
# Calculate Similarity for all combinations of texts
sotu_simil=as.data.frame(as.matrix(textstat_simil(DFM.human.api.fr,method="cosine",margin="documents",diag=T)))#Similarity matrix for all texts
# Only keep comparisons between humans and api
sotu_simil.2=sotu_simil[grepl("human",row.names(sotu_simil)),!grepl("human",row.names(sotu_simil))]
# Collapse to two col data frame
human.api.cosine.fr=data.frame(matrix(nrow=nrow(sotu_simil.2),ncol=2))
names(human.api.cosine.fr)=c("speech","cor")
for(i in 1:nrow(human.api.cosine.fr)){
  human.api.cosine.fr[i,1]=row.names(sotu_simil.2)[i]
  human.api.cosine.fr[i,2]=sotu_simil.2[i,i]
}
# Extract info from textname and add as variables
info<-str_split(human.api.cosine.fr$speech,"_") 
info <- do.call(rbind,info)
human.api.cosine.fr$token=rowSums(DFM.human.api.fr[grepl("human",row.names(DFM.human.api.fr))])
human.api.cosine.fr$Name=info[,1]
# Add in Metadata: Names, Group, Sex, Native Speaker
human.api.cosine.fr=left_join(human.api.cosine.fr,meta.simil)
human.api.cosine.fr$Language="French"
human.api.cosine.fr$Translated=ifelse(human.api.cosine.fr$Speech_Language=="French","No","Yes")

##############################
## Cosine Similarity basic corpus with api Transcripts German
# Create DFM with humans and api transcripts
DFM.human.api.de <- dfm(human.2011.de+api.2011.de,groups="Name_type",  
                           remove_punct = TRUE,remove_numbers=TRUE)
DFM.human.api.de=dfm_trim(DFM.human.api.de,min_termfreq  = 3)
# Calculate Similarity for all combinations of texts
sotu_simil=as.data.frame(as.matrix(textstat_simil(DFM.human.api.de,method="cosine",margin="documents",diag=T)))#Similarity matrix for all texts
# Only keep comparisons between humans and api
sotu_simil.2=sotu_simil[grepl("human",row.names(sotu_simil)),!grepl("human",row.names(sotu_simil))]
# Collapse to two col data frame
human.api.cosine.de=data.frame(matrix(nrow=nrow(sotu_simil.2),ncol=2))
names(human.api.cosine.de)=c("speech","cor")
for(i in 1:nrow(human.api.cosine.de)){
  human.api.cosine.de[i,1]=row.names(sotu_simil.2)[i]
  human.api.cosine.de[i,2]=sotu_simil.2[i,i]
}
# Extract info from textname and add as variables
info<-str_split(human.api.cosine.de$speech,"_") 
info <- do.call(rbind,info)
human.api.cosine.de$token=rowSums(DFM.human.api.de[grepl("human",row.names(DFM.human.api.de))])
human.api.cosine.de$Name=info[,1]
# Add in Metadata: Names, Group, Sex, Native Speaker
human.api.cosine.de=left_join(human.api.cosine.de,meta.simil)
human.api.cosine.de$Language="German"
human.api.cosine.de$Translated=ifelse(human.api.cosine.de$Speech_Language=="German","No","Yes")

############################################
# Plots
############################################

minimum.value<-as.numeric(min(human.api.cosine.de$cor,
                              human.api.cosine.en$cor,
                              human.api.cosine.fr$cor,
                              human.youtube.cosine.de$cor,
                              human.youtube.cosine.en$cor,
                              human.youtube.cosine.fr$cor))- 0.02


human.api=rbind(human.api.cosine.de,human.api.cosine.en,human.api.cosine.fr)
human.youtube=rbind(human.youtube.cosine.de,human.youtube.cosine.en,human.youtube.cosine.fr)

# This is Figure A1 in the Appendix
human.youtube$cor=round(human.youtube$cor,4)
plot.cosine.human.youtube=human.youtube%>%
  ggplot(aes(x = Language, y = cor)) +
  geom_boxplot()+geom_point()+
  labs(x="",y="Cosine similarity")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")+ 
  scale_y_continuous(limits=c(minimum.value,1))
# This is the information on Cosine Similarity at the beginning of chapter 3.1 
aggregate(human.youtube$cor,by=list(human.youtube$Language),mean)

# This is Figure A2 in the Appendix
human.api$cor=round(human.api$cor,4)
plot.cosine.human.api=human.api%>%
  ggplot(aes(x = Language, y = cor)) +
  geom_boxplot()+geom_point()+
  labs(x="",y="Cosine similarity")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position="none")+ 
  scale_y_continuous(limits=c(minimum.value,1))
# This is the information on Cosine Similarity at the beginning of chapter 3.1 
aggregate(human.api$cor,by=list(human.api$Language),mean)


pdf("graphs_paper/Figure_A1.pdf", height = 4) # Open a new pdf file
plot.cosine.human.youtube
dev.off()

pdf("graphs_paper/Figure_A2.pdf", height = 4) # Open a new pdf file
plot.cosine.human.api
dev.off()
