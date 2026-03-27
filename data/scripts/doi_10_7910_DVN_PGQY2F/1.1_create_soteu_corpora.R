#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script creates corpora from the raw State of the EU transcriptions

##########################
##########################
# Intro
library(quanteda)#This code was built with quanteda version 1.3.13
library(readtext)#This code was built with readtext version 0.71

# Check if all packages are the ones we used
if(packageVersion("readtext")=="0.71"&
     packageVersion("quanteda")=="1.3.13"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

meta.2011=read.csv("metadata/metadata_sotu_2011.csv",header=T,sep=";",stringsAsFactors = F,encoding="UTF-8")
names(meta.2011)[1]<-"Number"
meta.2011$Name_Protocol=trimws(meta.2011$Name_Protocol)
meta.2011.protocol=read.csv("metadata/metadata_sotu_2011_protocol.csv",header=T,sep=";",stringsAsFactors = F,encoding="UTF-8")
names(meta.2011.protocol)[1]<-"Number"

#########################################################################
#### 2011
#########################################################################

######################################
# Protocol 2011
protocol.2011.en.text <- readtext("data/2011_soteu_protocol_en.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  protocol.2011.en.text$text <- gsub(i,paste0("##",i),protocol.2011.en.text$text)
}
#Convert actor codes to tags (using whole words only)
protocol.2011.en <- corpus_segment(corpus(protocol.2011.en.text), "##*")
docvars(protocol.2011.en,"Name_Protocol")=gsub("##","",docvars(protocol.2011.en,"pattern"))
docvars(protocol.2011.en,"Name")=meta.2011$Name
docvars(protocol.2011.en,"Number")=meta.2011$Number
docvars(protocol.2011.en,"Group")=meta.2011$Group
docvars(protocol.2011.en,"Language")=meta.2011$Language
docvars(protocol.2011.en,"Name_type")=paste0(docvars(protocol.2011.en,"Name"),"_","protocol")
protocol.2011.en=corpus_subset(protocol.2011.en,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(protocol.2011.en)=paste0("Protocol_",docvars(protocol.2011.en,"Number"))
protocol.2011.en$documents$texts=gsub("\n"," ",protocol.2011.en$documents$texts)
protocol.2011.en$documents$texts=tolower(protocol.2011.en$documents$texts)#important for WER

#########################################################################
#### 2011 from Youtube
#########################################################################

###################
# French
youtube.2011.fr.text <- readtext("data/2011_soteu_youtube_fr.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  youtube.2011.fr.text$text <- gsub(i,paste0("##",i),youtube.2011.fr.text$text)
}
#Convert actor codes to tags (using whole words only)
youtube.2011.fr <- corpus_segment(corpus(youtube.2011.fr.text), "##*")
docvars(youtube.2011.fr,"Name_Protocol")=gsub("##","",docvars(youtube.2011.fr,"pattern"))
docvars(youtube.2011.fr,"Name")=meta.2011$Name
docvars(youtube.2011.fr,"Number")=meta.2011$Number
docvars(youtube.2011.fr,"Group")=meta.2011$Group
docvars(youtube.2011.fr,"Language")=meta.2011$Language
docvars(youtube.2011.fr,"Name_type")=paste0(docvars(youtube.2011.fr,"Name"),"_","youtube")
youtube.2011.fr=corpus_subset(youtube.2011.fr,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(youtube.2011.fr)=paste0("Youtube_",docvars(youtube.2011.fr,"Number"))
youtube.2011.fr$documents$texts=gsub("\n"," ",youtube.2011.fr$documents$texts)
youtube.2011.fr$documents$texts=tolower(youtube.2011.fr$documents$texts)

###################
# German
youtube.2011.de.text <- readtext("data/2011_soteu_youtube_de.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  youtube.2011.de.text$text <- gsub(i,paste0("##",i),youtube.2011.de.text$text)
}
#Convert actor codes to tags (using whole words only)
youtube.2011.de <- corpus_segment(corpus(youtube.2011.de.text), "##*")
docvars(youtube.2011.de,"Name_Protocol")=gsub("##","",docvars(youtube.2011.de,"pattern"))
docvars(youtube.2011.de,"Name")=meta.2011$Name
docvars(youtube.2011.de,"Number")=meta.2011$Number
docvars(youtube.2011.de,"Group")=meta.2011$Group
docvars(youtube.2011.de,"Language")=meta.2011$Language
docvars(youtube.2011.de,"Name_type")=paste0(docvars(youtube.2011.de,"Name"),"_","youtube")
youtube.2011.de=corpus_subset(youtube.2011.de,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(youtube.2011.de)=paste0("Youtube_",docvars(youtube.2011.de,"Number"))
youtube.2011.de$documents$texts=gsub("\n"," ",youtube.2011.de$documents$texts)
youtube.2011.de$documents$texts=tolower(youtube.2011.de$documents$texts)

###################
# English
youtube.2011.en.text <- readtext("data/2011_soteu_youtube_en.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  youtube.2011.en.text$text <- gsub(i,paste0("##",i),youtube.2011.en.text$text)
}
#Convert actor codes to tags (using whole words only)
youtube.2011.en <- corpus_segment(corpus(youtube.2011.en.text), "##*")
docvars(youtube.2011.en,"Name_Protocol")=gsub("##","",docvars(youtube.2011.en,"pattern"))
docvars(youtube.2011.en,"Name")=meta.2011$Name
docvars(youtube.2011.en,"Number")=meta.2011$Number
docvars(youtube.2011.en,"Group")=meta.2011$Group
docvars(youtube.2011.en,"Language")=meta.2011$Language
docvars(youtube.2011.en,"Name_type")=paste0(docvars(youtube.2011.en,"Name"),"_","youtube")
youtube.2011.en=corpus_subset(youtube.2011.en,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(youtube.2011.en)=paste0("Youtube_",docvars(youtube.2011.en,"Number"))
youtube.2011.en$documents$texts=gsub("\n"," ",youtube.2011.en$documents$texts)
youtube.2011.en$documents$texts=tolower(youtube.2011.en$documents$texts)

#########################################################################
#### 2011 from Human transcriptions
#########################################################################

###################
# French
human.2011.fr.text <- readtext("data/2011_soteu_human_fr.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  human.2011.fr.text$text <- gsub(i,paste0("##",i),human.2011.fr.text$text)
}
#Convert actor codes to tags (using whole words only)
human.2011.fr <- corpus_segment(corpus(human.2011.fr.text), "##*")
docvars(human.2011.fr,"Name_Protocol")=gsub("##","",docvars(human.2011.fr,"pattern"))
docvars(human.2011.fr,"Name")=meta.2011$Name
docvars(human.2011.fr,"Number")=meta.2011$Number
docvars(human.2011.fr,"Group")=meta.2011$Group
docvars(human.2011.fr,"Language")=meta.2011$Language
docvars(human.2011.fr,"Name_type")=paste0(docvars(human.2011.fr,"Name"),"_","human")
human.2011.fr=corpus_subset(human.2011.fr,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(human.2011.fr)=paste0("human_",docvars(human.2011.fr,"Number"))
human.2011.fr$documents$texts=gsub("\n"," ",human.2011.fr$documents$texts)
human.2011.fr$documents$texts=tolower(human.2011.fr$documents$texts)

###################
# German
human.2011.de.text <- readtext("data/2011_soteu_human_de.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  human.2011.de.text$text <- gsub(i,paste0("##",i),human.2011.de.text$text)
}
#Convert actor codes to tags (using whole words only)
human.2011.de <- corpus_segment(corpus(human.2011.de.text), "##*")
docvars(human.2011.de,"Name_Protocol")=gsub("##","",docvars(human.2011.de,"pattern"))
docvars(human.2011.de,"Name")=meta.2011$Name
docvars(human.2011.de,"Number")=meta.2011$Number
docvars(human.2011.de,"Group")=meta.2011$Group
docvars(human.2011.de,"Language")=meta.2011$Language
docvars(human.2011.de,"Name_type")=paste0(docvars(human.2011.de,"Name"),"_","human")
human.2011.de=corpus_subset(human.2011.de,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(human.2011.de)=paste0("human_",docvars(human.2011.de,"Number"))
human.2011.de$documents$texts=gsub("\n"," ",human.2011.de$documents$texts)
human.2011.de$documents$texts=tolower(human.2011.de$documents$texts)

###################
# English
human.2011.en.text <- readtext("data/2011_soteu_human_en.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  human.2011.en.text$text <- gsub(i,paste0("##",i),human.2011.en.text$text)
}
#Convert actor codes to tags (using whole words only)
human.2011.en <- corpus_segment(corpus(human.2011.en.text), "##*")
docvars(human.2011.en,"Name_Protocol")=gsub("##","",docvars(human.2011.en,"pattern"))
docvars(human.2011.en,"Name")=meta.2011$Name
docvars(human.2011.en,"Number")=meta.2011$Number
docvars(human.2011.en,"Group")=meta.2011$Group
docvars(human.2011.en,"Language")=meta.2011$Language
docvars(human.2011.en,"Name_type")=paste0(docvars(human.2011.en,"Name"),"_","human")
human.2011.en=corpus_subset(human.2011.en,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(human.2011.en)=paste0("human_",docvars(human.2011.en,"Number"))
human.2011.en$documents$texts=gsub("\n"," ",human.2011.en$documents$texts)
human.2011.en$documents$texts=tolower(human.2011.en$documents$texts)

#########################################################################
#### 2011 from API
#########################################################################

###################
# English
api.2011.en.text <- readtext("data/2011_soteu_api_en.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  api.2011.en.text$text <- gsub(i,paste0("##",i),api.2011.en.text$text)
}
#Convert actor codes to tags (using whole words only)
api.2011.en <- corpus_segment(corpus(api.2011.en.text), "##*")
docvars(api.2011.en,"Name_Protocol")=gsub("##","",docvars(api.2011.en,"pattern"))
docvars(api.2011.en,"Name")=meta.2011$Name
docvars(api.2011.en,"Number")=meta.2011$Number
docvars(api.2011.en,"Group")=meta.2011$Group
docvars(api.2011.en,"Language")=meta.2011$Language
docvars(api.2011.en,"Name_type")=paste0(docvars(api.2011.en,"Name"),"_","api")
api.2011.en=corpus_subset(api.2011.en,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(api.2011.en)=paste0("API_",docvars(api.2011.en,"Number"))
api.2011.en$documents$texts=gsub("\n"," ",api.2011.en$documents$texts)
api.2011.en$documents$texts=tolower(api.2011.en$documents$texts)

###################
# German
api.2011.de.text <- readtext("data/2011_soteu_api_de.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  api.2011.de.text$text <- gsub(i,paste0("##",i),api.2011.de.text$text)
}
#Convert actor codes to tags (using whole words only)
api.2011.de <- corpus_segment(corpus(api.2011.de.text), "##*")
docvars(api.2011.de,"Name_Protocol")=gsub("##","",docvars(api.2011.de,"pattern"))
docvars(api.2011.de,"Name")=meta.2011$Name
docvars(api.2011.de,"Number")=meta.2011$Number
docvars(api.2011.de,"Group")=meta.2011$Group
docvars(api.2011.de,"Language")=meta.2011$Language
docvars(api.2011.de,"Name_type")=paste0(docvars(api.2011.de,"Name"),"_","api")
api.2011.de=corpus_subset(api.2011.de,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(api.2011.de)=paste0("API_",docvars(api.2011.de,"Number"))
api.2011.de$documents$texts=gsub("\n"," ",api.2011.de$documents$texts)
api.2011.de$documents$texts=tolower(api.2011.de$documents$texts)

###################
# French
api.2011.fr.text <- readtext("data/2011_soteu_api_fr.txt",encoding="UTF-8")
for(i in unique(meta.2011$Name_Protocol)){
  api.2011.fr.text$text <- gsub(i,paste0("##",i),api.2011.fr.text$text)
}
#Convert actor codes to tags (using whole words only)
api.2011.fr <- corpus_segment(corpus(api.2011.fr.text), "##*")
docvars(api.2011.fr,"Name_Protocol")=gsub("##","",docvars(api.2011.fr,"pattern"))
docvars(api.2011.fr,"Name")=meta.2011$Name
docvars(api.2011.fr,"Number")=meta.2011$Number
docvars(api.2011.fr,"Group")=meta.2011$Group
docvars(api.2011.fr,"Language")=meta.2011$Language
docvars(api.2011.fr,"Name_type")=paste0(docvars(api.2011.fr,"Name"),"_","api")
api.2011.fr=corpus_subset(api.2011.fr,Name_Protocol!="PRESIDENT"&Name_Protocol!="CROSS")
docnames(api.2011.fr)=paste0("API_",docvars(api.2011.fr,"Number"))
api.2011.fr$documents$texts=gsub("\n"," ",api.2011.fr$documents$texts)
api.2011.fr$documents$texts=tolower(api.2011.fr$documents$texts)

# There are some texts that are empty because there are 
# only words in one of the languages (because the interpreters 
# did not translate a short statement). In order to deal with this,
# a text that is empty in one of the languages either in the Youtube
# transcriptm the API or the original is excluded in all corpora.
# This only affets very short "speeches" with generally less than five words.

emptytexts=unique(c(which(ntoken(human.2011.en)==0),which(ntoken(human.2011.de)==0),which(ntoken(human.2011.fr)==0),
                    which(ntoken(youtube.2011.en)==0),which(ntoken(youtube.2011.de)==0),which(ntoken(youtube.2011.fr)==0),
                    which(ntoken(api.2011.en)==0),which(ntoken(api.2011.de)==0),which(ntoken(api.2011.fr)==0)))
emptytexts.number=docvars(api.2011.de,"Number")[emptytexts]
api.2011.de=corpus_subset(api.2011.de,!Number%in%emptytexts.number)
api.2011.en=corpus_subset(api.2011.en,!Number%in%emptytexts.number)
api.2011.fr=corpus_subset(api.2011.fr,!Number%in%emptytexts.number)
human.2011.de=corpus_subset(human.2011.de,!Number%in%emptytexts.number)
human.2011.en=corpus_subset(human.2011.en,!Number%in%emptytexts.number)
human.2011.fr=corpus_subset(human.2011.fr,!Number%in%emptytexts.number)
youtube.2011.de=corpus_subset(youtube.2011.de,!Number%in%emptytexts.number)
youtube.2011.en=corpus_subset(youtube.2011.en,!Number%in%emptytexts.number)
youtube.2011.fr=corpus_subset(youtube.2011.fr,!Number%in%emptytexts.number)
protocol.2011.en=corpus_subset(protocol.2011.en,!Number%in%emptytexts.number)

save(youtube.2011.fr,human.2011.fr,
     youtube.2011.de,human.2011.de,
     youtube.2011.en,human.2011.en,
     api.2011.en,api.2011.de,api.2011.fr,protocol.2011.en,
     file="generated_data/Sotu_Out.RData")
