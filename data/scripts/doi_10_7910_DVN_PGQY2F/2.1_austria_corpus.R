#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script combines the raw transcripts from the Austria debates into two datasets: 
# the main one called "textcorpus.austria" and one including minor speakers for the appendix
# called "textcorpus.austria.appendix"
# It also calculates the Word Error Rate for a part of the corpus.

##########################
# Intro

library(readtext)#This code was built with readtext version 0.71
library(quanteda)#This code was built with quanteda version 1.3.13
library(wersim)#This code was built with wersim version 0.1.0
library(stringr)#This code was built with stringr version 1.3.1

# Check if all packages are the ones we used
if(packageVersion("readtext")=="0.71"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("wersim")=="0.1.0"&
   packageVersion("stringr")=="1.3.1"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

##########################
# Read in Texts
names.texts<-dir("data/austria_transcripts_speakers/")

place.df<-data.frame(matrix(NA,length(names.texts),2))

for(i in 1:length(names.texts)){
  text1<-readtext(paste0("data/austria_transcripts_speakers/",names.texts[i]),encoding = "UTF-8")
  text1$text=gsub("\n"," ",as.character(text1$text))
  place.df[i,]<-text1
  print(names.texts[i])
}

names(place.df)<-c("name","text")
textcorpus.austria<-corpus(place.df,docid_field = "name", text_field = "text")
sum.tc<-summary(textcorpus.austria)
info<-str_split(sum.tc$Text,"_")
info <- do.call(rbind,info)
info[,3]<-gsub(".txt","",info[,3])
docvars(textcorpus.austria,"Names")<-info[,3]
docvars(textcorpus.austria,"date")=info[,2]
docvars(textcorpus.austria,"type")=info[,1]

textcorpus.austria=corpus_subset(textcorpus.austria,!grepl(x=docvars(textcorpus.austria,"type"),"sommer"))
textcorpus.austria=corpus_subset(textcorpus.austria,Names!=c("pilz"))
textcorpus.austria.appendix=textcorpus.austria
# Eliminate speeches by minor candidates and interviews
textcorpus.austria=corpus_subset(textcorpus.austria,!Names%in%c("griss","hofer","pilz","felipe","moser"))

sum.tc=summary(textcorpus.austria)
summary(sum.tc$Tokens)
aggregate(sum.tc$Tokens,by=list(sum.tc$Names),mean)
aggregate(sum.tc$Tokens,by=list(sum.tc$Names),sum)
table(sum.tc$Names)

# Save Corpus
save(textcorpus.austria,textcorpus.austria.appendix,file="generated_data/Austria_Out.RData")

###########################################################
####
#Austria TV Debates Word Error Rate
####
###########################################################

load("generated_data/Austria_Out.RData")
# Verbatim Transcripts

austria.text.verbatim <- readtext("data/elefantenrunde_24092017_verbatim.txt")
austria.text.verbatim$text=gsub("\n"," ",austria.text.verbatim$text)
austria.text.verbatim$text=gsub("\t"," ",austria.text.verbatim$text)
austria.text.verbatim$text=gsub(" *\\[.*?\\] *"," ",austria.text.verbatim$text)
austria.text.verbatim$text=tolower(austria.text.verbatim$text)

names_austria=c("kurz:","strache:","kern:","lunacek:","strolz:","moderator:","pilz:","video:")
for(i in names_austria){
  austria.text.verbatim$text <- gsub(i,paste0("##",i),austria.text.verbatim$text)
}
austria.text.verbatim.corpus <- corpus_segment(corpus(austria.text.verbatim), "##*")
summary(austria.text.verbatim.corpus)

# Youtube Transcripts

austria.text.youtube <- readtext("data/elefantenrunde_24092017.txt")
austria.text.youtube$text=gsub("\n"," ",austria.text.youtube$text)
austria.text.youtube$text=gsub("\t"," ",austria.text.youtube$text)
austria.text.youtube$text=gsub(" *\\[.*?\\] *"," ",austria.text.youtube$text)
austria.text.youtube$text=tolower(austria.text.youtube$text)

austria.text.youtube.corpus <- corpus_segment(corpus(austria.text.youtube), "##*")

dat=data.frame(a=docvars(austria.text.verbatim.corpus,"pattern"),b=docvars(austria.text.youtube.corpus,"pattern"))

wer.austria=wer(r=austria.text.verbatim.corpus,h=austria.text.youtube.corpus)
(sum(wer.austria$sub)+sum(wer.austria$ins)+sum(wer.austria$del))/sum(wer.austria$words.ref)
round(apply(wer.austria[,2:4],2,sum)/sum(apply(wer.austria[,2:4],2,sum))*100,2)

save(file="generated_data/worderrorrates_austria.RData",
     wer.austria)
