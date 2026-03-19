#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script uses the WERSIM procedure on the SOTEU corpus
# It also produces results for Table 1 in the paper. 
# It also produces Figure A7 in the Appendix
# Running this script takes a long time. 
# You can skip the simulations themselves and load the data directly below.

##########################
# Intro
#devtools::install_github("jenswaeckerle/wersim")
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(austin)#This code was built with austin version 0.3.0
library(quanteda)#This code was built with quanteda version 1.3.13
library(RecordLinkage)#This code was built with RecordLinkage version 0.4-10
library(stargazer)#This code was built with stargazer version 5.2.2
library(wersim)#This code was built with wersim version 0.1.0
library(extrafont)#This code was built with extrafont version 0.17

# Check if all packages are the ones we used
if(packageVersion("tidyverse")=="1.2.1"&
   packageVersion("austin")=="0.3.0"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("RecordLinkage")=="0.4-10"&
   packageVersion("stargazer")=="5.2.2"&
   packageVersion("wersim")=="0.1.0"&
   packageVersion("extrafont")=="0.17"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

load("generated_data/Sotu_Out.RData")
load("generated_data/worderrorrates_soteu.RData")
wer_soteu=(sum(wer.youtube.en$sub)+sum(wer.youtube.en$ins)+sum(wer.youtube.en$del))/sum(wer.youtube.en$words.ref)
round(apply(wer.youtube.en[,2:4],2,sum)/sum(apply(wer.youtube.en[,2:4],2,sum))*100,2)

##################
#Run WERSIM on the youtube corpus.

docvars(youtube.2011.en,"Text")=docvars(youtube.2011.en,"Name")
set.seed(1711)

werse_est_20_wf=wersimtext(youtube.2011.en,measured_wer = 0.0258,new_wer=(wer_soteu+0.2),direction =c(22,37),
                           deletions_sim=0.13,insertions_sim=0.22,substitutions_sim = 0.65,num_sims = 200,preprocessing = c("punctuation", "numbers","min_term"),
                       groupingvar_sim=docvars(youtube.2011.en,"Text"),method="wordfish",mincount_wersim=3)
save(file="generated_data/soteusimulations_twenty_wf.RData",
     werse_est_20_wf)

werse_est_20_senti=wersimtext(youtube.2011.en,measured_wer = 0.0258,new_wer=(wer_soteu+0.2),direction =c(22,37),
                              deletions_sim=0.13,insertions_sim=0.22,substitutions_sim = 0.65,num_sims = 200,
                          groupingvar_sim=docvars(youtube.2011.en,"Text"),method="sentiment")
save(file="generated_data/soteusimulations_twenty_senti.RData",
     werse_est_20_senti)

####################################################
# Sentiment
####################################################
#load("generated_data/soteusimulations_twenty_wf.RData")
#load("generated_data/soteusimulations_twenty_senti.RData")

#Load Data from Youtube Transcriptions
senti_youtube=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary=data_dictionary_LSD2015)%>%
  convert(to="data.frame")
#Calculate Sentiment
senti_youtube$Sentiment=log((senti_youtube$positive+0.5)/(senti_youtube$negative+0.5))

data.plot=data.frame(
  rbind(
    data.frame(Name=c("José Manuel Barroso","Sophie Auconie","Nigel Farage"),
               Sentiment=c(senti_youtube$Sentiment[grepl("Barroso",senti_youtube$document)],
                           senti_youtube$Sentiment[grepl("Auconie",senti_youtube$document)],
                           senti_youtube$Sentiment[grepl("Farage",senti_youtube$document)]),
               WER=0.03),
    data.frame(Name="José Manuel Barroso",
               Sentiment=as.numeric(unlist(werse_est_20_senti[grepl("Barroso",werse_est_20_senti$index),2:ncol(werse_est_20_senti)])),
               WER=0.23),
    data.frame(Name="Sophie Auconie",
               Sentiment=as.numeric(unlist(werse_est_20_senti[grepl("Auconie",werse_est_20_senti$index),2:ncol(werse_est_20_senti)])),
               WER=0.23),
    data.frame(Name="Nigel Farage",
               Sentiment=as.numeric(unlist(werse_est_20_senti[grepl("Farage",werse_est_20_senti$index),2:ncol(werse_est_20_senti)])),
               WER=0.23)))
###############
# This produces Figure A7 in Appendix 4
set.seed(1711)
wersim_senti=ggplot(data.plot,aes(x=WER,y=Sentiment,col=Name))+
  geom_jitter(width=0.01,height = 0,size=3,alpha=0.6)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.title=element_blank(),
        legend.text=element_text(family="Verdana"))
wersim_senti

pdf("graphs_paper/Figure_A7.pdf", height = 4) # Open a new pdf file
wersim_senti
dev.off()

####################################################
# Sentiment Model
####################################################

data_store_quantilechanges_nopre=data.frame(document=werse_est_20_senti[,1],stringsAsFactors = F)
data_store_quantilechanges_nopre$document=as.character(data_store_quantilechanges_nopre$document)
data_store_quantilechanges_nopre=left_join(data_store_quantilechanges_nopre,senti_youtube)
data_store_quantilechanges_nopre$abs_senti=abs(data_store_quantilechanges_nopre$Sentiment-mean(data_store_quantilechanges_nopre$Sentiment))
int_senti_twenty=apply(werse_est_20_senti[,2:ncol(werse_est_20_senti)],1,IQR)

data_store_quantilechanges_nopre$interval20=int_senti_twenty

#calculate text length
rowsums.store=data.frame(textlength=as.numeric(rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))),
                         document=names(rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))),stringsAsFactors = F)
data_store_quantilechanges_nopre=left_join(data_store_quantilechanges_nopre,rowsums.store)

senti_human=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary=data_dictionary_LSD2015)%>%
  convert(to="data.frame")

#Calculate Sentiment
senti_human$Sentiment=log((senti_human$positive+0.5)/(senti_human$negative+0.5))

cors=data.frame(num=1:200,cor=NA)
for(i in 1:nrow(cors)){
  mean_senti_twenty=werse_est_20_senti[,i+1]
  cors$cor[i]=cor(senti_human$Sentiment,mean_senti_twenty)
}
mean(cors$cor)
quantile(cors$cor,c(0.025,0.975))

no_pre_senti=data_store_quantilechanges_nopre
no_pre_senti$textlength=no_pre_senti$textlength/1000

mod_senti=lm(interval20~abs_senti+textlength,data=no_pre_senti)
summary(mod_senti)

####################################################
# Wordfish model
####################################################

#load("generated_data/sotusimulations_twenty_wf.RData")
data_store_quantilechanges_nopre=data.frame(document=werse_est_20_wf[,1],stringsAsFactors = F)
dfm_youtube=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
dfm_youtube=dfm_trim(dfm_youtube,min_termfreq  = 3)
wf_youtube=austin::wordfish(quanteda::as.wfm(dfm_youtube),verbose=TRUE)
wf_youtube.sum=data.frame("theta"=wf_youtube$theta,"document"=wf_youtube$docs,stringsAsFactors = F)

#and join
data_store_quantilechanges_nopre=left_join(data_store_quantilechanges_nopre,wf_youtube.sum)
data_store_quantilechanges_nopre$abs_wf=abs(data_store_quantilechanges_nopre$theta)

#calculate text length
rowsums.store=data.frame(textlength=as.numeric(rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))),
                         document=names(rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))))
data_store_quantilechanges_nopre=left_join(data_store_quantilechanges_nopre,rowsums.store)
int_wf_twenty=apply(werse_est_20_wf[,2:ncol(werse_est_20_senti)],1,IQR)
data_store_quantilechanges_nopre$interval20=int_wf_twenty

#Calculate Wordfish
dfm_human=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
dfm_human=dfm_trim(dfm_human,min_termfreq  = 3)
wf_human=austin::wordfish(quanteda::as.wfm(dfm_human),verbose=TRUE)
wf_human.sum=data.frame("theta"=wf_human$theta,"document"=wf_human$docs,stringsAsFactors = F)

cor(wf_youtube.sum$theta,wf_human.sum$theta)
cors=data.frame(num=1:200,cor=NA)
for(i in 1:nrow(cors)){
  mean_wf_twenty=werse_est_20_wf[,i+1]
  cors$cor[i]=cor(wf_human.sum$theta,mean_wf_twenty)
}
mean(cors$cor)
quantile(cors$cor,c(0.025,0.975))

no_pre_wf=data_store_quantilechanges_nopre
no_pre_wf$textlength=no_pre_wf$textlength/1000

mod_wf=lm(interval20~abs_wf+textlength,data=no_pre_wf)
summary(mod_wf)

########################
# This produces Table 1
stargazer(mod_wf,mod_senti,
          title            = "Table 1: Examining Word Error Rate Simulations for the SOTEU Debate",
          dep.var.labels=c("IQR for additional 0.2 WER"),
          covariate.labels=c("Wordfish absolute deviation from mean",
                             "Sentiment absolute deviation from mean",
                             "Text length by 1000 words"),
          column.labels = c("Wordfish model","Sentiment model"),
          ci = T,
          omit.stat = c("f" , "ser"),
          notes = "",
          notes.append=F,star.cutoffs = c(0.00000000001),
          omit.table.layout = "n",
          out="tables/Table1.tex")
