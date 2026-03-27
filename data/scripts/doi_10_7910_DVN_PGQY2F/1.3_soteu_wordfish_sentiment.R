#############################################################################################
# Replication Data for: Proksch, Wratil, Wäckerle. (2018). Testing the Validity of Automatic
# Speech Recognition for Political Text Analysis. Political Analysis, forthcoming.
#############################################################################################

#########################
# This script calculates Wordfish and Sentiment estimates for the SOTEU corpus.
# It also produces Figure 2 in the paper.
# It also produces Figures A5 and A6, as well as A8 to A11 in the Appendix.
# It also produces data for Table A1 in the Appendix.
# If you want to skip calculations, you can go right to the graphs and load the data.

##########################
# Intro
#devtools::install_github("conjugateprior/austin")
library(extrafont)#This code was built with extrafont version 0.17
library(tidyverse)#This code was built with tidyverse version 1.2.1
library(austin)#This code was built with austin version 0.3.0
library(quanteda)#This code was built with quanteda version 1.3.13
library(stm)#This code was built with stm version 1.3.3. You also have to have the tm package installed but don't need to load it
library(xtable)
# Check if all packages are the ones we used
if(packageVersion("tidyverse")=="1.2.1"&
   packageVersion("extrafont")=="0.17"&
   packageVersion("austin")=="0.3.0"&
   packageVersion("quanteda")=="1.3.13"&
   packageVersion("stm")=="1.3.3"){
  paste("Your packages seem good to go")
}else{
  paste("Warning: Some or all of the package versions you are using are not exactly the ones we used for this paper. If the code doesn't replicate, this might be the reason.")
}

#######################################################################
# Read in data
#######################################################################

meta.2011=read.csv("metadata/metadata_sotu_2011.csv",header=T,sep=";",stringsAsFactors = F,encoding="UTF-8")
meta.2011$Name_Protocol=trimws(meta.2011$Name_Protocol)
names(meta.2011)[1]<-"Number"
meta.2011.no.duplicates=meta.2011[!duplicated(meta.2011$Name),]
# Read in all the Corpuus 
load("generated_data/Sotu_Out.RData")

load("data/lsde_frenche_germane.RData")

#######################################################################
# wordfish by Speaker - BASIC
#######################################################################

# English human - BASIC
DFM.human.2011.en=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.en=dfm_trim(DFM.human.2011.en,min_termfreq  = 3)
left.fix=which(row.names(DFM.human.2011.en)=="José Manuel Barroso")
right.fix=which(row.names(DFM.human.2011.en)=="Jan Zahradil")
wf.human.2011.en=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en),dir=c(left.fix,right.fix),verbose=T)
sum.wf.human.2011.en=data.frame(est.human.en=wf.human.2011.en$theta,
                                   lwr.human.en=wf.human.2011.en$theta-1.96*wf.human.2011.en$se.theta,
                                   upr.human.en=wf.human.2011.en$theta+1.96*wf.human.2011.en$se.theta,
                                   Name=wf.human.2011.en$docs,stringsAsFactors = F)
sum.wf.human.2011.en=left_join(x=sum.wf.human.2011.en,y=meta.2011.no.duplicates)

# French human - BASIC
DFM.human.2011.fr=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.fr=dfm_trim(DFM.human.2011.fr,min_termfreq = 3)
wf.human.2011.fr=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr),dir=c(left.fix,right.fix),verbose=T)
sum.wf.human.2011.fr=data.frame(est.human.fr=wf.human.2011.fr$theta,
                                   lwr.human.fr=wf.human.2011.fr$theta-1.96*wf.human.2011.fr$se.theta,
                                   upr.human.fr=wf.human.2011.fr$theta+1.96*wf.human.2011.fr$se.theta,
                                   Name=wf.human.2011.fr$docs,stringsAsFactors = F)
sum.wf.human.2011.fr=left_join(x=sum.wf.human.2011.fr,y=meta.2011.no.duplicates)

# German human - BASIC
DFM.human.2011.de=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.de=dfm_trim(DFM.human.2011.de,min_termfreq = 3)
wf.human.2011.de=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de),dir=c(left.fix,right.fix),verbose=T)
sum.wf.human.2011.de=data.frame(est.human.de=wf.human.2011.de$theta,
                                   lwr.human.de=wf.human.2011.de$theta-1.96*wf.human.2011.de$se.theta,
                                   upr.human.de=wf.human.2011.de$theta+1.96*wf.human.2011.de$se.theta,
                                   Name=wf.human.2011.de$docs,stringsAsFactors = F)
sum.wf.human.2011.de=left_join(x=sum.wf.human.2011.de,y=meta.2011.no.duplicates)

# English Youtube - BASIC
DFM.youtube.2011.en=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.en=dfm_trim(DFM.youtube.2011.en,min_termfreq = 3)
wf.youtube.2011.en=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en),dir=c(left.fix,right.fix),verbose=T)
sum.wf.youtube.2011.en=data.frame(est.youtube.en=wf.youtube.2011.en$theta,
                                  lwr.youtube.en=wf.youtube.2011.en$theta-1.96*wf.youtube.2011.en$se.theta,
                                  upr.youtube.en=wf.youtube.2011.en$theta+1.96*wf.youtube.2011.en$se.theta,
                                  Name=wf.youtube.2011.en$docs,stringsAsFactors = F)
sum.wf.youtube.2011.en=left_join(x=sum.wf.youtube.2011.en,y=meta.2011.no.duplicates)

# French Youtube - BASIC
DFM.youtube.2011.fr=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.fr=dfm_trim(DFM.youtube.2011.fr,min_termfreq = 3)
wf.youtube.2011.fr=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr),dir=c(left.fix,right.fix),verbose=T)
sum.wf.youtube.2011.fr=data.frame(est.youtube.fr=wf.youtube.2011.fr$theta,
                                  lwr.youtube.fr=wf.youtube.2011.fr$theta-1.96*wf.youtube.2011.fr$se.theta,
                                  upr.youtube.fr=wf.youtube.2011.fr$theta+1.96*wf.youtube.2011.fr$se.theta,
                                  Name=wf.youtube.2011.fr$docs,stringsAsFactors = F)
sum.wf.youtube.2011.fr=left_join(x=sum.wf.youtube.2011.fr,y=meta.2011.no.duplicates)

# German Youtube - BASIC
DFM.youtube.2011.de=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.de=dfm_trim(DFM.youtube.2011.de,min_termfreq = 3)
wf.youtube.2011.de=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de),dir=c(left.fix,right.fix),verbose=T)
sum.wf.youtube.2011.de=data.frame(est.youtube.de=wf.youtube.2011.de$theta,
                                  lwr.youtube.de=wf.youtube.2011.de$theta-1.96*wf.youtube.2011.de$se.theta,
                                  upr.youtube.de=wf.youtube.2011.de$theta+1.96*wf.youtube.2011.de$se.theta,
                                  Name=wf.youtube.2011.de$docs,stringsAsFactors = F)
sum.wf.youtube.2011.de=left_join(x=sum.wf.youtube.2011.de,y=meta.2011.no.duplicates)


# English API - BASIC
DFM.api.2011.en=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.en=dfm_trim(DFM.api.2011.en,min_termfreq = 3)
wf.api.2011.en=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en),dir=c(left.fix,right.fix),verbose=T)
sum.wf.api.2011.en=data.frame(est.api.en=wf.api.2011.en$theta,
                              lwr.api.en=wf.api.2011.en$theta-1.96*wf.api.2011.en$se.theta,
                              upr.api.en=wf.api.2011.en$theta+1.96*wf.api.2011.en$se.theta,
                              Name=wf.api.2011.en$docs,stringsAsFactors = F)
sum.wf.api.2011.en=left_join(x=sum.wf.api.2011.en,y=meta.2011.no.duplicates)

# French API - BASIC
DFM.api.2011.fr=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.fr=dfm_trim(DFM.api.2011.fr,min_termfreq = 3)
wf.api.2011.fr=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr),dir=c(left.fix,right.fix),verbose=T)
sum.wf.api.2011.fr=data.frame(est.api.fr=wf.api.2011.fr$theta,
                              lwr.api.fr=wf.api.2011.fr$theta-1.96*wf.api.2011.fr$se.theta,
                              upr.api.fr=wf.api.2011.fr$theta+1.96*wf.api.2011.fr$se.theta,
                              Name=wf.api.2011.fr$docs,stringsAsFactors = F)
sum.wf.api.2011.fr=left_join(x=sum.wf.api.2011.fr,y=meta.2011.no.duplicates)

# German API - BASIC
DFM.api.2011.de=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.de=dfm_trim(DFM.api.2011.de,min_termfreq = 3)
wf.api.2011.de=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de),dir=c(left.fix,right.fix),verbose=T)
sum.wf.api.2011.de=data.frame(est.api.de=wf.api.2011.de$theta,
                              lwr.api.de=wf.api.2011.de$theta-1.96*wf.api.2011.de$se.theta,
                              upr.api.de=wf.api.2011.de$theta+1.96*wf.api.2011.de$se.theta,
                              Name=wf.api.2011.de$docs,stringsAsFactors = F)
sum.wf.api.2011.de=left_join(x=sum.wf.api.2011.de,y=meta.2011.no.duplicates)

# Join all together - BASIC
joined.wf.speaker.basic=left_join(sum.wf.human.2011.fr,sum.wf.youtube.2011.fr%>%
                                    select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.human.2011.de%>%
                                    select(est.human.de,lwr.human.de,upr.human.de,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.youtube.2011.de%>%
                                    select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.human.2011.en%>%
                                    select(est.human.en,lwr.human.en,upr.human.en,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.youtube.2011.en%>%
                                    select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.api.2011.en%>%
                                    select(est.api.en,lwr.api.en,upr.api.en,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.api.2011.de%>%
                                    select(est.api.de,lwr.api.de,upr.api.de,Name),by="Name")
joined.wf.speaker.basic=left_join(joined.wf.speaker.basic,sum.wf.api.2011.fr%>%
                                    select(est.api.fr,lwr.api.fr,upr.api.fr,Name),by="Name")

#######################################################################
# Wordfish by Party - BASIC
#######################################################################

# English human by Party - BASIC
DFM.human.2011.en.party=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.human.2011.en.party=dfm_trim(DFM.human.2011.en.party,min_termfreq = 3)
left.fix.parties=which(row.names(DFM.human.2011.en.party)=="Commission")
right.fix.parties=which(row.names(DFM.human.2011.en.party)=="ECR")
wf.human.2011.en.party=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.human.2011.en.party=data.frame(est.human.en=wf.human.2011.en.party$theta,
                                         lwr.human.en=wf.human.2011.en.party$theta-1.96*wf.human.2011.en.party$se.theta,
                                         upr.human.en=wf.human.2011.en.party$theta+1.96*wf.human.2011.en.party$se.theta,
                                         Name=wf.human.2011.en.party$docs,stringsAsFactors = F)

# French human by Party - BASIC
DFM.human.2011.fr.party=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.human.2011.fr.party=dfm_trim(DFM.human.2011.fr.party,min_termfreq = 3)
wf.human.2011.fr.party=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.human.2011.fr.party=data.frame(est.human.fr=wf.human.2011.fr.party$theta,
                                         lwr.human.fr=wf.human.2011.fr.party$theta-1.96*wf.human.2011.fr.party$se.theta,
                                         upr.human.fr=wf.human.2011.fr.party$theta+1.96*wf.human.2011.fr.party$se.theta,
                                         Name=wf.human.2011.fr.party$docs,stringsAsFactors = F)

# German human by Party - BASIC
DFM.human.2011.de.party=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.human.2011.de.party=dfm_trim(DFM.human.2011.de.party,min_termfreq = 3)
wf.human.2011.de.party=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.human.2011.de.party=data.frame(est.human.de=wf.human.2011.de.party$theta,
                                         lwr.human.de=wf.human.2011.de.party$theta-1.96*wf.human.2011.de.party$se.theta,
                                         upr.human.de=wf.human.2011.de.party$theta+1.96*wf.human.2011.de.party$se.theta,
                                         Name=wf.human.2011.de.party$docs,stringsAsFactors = F)

# English Youtube by Party - BASIC
DFM.youtube.2011.en.party=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.youtube.2011.en.party=dfm_trim(DFM.youtube.2011.en.party,min_termfreq = 3)
wf.youtube.2011.en.party=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.youtube.2011.en.party=data.frame(est.youtube.en=wf.youtube.2011.en.party$theta,
                                        lwr.youtube.en=wf.youtube.2011.en.party$theta-1.96*wf.youtube.2011.en.party$se.theta,
                                        upr.youtube.en=wf.youtube.2011.en.party$theta+1.96*wf.youtube.2011.en.party$se.theta,
                                        Name=wf.youtube.2011.en.party$docs,stringsAsFactors = F)

# French Youtube by Party - BASIC
DFM.youtube.2011.fr.party=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.youtube.2011.fr.party=dfm_trim(DFM.youtube.2011.fr.party,min_termfreq = 3)
wf.youtube.2011.fr.party=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.youtube.2011.fr.party=data.frame(est.youtube.fr=wf.youtube.2011.fr.party$theta,
                                        lwr.youtube.fr=wf.youtube.2011.fr.party$theta-1.96*wf.youtube.2011.fr.party$se.theta,
                                        upr.youtube.fr=wf.youtube.2011.fr.party$theta+1.96*wf.youtube.2011.fr.party$se.theta,
                                        Name=wf.youtube.2011.fr.party$docs,stringsAsFactors = F)

# German Youtube by Party - BASIC
DFM.youtube.2011.de.party=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.youtube.2011.de.party=dfm_trim(DFM.youtube.2011.de.party,min_termfreq = 3)
wf.youtube.2011.de.party=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.youtube.2011.de.party=data.frame(est.youtube.de=wf.youtube.2011.de.party$theta,
                                        lwr.youtube.de=wf.youtube.2011.de.party$theta-1.96*wf.youtube.2011.de.party$se.theta,
                                        upr.youtube.de=wf.youtube.2011.de.party$theta+1.96*wf.youtube.2011.de.party$se.theta,
                                        Name=wf.youtube.2011.de.party$docs,stringsAsFactors = F)

# English API by Party - BASIC
DFM.api.2011.en.party=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.api.2011.en.party=dfm_trim(DFM.api.2011.en.party,min_termfreq = 3)
wf.api.2011.en.party=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.api.2011.en.party=data.frame(est.api.en=wf.api.2011.en.party$theta,
                                    lwr.api.en=wf.api.2011.en.party$theta-1.96*wf.api.2011.en.party$se.theta,
                                    upr.api.en=wf.api.2011.en.party$theta+1.96*wf.api.2011.en.party$se.theta,
                                    Name=wf.api.2011.en.party$docs,stringsAsFactors = F)

# French API by Party - BASIC
DFM.api.2011.fr.party=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.api.2011.fr.party=dfm_trim(DFM.api.2011.fr.party,min_termfreq = 3)
wf.api.2011.fr.party=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.api.2011.fr.party=data.frame(est.api.fr=wf.api.2011.fr.party$theta,
                                    lwr.api.fr=wf.api.2011.fr.party$theta-1.96*wf.api.2011.fr.party$se.theta,
                                    upr.api.fr=wf.api.2011.fr.party$theta+1.96*wf.api.2011.fr.party$se.theta,
                                    Name=wf.api.2011.fr.party$docs,stringsAsFactors = F)

# German API by Party - BASIC
DFM.api.2011.de.party=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.api.2011.de.party=dfm_trim(DFM.api.2011.de.party,min_termfreq = 3)
wf.api.2011.de.party=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de.party),dir=c(left.fix.parties,right.fix.parties),verbose=T)
sum.wf.api.2011.de.party=data.frame(est.api.de=wf.api.2011.de.party$theta,
                                    lwr.api.de=wf.api.2011.de.party$theta-1.96*wf.api.2011.de.party$se.theta,
                                    upr.api.de=wf.api.2011.de.party$theta+1.96*wf.api.2011.de.party$se.theta,
                                    Name=wf.api.2011.de.party$docs,stringsAsFactors = F)

# Combine
joined.wf.party.basic=left_join(sum.wf.human.2011.fr.party,sum.wf.youtube.2011.fr.party%>%
                                  select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.human.2011.de.party%>%
                                  select(est.human.de,lwr.human.de,upr.human.de,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.youtube.2011.de.party%>%
                                  select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.human.2011.en.party%>%
                                  select(est.human.en,lwr.human.en,upr.human.en,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.youtube.2011.en.party%>%
                                  select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.api.2011.en.party%>%
                                  select(est.api.en,lwr.api.en,upr.api.en,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.api.2011.de.party%>%
                                  select(est.api.de,lwr.api.de,upr.api.de,Name),by="Name")
joined.wf.party.basic=left_join(joined.wf.party.basic,sum.wf.api.2011.fr.party%>%
                                  select(est.api.fr,lwr.api.fr,upr.api.fr,Name),by="Name")

#######################################################################
# Wordfish by speaker - STEMMING
#######################################################################

# English human - STEMMING
DFM.human.2011.en.stemming=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.human.2011.en.stemming=dfm_trim(DFM.human.2011.en.stemming,min_termfreq = 3)
left.fix.stemming=which(row.names(DFM.human.2011.en.stemming)=="José Manuel Barroso")
right.fix.stemming=which(row.names(DFM.human.2011.en.stemming)=="Jan Zahradil")
wf.human.2011.en.stemming=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.human.2011.en.stemming=data.frame(est.human.en=wf.human.2011.en.stemming$theta,
                                            lwr.human.en=wf.human.2011.en.stemming$theta-1.96*wf.human.2011.en.stemming$se.theta,
                                            upr.human.en=wf.human.2011.en.stemming$theta+1.96*wf.human.2011.en.stemming$se.theta,
                                            Name=wf.human.2011.en.stemming$docs,stringsAsFactors = F)
sum.wf.human.2011.en.stemming=left_join(x=sum.wf.human.2011.en.stemming,y=meta.2011.no.duplicates,by="Name")

# French human - STEMMING
DFM.human.2011.fr.stemming=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.human.2011.fr.stemming=dfm_trim(DFM.human.2011.fr.stemming,min_termfreq = 3)
wf.human.2011.fr.stemming=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.human.2011.fr.stemming=data.frame(est.human.fr=wf.human.2011.fr.stemming$theta,
                                            lwr.human.fr=wf.human.2011.fr.stemming$theta-1.96*wf.human.2011.fr.stemming$se.theta,
                                            upr.human.fr=wf.human.2011.fr.stemming$theta+1.96*wf.human.2011.fr.stemming$se.theta,
                                            Name=wf.human.2011.fr.stemming$docs,stringsAsFactors = F)
sum.wf.human.2011.fr.stemming=left_join(x=sum.wf.human.2011.fr.stemming,y=meta.2011.no.duplicates,by="Name")

# German human - STEMMING
DFM.human.2011.de.stemming=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.human.2011.de.stemming=dfm_trim(DFM.human.2011.de.stemming,min_termfreq = 3)
wf.human.2011.de.stemming=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.human.2011.de.stemming=data.frame(est.human.de=wf.human.2011.de.stemming$theta,
                                            lwr.human.de=wf.human.2011.de.stemming$theta-1.96*wf.human.2011.de.stemming$se.theta,
                                            upr.human.de=wf.human.2011.de.stemming$theta+1.96*wf.human.2011.de.stemming$se.theta,
                                            Name=wf.human.2011.de.stemming$docs,stringsAsFactors = F)
sum.wf.human.2011.de.stemming=left_join(x=sum.wf.human.2011.de.stemming,y=meta.2011.no.duplicates,by="Name")

# English Youtube - STEMMING
DFM.youtube.2011.en.stemming=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.youtube.2011.en.stemming=dfm_trim(DFM.youtube.2011.en.stemming,min_termfreq = 3)
wf.youtube.2011.en.stemming=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.youtube.2011.en.stemming=data.frame(est.youtube.en=wf.youtube.2011.en.stemming$theta,
                                           lwr.youtube.en=wf.youtube.2011.en.stemming$theta-1.96*wf.youtube.2011.en.stemming$se.theta,
                                           upr.youtube.en=wf.youtube.2011.en.stemming$theta+1.96*wf.youtube.2011.en.stemming$se.theta,
                                           Name=wf.youtube.2011.en.stemming$docs,stringsAsFactors = F)
sum.wf.youtube.2011.en.stemming=left_join(x=sum.wf.youtube.2011.en.stemming,y=meta.2011.no.duplicates,by="Name")

# French Youtube - STEMMING
DFM.youtube.2011.fr.stemming=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.youtube.2011.fr.stemming=dfm_trim(DFM.youtube.2011.fr.stemming,min_termfreq = 3)
wf.youtube.2011.fr.stemming=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.youtube.2011.fr.stemming=data.frame(est.youtube.fr=wf.youtube.2011.fr.stemming$theta,
                                           lwr.youtube.fr=wf.youtube.2011.fr.stemming$theta-1.96*wf.youtube.2011.fr.stemming$se.theta,
                                           upr.youtube.fr=wf.youtube.2011.fr.stemming$theta+1.96*wf.youtube.2011.fr.stemming$se.theta,
                                           Name=wf.youtube.2011.fr.stemming$docs,stringsAsFactors = F)
sum.wf.youtube.2011.fr.stemming=left_join(x=sum.wf.youtube.2011.fr.stemming,y=meta.2011.no.duplicates,by="Name")

# German Youtube - STEMMING
DFM.youtube.2011.de.stemming=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.youtube.2011.de.stemming=dfm_trim(DFM.youtube.2011.de.stemming,min_termfreq = 3)
wf.youtube.2011.de.stemming=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.youtube.2011.de.stemming=data.frame(est.youtube.de=wf.youtube.2011.de.stemming$theta,
                                           lwr.youtube.de=wf.youtube.2011.de.stemming$theta-1.96*wf.youtube.2011.de.stemming$se.theta,
                                           upr.youtube.de=wf.youtube.2011.de.stemming$theta+1.96*wf.youtube.2011.de.stemming$se.theta,
                                           Name=wf.youtube.2011.de.stemming$docs,stringsAsFactors = F)
sum.wf.youtube.2011.de.stemming=left_join(x=sum.wf.youtube.2011.de.stemming,y=meta.2011.no.duplicates,by="Name")

# English API - STEMMING
DFM.api.2011.en.stemming=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.api.2011.en.stemming=dfm_trim(DFM.api.2011.en.stemming,min_termfreq = 3)
wf.api.2011.en.stemming=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.api.2011.en.stemming=data.frame(est.api.en=wf.api.2011.en.stemming$theta,
                                       lwr.api.en=wf.api.2011.en.stemming$theta-1.96*wf.api.2011.en.stemming$se.theta,
                                       upr.api.en=wf.api.2011.en.stemming$theta+1.96*wf.api.2011.en.stemming$se.theta,
                                       Name=wf.api.2011.en.stemming$docs,stringsAsFactors = F)
sum.wf.api.2011.en.stemming=left_join(x=sum.wf.api.2011.en.stemming,y=meta.2011.no.duplicates,by="Name")

# French API - STEMMING
DFM.api.2011.fr.stemming=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.api.2011.fr.stemming=dfm_trim(DFM.api.2011.fr.stemming,min_termfreq = 3)
wf.api.2011.fr.stemming=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.api.2011.fr.stemming=data.frame(est.api.fr=wf.api.2011.fr.stemming$theta,
                                       lwr.api.fr=wf.api.2011.fr.stemming$theta-1.96*wf.api.2011.fr.stemming$se.theta,
                                       upr.api.fr=wf.api.2011.fr.stemming$theta+1.96*wf.api.2011.fr.stemming$se.theta,
                                       Name=wf.api.2011.fr.stemming$docs,stringsAsFactors = F)
sum.wf.api.2011.fr.stemming=left_join(x=sum.wf.api.2011.fr.stemming,y=meta.2011.no.duplicates,by="Name")

# German API - STEMMING
DFM.api.2011.de.stemming=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",stem=T)
DFM.api.2011.de.stemming=dfm_trim(DFM.api.2011.de.stemming,min_termfreq = 3)
wf.api.2011.de.stemming=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de.stemming),dir=c(left.fix.stemming,right.fix.stemming),verbose=T)
sum.wf.api.2011.de.stemming=data.frame(est.api.de=wf.api.2011.de.stemming$theta,
                                       lwr.api.de=wf.api.2011.de.stemming$theta-1.96*wf.api.2011.de.stemming$se.theta,
                                       upr.api.de=wf.api.2011.de.stemming$theta+1.96*wf.api.2011.de.stemming$se.theta,
                                       Name=wf.api.2011.de.stemming$docs,stringsAsFactors = F)
sum.wf.api.2011.de.stemming=left_join(x=sum.wf.api.2011.de.stemming,y=meta.2011.no.duplicates,by="Name")

# Combine
joined.wf.speaker.stemming=left_join(sum.wf.human.2011.fr.stemming,sum.wf.youtube.2011.fr.stemming%>%
                                       select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.human.2011.de.stemming%>%
                                       select(est.human.de,lwr.human.de,upr.human.de,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.youtube.2011.de.stemming%>%
                                       select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.human.2011.en.stemming%>%
                                       select(est.human.en,lwr.human.en,upr.human.en,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.youtube.2011.en.stemming%>%
                                       select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.api.2011.en.stemming%>%
                                       select(est.api.en,lwr.api.en,upr.api.en,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.api.2011.de.stemming%>%
                                       select(est.api.de,lwr.api.de,upr.api.de,Number),by="Number")
joined.wf.speaker.stemming=left_join(joined.wf.speaker.stemming,sum.wf.api.2011.fr.stemming%>%
                                       select(est.api.fr,lwr.api.fr,upr.api.fr,Number),by="Number")

#######################################################################
# Wordfish by Speaker - 100 TOKENS
#######################################################################

# create variable to distinguish texts with more than 100 tokens
docvars(human.2011.en,"min100")=ifelse(ntoken(human.2011.en)>99,1,0)
docvars(human.2011.de,"min100")=ifelse(ntoken(human.2011.de)>99,1,0)
docvars(human.2011.fr,"min100")=ifelse(ntoken(human.2011.fr)>99,1,0)
docvars(youtube.2011.en,"min100")=docvars(human.2011.en,"min100")
docvars(youtube.2011.de,"min100")=docvars(human.2011.de,"min100")
docvars(youtube.2011.fr,"min100")=docvars(human.2011.fr,"min100")
docvars(api.2011.en,"min100")=docvars(human.2011.en,"min100")
docvars(api.2011.de,"min100")=docvars(human.2011.de,"min100")
docvars(api.2011.fr,"min100")=docvars(human.2011.fr,"min100")
docvars(protocol.2011.en,"min100")=docvars(human.2011.en,"min100")

# eliminate short texts from both youtube AND human corpuus
human.2011.en.100=corpus_subset(human.2011.en,min100==1)
human.2011.de.100=corpus_subset(human.2011.de,min100==1)
human.2011.fr.100=corpus_subset(human.2011.fr,min100==1)
youtube.2011.en.100=corpus_subset(youtube.2011.en,min100==1)
youtube.2011.de.100=corpus_subset(youtube.2011.de,min100==1)
youtube.2011.fr.100=corpus_subset(youtube.2011.fr,min100==1)
api.2011.en.100=corpus_subset(api.2011.en,min100==1)
api.2011.de.100=corpus_subset(api.2011.de,min100==1)
api.2011.fr.100=corpus_subset(api.2011.fr,min100==1)
protocol.2011.en.100=corpus_subset(protocol.2011.en,min100==1)

# English human> 100 tokens
DFM.human.2011.en.100=dfm(human.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.en.100=dfm_trim(DFM.human.2011.en.100,min_termfreq = 3)
left.fix.100.en=which(row.names(DFM.human.2011.en.100)=="José Manuel Barroso")
right.fix.100.en=which(row.names(DFM.human.2011.en.100)=="Jan Zahradil")
wf.human.2011.en.100=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en.100),dir=c(left.fix.100.en,right.fix.100.en),verbose=T)
sum.wf.human.2011.en.100=data.frame(est.human.en=wf.human.2011.en.100$theta,
                                       lwr.human.en=wf.human.2011.en.100$theta-1.96*wf.human.2011.en.100$se.theta,
                                       upr.human.en=wf.human.2011.en.100$theta+1.96*wf.human.2011.en.100$se.theta,
                                       Name=wf.human.2011.en.100$docs,stringsAsFactors = F)
sum.wf.human.2011.en.100=left_join(x=sum.wf.human.2011.en.100,y=meta.2011.no.duplicates,by="Name")

# French human > 100 tokens
DFM.human.2011.fr.100=dfm(human.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.fr.100=dfm_trim(DFM.human.2011.fr.100,min_termfreq = 3)
left.fix.100.fr=which(row.names(DFM.human.2011.fr.100)=="José Manuel Barroso")
right.fix.100.fr=which(row.names(DFM.human.2011.fr.100)=="Jan Zahradil")
wf.human.2011.fr.100=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr.100),dir=c(left.fix.100.fr,right.fix.100.fr),verbose=T)
sum.wf.human.2011.fr.100=data.frame(est.human.fr=wf.human.2011.fr.100$theta,
                                       lwr.human.fr=wf.human.2011.fr.100$theta-1.96*wf.human.2011.fr.100$se.theta,
                                       upr.human.fr=wf.human.2011.fr.100$theta+1.96*wf.human.2011.fr.100$se.theta,
                                       Name=wf.human.2011.fr.100$docs,stringsAsFactors = F)
sum.wf.human.2011.fr.100=left_join(x=sum.wf.human.2011.fr.100,y=meta.2011.no.duplicates,by="Name")

# German human> 100 tokens
DFM.human.2011.de.100=dfm(human.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.de.100=dfm_trim(DFM.human.2011.de.100,min_termfreq = 3)
left.fix.100.de=which(row.names(DFM.human.2011.de.100)=="José Manuel Barroso")
right.fix.100.de=which(row.names(DFM.human.2011.de.100)=="Jan Zahradil")
wf.human.2011.de.100=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de.100),dir=c(left.fix.100.de,right.fix.100.de),verbose=T)
sum.wf.human.2011.de.100=data.frame(est.human.de=wf.human.2011.de.100$theta,
                                       lwr.human.de=wf.human.2011.de.100$theta-1.96*wf.human.2011.de.100$se.theta,
                                       upr.human.de=wf.human.2011.de.100$theta+1.96*wf.human.2011.de.100$se.theta,
                                       Name=wf.human.2011.de.100$docs,stringsAsFactors = F)
sum.wf.human.2011.de.100=left_join(x=sum.wf.human.2011.de.100,y=meta.2011.no.duplicates,by="Name")

# English Youtube> 100 tokens
DFM.youtube.2011.en.100=dfm(youtube.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.en.100=dfm_trim(DFM.youtube.2011.en.100,min_termfreq = 3)
wf.youtube.2011.en.100=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en.100),dir=c(left.fix.100.en,right.fix.100.en),verbose=T)
sum.wf.youtube.2011.en.100=data.frame(est.youtube.en=wf.youtube.2011.en.100$theta,
                                      lwr.youtube.en=wf.youtube.2011.en.100$theta-1.96*wf.youtube.2011.en.100$se.theta,
                                      upr.youtube.en=wf.youtube.2011.en.100$theta+1.96*wf.youtube.2011.en.100$se.theta,
                                      Name=wf.youtube.2011.en.100$docs,stringsAsFactors = F)
sum.wf.youtube.2011.en.100=left_join(x=sum.wf.youtube.2011.en.100,y=meta.2011.no.duplicates,by="Name")

# French Youtube> 100 tokens
DFM.youtube.2011.fr.100=dfm(youtube.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.fr.100=dfm_trim(DFM.youtube.2011.fr.100,min_termfreq = 3)
wf.youtube.2011.fr.100=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr.100),dir=c(left.fix.100.fr,right.fix.100.fr),verbose=T)
sum.wf.youtube.2011.fr.100=data.frame(est.youtube.fr=wf.youtube.2011.fr.100$theta,
                                      lwr.youtube.fr=wf.youtube.2011.fr.100$theta-1.96*wf.youtube.2011.fr.100$se.theta,
                                      upr.youtube.fr=wf.youtube.2011.fr.100$theta+1.96*wf.youtube.2011.fr.100$se.theta,
                                      Name=wf.youtube.2011.fr.100$docs,stringsAsFactors = F)
sum.wf.youtube.2011.fr.100=left_join(x=sum.wf.youtube.2011.fr.100,y=meta.2011.no.duplicates,by="Name")

# German Youtube> 100 tokens
DFM.youtube.2011.de.100=dfm(youtube.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.de.100=dfm_trim(DFM.youtube.2011.de.100,min_termfreq = 3)
wf.youtube.2011.de.100=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de.100),dir=c(left.fix.100.de,right.fix.100.de),verbose=T)
sum.wf.youtube.2011.de.100=data.frame(est.youtube.de=wf.youtube.2011.de.100$theta,
                                      lwr.youtube.de=wf.youtube.2011.de.100$theta-1.96*wf.youtube.2011.de.100$se.theta,
                                      upr.youtube.de=wf.youtube.2011.de.100$theta+1.96*wf.youtube.2011.de.100$se.theta,
                                      Name=wf.youtube.2011.de.100$docs,stringsAsFactors = F)
sum.wf.youtube.2011.de.100=left_join(x=sum.wf.youtube.2011.de.100,y=meta.2011.no.duplicates,by="Name")

# English API> 100 tokens
DFM.api.2011.en.100=dfm(api.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.en.100=dfm_trim(DFM.api.2011.en.100,min_termfreq = 3)
wf.api.2011.en.100=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en.100),dir=c(left.fix.100.en,right.fix.100.en),verbose=T)
sum.wf.api.2011.en.100=data.frame(est.api.en=wf.api.2011.en.100$theta,
                                  lwr.api.en=wf.api.2011.en.100$theta-1.96*wf.api.2011.en.100$se.theta,
                                  upr.api.en=wf.api.2011.en.100$theta+1.96*wf.api.2011.en.100$se.theta,
                                  Name=wf.api.2011.en.100$docs,stringsAsFactors = F)
sum.wf.api.2011.en.100=left_join(x=sum.wf.api.2011.en.100,y=meta.2011.no.duplicates,by="Name")

# French API> 100 tokens
DFM.api.2011.fr.100=dfm(api.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.fr.100=dfm_trim(DFM.api.2011.fr.100,min_termfreq = 3)
wf.api.2011.fr.100=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr.100),dir=c(left.fix.100.fr,right.fix.100.fr),verbose=T)
sum.wf.api.2011.fr.100=data.frame(est.api.fr=wf.api.2011.fr.100$theta,
                                  lwr.api.fr=wf.api.2011.fr.100$theta-1.96*wf.api.2011.fr.100$se.theta,
                                  upr.api.fr=wf.api.2011.fr.100$theta+1.96*wf.api.2011.fr.100$se.theta,
                                  Name=wf.api.2011.fr.100$docs,stringsAsFactors = F)
sum.wf.api.2011.fr.100=left_join(x=sum.wf.api.2011.fr.100,y=meta.2011.no.duplicates,by="Name")

# German API> 100 tokens
DFM.api.2011.de.100=dfm(api.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.de.100=dfm_trim(DFM.api.2011.de.100,min_termfreq = 3)
wf.api.2011.de.100=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de.100),dir=c(left.fix.100.de,right.fix.100.de),verbose=T)
sum.wf.api.2011.de.100=data.frame(est.api.de=wf.api.2011.de.100$theta,
                                  lwr.api.de=wf.api.2011.de.100$theta-1.96*wf.api.2011.de.100$se.theta,
                                  upr.api.de=wf.api.2011.de.100$theta+1.96*wf.api.2011.de.100$se.theta,
                                  Name=wf.api.2011.de.100$docs,stringsAsFactors = F)
sum.wf.api.2011.de.100=left_join(x=sum.wf.api.2011.de.100,y=meta.2011.no.duplicates,by="Name")

#Combine French
joined.wf.100.fr=left_join(sum.wf.human.2011.fr.100,sum.wf.youtube.2011.fr.100%>%
                             select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Name),by="Name")
joined.wf.100.fr=left_join(joined.wf.100.fr,sum.wf.api.2011.fr.100%>%
                             select(est.api.fr,lwr.api.fr,upr.api.fr,Name),by="Name")
#Combine English
joined.wf.100.en=left_join(sum.wf.human.2011.en.100,sum.wf.youtube.2011.en.100%>%
                             select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Name),by="Name")
joined.wf.100.en=left_join(joined.wf.100.en,sum.wf.api.2011.en.100%>%
                             select(est.api.en,lwr.api.en,upr.api.en,Name),by="Name")
#Combine German
joined.wf.100.de=left_join(sum.wf.human.2011.de.100,sum.wf.youtube.2011.de.100%>%
                             select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Name),by="Name")
joined.wf.100.de=left_join(joined.wf.100.de,sum.wf.api.2011.de.100%>%
                             select(est.api.de,lwr.api.de,upr.api.de,Name),by="Name")

#######################################################################
# Wordfish by Speaker - 200 TOKENS
#######################################################################

# create variable to distinguish texts with more than 200 tokens
docvars(human.2011.en,"min200")=ifelse(ntoken(human.2011.en)>199,1,0)
docvars(human.2011.de,"min200")=ifelse(ntoken(human.2011.de)>199,1,0)
docvars(human.2011.fr,"min200")=ifelse(ntoken(human.2011.fr)>199,1,0)
docvars(youtube.2011.en,"min200")=docvars(human.2011.en,"min200")
docvars(youtube.2011.de,"min200")=docvars(human.2011.de,"min200")
docvars(youtube.2011.fr,"min200")=docvars(human.2011.fr,"min200")
docvars(api.2011.en,"min200")=docvars(human.2011.en,"min200")
docvars(api.2011.de,"min200")=docvars(human.2011.de,"min200")
docvars(api.2011.fr,"min200")=docvars(human.2011.fr,"min200")
docvars(protocol.2011.en,"min200")=docvars(human.2011.en,"min200")

# eliminate short texts from both youtube AND human corpuus
human.2011.en.200=corpus_subset(human.2011.en,min200==1)
human.2011.de.200=corpus_subset(human.2011.de,min200==1)
human.2011.fr.200=corpus_subset(human.2011.fr,min200==1)
youtube.2011.en.200=corpus_subset(youtube.2011.en,min200==1)
youtube.2011.de.200=corpus_subset(youtube.2011.de,min200==1)
youtube.2011.fr.200=corpus_subset(youtube.2011.fr,min200==1)
api.2011.en.200=corpus_subset(api.2011.en,min200==1)
api.2011.de.200=corpus_subset(api.2011.de,min200==1)
api.2011.fr.200=corpus_subset(api.2011.fr,min200==1)
protocol.2011.en.200=corpus_subset(protocol.2011.en,min200==1)

# English human> 200 tokens
DFM.human.2011.en.200=dfm(human.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.en.200=dfm_trim(DFM.human.2011.en.200,min_termfreq = 3)
left.fix.200.en=which(row.names(DFM.human.2011.en.200)=="José Manuel Barroso")
right.fix.200.en=which(row.names(DFM.human.2011.en.200)=="Jan Zahradil")
wf.human.2011.en.200=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en.200),dir=c(left.fix.200.en,right.fix.200.en),verbose=T)
sum.wf.human.2011.en.200=data.frame(est.human.en=wf.human.2011.en.200$theta,
                                       lwr.human.en=wf.human.2011.en.200$theta-1.96*wf.human.2011.en.200$se.theta,
                                       upr.human.en=wf.human.2011.en.200$theta+1.96*wf.human.2011.en.200$se.theta,
                                       Name=wf.human.2011.en.200$docs,stringsAsFactors = F)
sum.wf.human.2011.en.200=left_join(x=sum.wf.human.2011.en.200,y=meta.2011.no.duplicates,by="Name")

# French human > 200 tokens
DFM.human.2011.fr.200=dfm(human.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.fr.200=dfm_trim(DFM.human.2011.fr.200,min_termfreq = 3)
left.fix.200.fr=which(row.names(DFM.human.2011.fr.200)=="José Manuel Barroso")
right.fix.200.fr=which(row.names(DFM.human.2011.fr.200)=="Jan Zahradil")
wf.human.2011.fr.200=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr.200),dir=c(left.fix.200.fr,right.fix.200.fr),verbose=T)
sum.wf.human.2011.fr.200=data.frame(est.human.fr=wf.human.2011.fr.200$theta,
                                       lwr.human.fr=wf.human.2011.fr.200$theta-1.96*wf.human.2011.fr.200$se.theta,
                                       upr.human.fr=wf.human.2011.fr.200$theta+1.96*wf.human.2011.fr.200$se.theta,
                                       Name=wf.human.2011.fr.200$docs,stringsAsFactors = F)
sum.wf.human.2011.fr.200=left_join(x=sum.wf.human.2011.fr.200,y=meta.2011.no.duplicates,by="Name")

# German human> 200 tokens
DFM.human.2011.de.200=dfm(human.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.de.200=dfm_trim(DFM.human.2011.de.200,min_termfreq = 3)
left.fix.200.de=which(row.names(DFM.human.2011.de.200)=="José Manuel Barroso")
right.fix.200.de=which(row.names(DFM.human.2011.de.200)=="Jan Zahradil")
wf.human.2011.de.200=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de.200),dir=c(left.fix.200.de,right.fix.200.de),verbose=T)
sum.wf.human.2011.de.200=data.frame(est.human.de=wf.human.2011.de.200$theta,
                                       lwr.human.de=wf.human.2011.de.200$theta-1.96*wf.human.2011.de.200$se.theta,
                                       upr.human.de=wf.human.2011.de.200$theta+1.96*wf.human.2011.de.200$se.theta,
                                       Name=wf.human.2011.de.200$docs,stringsAsFactors = F)
sum.wf.human.2011.de.200=left_join(x=sum.wf.human.2011.de.200,y=meta.2011.no.duplicates,by="Name")

# English Youtube> 200 tokens
DFM.youtube.2011.en.200=dfm(youtube.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.en.200=dfm_trim(DFM.youtube.2011.en.200,min_termfreq = 3)
wf.youtube.2011.en.200=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en.200),dir=c(left.fix.200.en,right.fix.200.en),verbose=T)
sum.wf.youtube.2011.en.200=data.frame(est.youtube.en=wf.youtube.2011.en.200$theta,
                                      lwr.youtube.en=wf.youtube.2011.en.200$theta-1.96*wf.youtube.2011.en.200$se.theta,
                                      upr.youtube.en=wf.youtube.2011.en.200$theta+1.96*wf.youtube.2011.en.200$se.theta,
                                      Name=wf.youtube.2011.en.200$docs,stringsAsFactors = F)
sum.wf.youtube.2011.en.200=left_join(x=sum.wf.youtube.2011.en.200,y=meta.2011.no.duplicates,by="Name")

# French Youtube> 200 tokens
DFM.youtube.2011.fr.200=dfm(youtube.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.fr.200=dfm_trim(DFM.youtube.2011.fr.200,min_termfreq = 3)
wf.youtube.2011.fr.200=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr.200),dir=c(left.fix.200.fr,right.fix.200.fr),verbose=T)
sum.wf.youtube.2011.fr.200=data.frame(est.youtube.fr=wf.youtube.2011.fr.200$theta,
                                      lwr.youtube.fr=wf.youtube.2011.fr.200$theta-1.96*wf.youtube.2011.fr.200$se.theta,
                                      upr.youtube.fr=wf.youtube.2011.fr.200$theta+1.96*wf.youtube.2011.fr.200$se.theta,
                                      Name=wf.youtube.2011.fr.200$docs,stringsAsFactors = F)
sum.wf.youtube.2011.fr.200=left_join(x=sum.wf.youtube.2011.fr.200,y=meta.2011.no.duplicates,by="Name")

# German Youtube> 200 tokens
DFM.youtube.2011.de.200=dfm(youtube.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.de.200=dfm_trim(DFM.youtube.2011.de.200,min_termfreq = 3)
wf.youtube.2011.de.200=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de.200),dir=c(left.fix.200.de,right.fix.200.de),verbose=T)
sum.wf.youtube.2011.de.200=data.frame(est.youtube.de=wf.youtube.2011.de.200$theta,
                                      lwr.youtube.de=wf.youtube.2011.de.200$theta-1.96*wf.youtube.2011.de.200$se.theta,
                                      upr.youtube.de=wf.youtube.2011.de.200$theta+1.96*wf.youtube.2011.de.200$se.theta,
                                      Name=wf.youtube.2011.de.200$docs,stringsAsFactors = F)
sum.wf.youtube.2011.de.200=left_join(x=sum.wf.youtube.2011.de.200,y=meta.2011.no.duplicates,by="Name")

# English API> 200 tokens
DFM.api.2011.en.200=dfm(api.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.en.200=dfm_trim(DFM.api.2011.en.200,min_termfreq = 3)
wf.api.2011.en.200=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en.200),dir=c(left.fix.200.en,right.fix.200.en),verbose=T)
sum.wf.api.2011.en.200=data.frame(est.api.en=wf.api.2011.en.200$theta,
                                  lwr.api.en=wf.api.2011.en.200$theta-1.96*wf.api.2011.en.200$se.theta,
                                  upr.api.en=wf.api.2011.en.200$theta+1.96*wf.api.2011.en.200$se.theta,
                                  Name=wf.api.2011.en.200$docs,stringsAsFactors = F)
sum.wf.api.2011.en.200=left_join(x=sum.wf.api.2011.en.200,y=meta.2011.no.duplicates,by="Name")

# French API> 200 tokens
DFM.api.2011.fr.200=dfm(api.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.fr.200=dfm_trim(DFM.api.2011.fr.200,min_termfreq = 3)
wf.api.2011.fr.200=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr.200),dir=c(left.fix.200.fr,right.fix.200.fr),verbose=T)
sum.wf.api.2011.fr.200=data.frame(est.api.fr=wf.api.2011.fr.200$theta,
                                  lwr.api.fr=wf.api.2011.fr.200$theta-1.96*wf.api.2011.fr.200$se.theta,
                                  upr.api.fr=wf.api.2011.fr.200$theta+1.96*wf.api.2011.fr.200$se.theta,
                                  Name=wf.api.2011.fr.200$docs,stringsAsFactors = F)
sum.wf.api.2011.fr.200=left_join(x=sum.wf.api.2011.fr.200,y=meta.2011.no.duplicates,by="Name")

# German API> 200 tokens
DFM.api.2011.de.200=dfm(api.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.de.200=dfm_trim(DFM.api.2011.de.200,min_termfreq = 3)
wf.api.2011.de.200=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de.200),dir=c(left.fix.200.de,right.fix.200.de),verbose=T)
sum.wf.api.2011.de.200=data.frame(est.api.de=wf.api.2011.de.200$theta,
                                  lwr.api.de=wf.api.2011.de.200$theta-1.96*wf.api.2011.de.200$se.theta,
                                  upr.api.de=wf.api.2011.de.200$theta+1.96*wf.api.2011.de.200$se.theta,
                                  Name=wf.api.2011.de.200$docs,stringsAsFactors = F)
sum.wf.api.2011.de.200=left_join(x=sum.wf.api.2011.de.200,y=meta.2011.no.duplicates,by="Name")

#Combine French
joined.wf.200.fr=left_join(sum.wf.human.2011.fr.200,sum.wf.youtube.2011.fr.200%>%
                             select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Name),by="Name")
joined.wf.200.fr=left_join(joined.wf.200.fr,sum.wf.api.2011.fr.200%>%
                             select(est.api.fr,lwr.api.fr,upr.api.fr,Name),by="Name")
#Combine English
joined.wf.200.en=left_join(sum.wf.human.2011.en.200,sum.wf.youtube.2011.en.200%>%
                             select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Name),by="Name")
joined.wf.200.en=left_join(joined.wf.200.en,sum.wf.api.2011.en.200%>%
                             select(est.api.en,lwr.api.en,upr.api.en,Name),by="Name")
#Combine German
joined.wf.200.de=left_join(sum.wf.human.2011.de.200,sum.wf.youtube.2011.de.200%>%
                             select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Name),by="Name")
joined.wf.200.de=left_join(joined.wf.200.de,sum.wf.api.2011.de.200%>%
                             select(est.api.de,lwr.api.de,upr.api.de,Name),by="Name")

#######################################################################
# Wordfish by speaker - STOPWORDS
#######################################################################
# English human - stopwords
DFM.human.2011.en.stopwords=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("english"))
DFM.human.2011.en.stopwords=dfm_trim(DFM.human.2011.en.stopwords,min_termfreq = 3)
left.fix.stopwords=which(row.names(DFM.human.2011.en.stopwords)=="José Manuel Barroso")
right.fix.stopwords=which(row.names(DFM.human.2011.en.stopwords)=="Jan Zahradil")
wf.human.2011.en.stopwords=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.human.2011.en.stopwords=data.frame(est.human.en=wf.human.2011.en.stopwords$theta,
                                             lwr.human.en=wf.human.2011.en.stopwords$theta-1.96*wf.human.2011.en.stopwords$se.theta,
                                             upr.human.en=wf.human.2011.en.stopwords$theta+1.96*wf.human.2011.en.stopwords$se.theta,
                                             Name=wf.human.2011.en.stopwords$docs,stringsAsFactors = F)
sum.wf.human.2011.en.stopwords=left_join(x=sum.wf.human.2011.en.stopwords,y=meta.2011.no.duplicates,by="Name")

# French human - stopwords
DFM.human.2011.fr.stopwords=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("french"))
DFM.human.2011.fr.stopwords=dfm_trim(DFM.human.2011.fr.stopwords,min_termfreq = 3)
wf.human.2011.fr.stopwords=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.human.2011.fr.stopwords=data.frame(est.human.fr=wf.human.2011.fr.stopwords$theta,
                                             lwr.human.fr=wf.human.2011.fr.stopwords$theta-1.96*wf.human.2011.fr.stopwords$se.theta,
                                             upr.human.fr=wf.human.2011.fr.stopwords$theta+1.96*wf.human.2011.fr.stopwords$se.theta,
                                             Name=wf.human.2011.fr.stopwords$docs,stringsAsFactors = F)
sum.wf.human.2011.fr.stopwords=left_join(x=sum.wf.human.2011.fr.stopwords,y=meta.2011.no.duplicates,by="Name")

# German human - stopwords
DFM.human.2011.de.stopwords=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("german"))
DFM.human.2011.de.stopwords=dfm_trim(DFM.human.2011.de.stopwords,min_termfreq = 3)
wf.human.2011.de.stopwords=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.human.2011.de.stopwords=data.frame(est.human.de=wf.human.2011.de.stopwords$theta,
                                             lwr.human.de=wf.human.2011.de.stopwords$theta-1.96*wf.human.2011.de.stopwords$se.theta,
                                             upr.human.de=wf.human.2011.de.stopwords$theta+1.96*wf.human.2011.de.stopwords$se.theta,
                                             Name=wf.human.2011.de.stopwords$docs,stringsAsFactors = F)
sum.wf.human.2011.de.stopwords=left_join(x=sum.wf.human.2011.de.stopwords,y=meta.2011.no.duplicates,by="Name")

# English Youtube - stopwords
DFM.youtube.2011.en.stopwords=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("english"))
DFM.youtube.2011.en.stopwords=dfm_trim(DFM.youtube.2011.en.stopwords,min_termfreq = 3)
wf.youtube.2011.en.stopwords=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.youtube.2011.en.stopwords=data.frame(est.youtube.en=wf.youtube.2011.en.stopwords$theta,
                                            lwr.youtube.en=wf.youtube.2011.en.stopwords$theta-1.96*wf.youtube.2011.en.stopwords$se.theta,
                                            upr.youtube.en=wf.youtube.2011.en.stopwords$theta+1.96*wf.youtube.2011.en.stopwords$se.theta,
                                            Name=wf.youtube.2011.en.stopwords$docs,stringsAsFactors = F)
sum.wf.youtube.2011.en.stopwords=left_join(x=sum.wf.youtube.2011.en.stopwords,y=meta.2011.no.duplicates,by="Name")

# French Youtube - stopwords
DFM.youtube.2011.fr.stopwords=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("french"))
DFM.youtube.2011.fr.stopwords=dfm_trim(DFM.youtube.2011.fr.stopwords,min_termfreq = 3)
wf.youtube.2011.fr.stopwords=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.youtube.2011.fr.stopwords=data.frame(est.youtube.fr=wf.youtube.2011.fr.stopwords$theta,
                                            lwr.youtube.fr=wf.youtube.2011.fr.stopwords$theta-1.96*wf.youtube.2011.fr.stopwords$se.theta,
                                            upr.youtube.fr=wf.youtube.2011.fr.stopwords$theta+1.96*wf.youtube.2011.fr.stopwords$se.theta,
                                            Name=wf.youtube.2011.fr.stopwords$docs,stringsAsFactors = F)
sum.wf.youtube.2011.fr.stopwords=left_join(x=sum.wf.youtube.2011.fr.stopwords,y=meta.2011.no.duplicates,by="Name")

# German Youtube - stopwords
DFM.youtube.2011.de.stopwords=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("german"))
DFM.youtube.2011.de.stopwords=dfm_trim(DFM.youtube.2011.de.stopwords,min_termfreq = 3)
wf.youtube.2011.de.stopwords=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.youtube.2011.de.stopwords=data.frame(est.youtube.de=wf.youtube.2011.de.stopwords$theta,
                                            lwr.youtube.de=wf.youtube.2011.de.stopwords$theta-1.96*wf.youtube.2011.de.stopwords$se.theta,
                                            upr.youtube.de=wf.youtube.2011.de.stopwords$theta+1.96*wf.youtube.2011.de.stopwords$se.theta,
                                            Name=wf.youtube.2011.de.stopwords$docs,stringsAsFactors = F)
sum.wf.youtube.2011.de.stopwords=left_join(x=sum.wf.youtube.2011.de.stopwords,y=meta.2011.no.duplicates,by="Name")


# English API - stopwords
DFM.api.2011.en.stopwords=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("english"))
DFM.api.2011.en.stopwords=dfm_trim(DFM.api.2011.en.stopwords,min_termfreq = 3)
wf.api.2011.en.stopwords=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.api.2011.en.stopwords=data.frame(est.api.en=wf.api.2011.en.stopwords$theta,
                                        lwr.api.en=wf.api.2011.en.stopwords$theta-1.96*wf.api.2011.en.stopwords$se.theta,
                                        upr.api.en=wf.api.2011.en.stopwords$theta+1.96*wf.api.2011.en.stopwords$se.theta,
                                        Name=wf.api.2011.en.stopwords$docs,stringsAsFactors = F)
sum.wf.api.2011.en.stopwords=left_join(x=sum.wf.api.2011.en.stopwords,y=meta.2011.no.duplicates,by="Name")
tza.api=data.frame(DFM.api.2011.en.stopwords[row.names(DFM.api.2011.en.stopwords)=="Niki Tzavela",])
tza.api[,tza.api[1,]>0]
words.api=data.frame(words=wf.api.2011.en.stopwords$words,beta=wf.api.2011.en.stopwords$beta)
words.ori=data.frame(words=wf.human.2011.en.stopwords$words,beta=wf.human.2011.en.stopwords$beta)
words.api%>%filter(words%in%names(tza.api[,tza.api[1,]>0]))
words.ori%>%filter(words%in%names(tza.api[,tza.api[1,]>0]))

# French API - stopwords
DFM.api.2011.fr.stopwords=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("french"))
DFM.api.2011.fr.stopwords=dfm_trim(DFM.api.2011.fr.stopwords,min_termfreq = 3)
wf.api.2011.fr.stopwords=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.api.2011.fr.stopwords=data.frame(est.api.fr=wf.api.2011.fr.stopwords$theta,
                                        lwr.api.fr=wf.api.2011.fr.stopwords$theta-1.96*wf.api.2011.fr.stopwords$se.theta,
                                        upr.api.fr=wf.api.2011.fr.stopwords$theta+1.96*wf.api.2011.fr.stopwords$se.theta,
                                        Name=wf.api.2011.fr.stopwords$docs,stringsAsFactors = F)
sum.wf.api.2011.fr.stopwords=left_join(x=sum.wf.api.2011.fr.stopwords,y=meta.2011.no.duplicates,by="Name")

# German API - stopwords
DFM.api.2011.de.stopwords=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("german"))
DFM.api.2011.de.stopwords=dfm_trim(DFM.api.2011.de.stopwords,min_termfreq = 3)
wf.api.2011.de.stopwords=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de.stopwords),dir=c(left.fix.stopwords,right.fix.stopwords),verbose=T)
sum.wf.api.2011.de.stopwords=data.frame(est.api.de=wf.api.2011.de.stopwords$theta,
                                        lwr.api.de=wf.api.2011.de.stopwords$theta-1.96*wf.api.2011.de.stopwords$se.theta,
                                        upr.api.de=wf.api.2011.de.stopwords$theta+1.96*wf.api.2011.de.stopwords$se.theta,
                                        Name=wf.api.2011.de.stopwords$docs,stringsAsFactors = F)
sum.wf.api.2011.de.stopwords=left_join(x=sum.wf.api.2011.de.stopwords,y=meta.2011.no.duplicates,by="Name")

# Combine
joined.wf.speaker.stopwords=left_join(sum.wf.human.2011.fr.stopwords,sum.wf.youtube.2011.fr.stopwords%>%
                                        select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.human.2011.de.stopwords%>%
                                        select(est.human.de,lwr.human.de,upr.human.de,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.youtube.2011.de.stopwords%>%
                                        select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.human.2011.en.stopwords%>%
                                        select(est.human.en,lwr.human.en,upr.human.en,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.youtube.2011.en.stopwords%>%
                                        select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.api.2011.en.stopwords%>%
                                        select(est.api.en,lwr.api.en,upr.api.en,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.api.2011.de.stopwords%>%
                                        select(est.api.de,lwr.api.de,upr.api.de,Number),by="Number")
joined.wf.speaker.stopwords=left_join(joined.wf.speaker.stopwords,sum.wf.api.2011.fr.stopwords%>%
                                        select(est.api.fr,lwr.api.fr,upr.api.fr,Number),by="Number")

#######################################################################
# Wordfish by speaker - Min 5 WORDS
#######################################################################

# English human - single
DFM.human.2011.en.single=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.en.single=dfm_trim(DFM.human.2011.en.single,min_termfreq = 5)
left.fix=which(row.names(DFM.human.2011.en.single)=="José Manuel Barroso")
right.fix=which(row.names(DFM.human.2011.en.single)=="Jan Zahradil")
wf.human.2011.en.single=austin::wordfish(quanteda::as.wfm(DFM.human.2011.en.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.human.2011.en.single=data.frame(est.human.en=wf.human.2011.en.single$theta,
                                          lwr.human.en=wf.human.2011.en.single$theta-1.96*wf.human.2011.en.single$se.theta,
                                          upr.human.en=wf.human.2011.en.single$theta+1.96*wf.human.2011.en.single$se.theta,
                                          Name=wf.human.2011.en.single$docs,stringsAsFactors = F)
sum.wf.human.2011.en.single=left_join(x=sum.wf.human.2011.en.single,y=meta.2011.no.duplicates)

# French human - single
DFM.human.2011.fr.single=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.fr.single=dfm_trim(DFM.human.2011.fr.single,min_termfreq = 5)
left.fix=which(row.names(DFM.human.2011.fr.single)=="José Manuel Barroso")
right.fix=which(row.names(DFM.human.2011.fr.single)=="Jan Zahradil")
wf.human.2011.fr.single=austin::wordfish(quanteda::as.wfm(DFM.human.2011.fr.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.human.2011.fr.single=data.frame(est.human.fr=wf.human.2011.fr.single$theta,
                                          lwr.human.fr=wf.human.2011.fr.single$theta-1.96*wf.human.2011.fr.single$se.theta,
                                          upr.human.fr=wf.human.2011.fr.single$theta+1.96*wf.human.2011.fr.single$se.theta,
                                          Name=wf.human.2011.fr.single$docs,stringsAsFactors = F)
sum.wf.human.2011.fr.single=left_join(x=sum.wf.human.2011.fr.single,y=meta.2011.no.duplicates)

# German human - single
DFM.human.2011.de.single=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.de.single=dfm_trim(DFM.human.2011.de.single,min_termfreq = 5)
wf.human.2011.de.single=austin::wordfish(quanteda::as.wfm(DFM.human.2011.de.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.human.2011.de.single=data.frame(est.human.de=wf.human.2011.de.single$theta,
                                          lwr.human.de=wf.human.2011.de.single$theta-1.96*wf.human.2011.de.single$se.theta,
                                          upr.human.de=wf.human.2011.de.single$theta+1.96*wf.human.2011.de.single$se.theta,
                                          Name=wf.human.2011.de.single$docs,stringsAsFactors = F)
sum.wf.human.2011.de.single=left_join(x=sum.wf.human.2011.de.single,y=meta.2011.no.duplicates)

# English Youtube - single
DFM.youtube.2011.en.single=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.en.single=dfm_trim(DFM.youtube.2011.en.single,min_termfreq = 5)
wf.youtube.2011.en.single=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.youtube.2011.en.single=data.frame(est.youtube.en=wf.youtube.2011.en.single$theta,
                                         lwr.youtube.en=wf.youtube.2011.en.single$theta-1.96*wf.youtube.2011.en.single$se.theta,
                                         upr.youtube.en=wf.youtube.2011.en.single$theta+1.96*wf.youtube.2011.en.single$se.theta,
                                         Name=wf.youtube.2011.en.single$docs,stringsAsFactors = F)
sum.wf.youtube.2011.en.single=left_join(x=sum.wf.youtube.2011.en.single,y=meta.2011.no.duplicates)

# French Youtube - single
DFM.youtube.2011.fr.single=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.fr.single=dfm_trim(DFM.youtube.2011.fr.single,min_termfreq = 5)
wf.youtube.2011.fr.single=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.fr.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.youtube.2011.fr.single=data.frame(est.youtube.fr=wf.youtube.2011.fr.single$theta,
                                         lwr.youtube.fr=wf.youtube.2011.fr.single$theta-1.96*wf.youtube.2011.fr.single$se.theta,
                                         upr.youtube.fr=wf.youtube.2011.fr.single$theta+1.96*wf.youtube.2011.fr.single$se.theta,
                                         Name=wf.youtube.2011.fr.single$docs,stringsAsFactors = F)
sum.wf.youtube.2011.fr.single=left_join(x=sum.wf.youtube.2011.fr.single,y=meta.2011.no.duplicates)

# German Youtube - single
DFM.youtube.2011.de.single=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.de.single=dfm_trim(DFM.youtube.2011.de.single,min_termfreq = 5)
wf.youtube.2011.de.single=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.de.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.youtube.2011.de.single=data.frame(est.youtube.de=wf.youtube.2011.de.single$theta,
                                         lwr.youtube.de=wf.youtube.2011.de.single$theta-1.96*wf.youtube.2011.de.single$se.theta,
                                         upr.youtube.de=wf.youtube.2011.de.single$theta+1.96*wf.youtube.2011.de.single$se.theta,
                                         Name=wf.youtube.2011.de.single$docs,stringsAsFactors = F)
sum.wf.youtube.2011.de.single=left_join(x=sum.wf.youtube.2011.de.single,y=meta.2011.no.duplicates)


# English API - single
DFM.api.2011.en.single=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.en.single=dfm_trim(DFM.api.2011.en.single,min_termfreq = 5)
wf.api.2011.en.single=austin::wordfish(quanteda::as.wfm(DFM.api.2011.en.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.api.2011.en.single=data.frame(est.api.en=wf.api.2011.en.single$theta,
                                     lwr.api.en=wf.api.2011.en.single$theta-1.96*wf.api.2011.en.single$se.theta,
                                     upr.api.en=wf.api.2011.en.single$theta+1.96*wf.api.2011.en.single$se.theta,
                                     Name=wf.api.2011.en.single$docs,stringsAsFactors = F)
sum.wf.api.2011.en.single=left_join(x=sum.wf.api.2011.en.single,y=meta.2011.no.duplicates)

# French API - single
DFM.api.2011.fr.single=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.fr.single=dfm_trim(DFM.api.2011.fr.single,min_termfreq = 5)
wf.api.2011.fr.single=austin::wordfish(quanteda::as.wfm(DFM.api.2011.fr.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.api.2011.fr.single=data.frame(est.api.fr=wf.api.2011.fr.single$theta,
                                     lwr.api.fr=wf.api.2011.fr.single$theta-1.96*wf.api.2011.fr.single$se.theta,
                                     upr.api.fr=wf.api.2011.fr.single$theta+1.96*wf.api.2011.fr.single$se.theta,
                                     Name=wf.api.2011.fr.single$docs,stringsAsFactors = F)
sum.wf.api.2011.fr.single=left_join(x=sum.wf.api.2011.fr.single,y=meta.2011.no.duplicates)

# German API - single
DFM.api.2011.de.single=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.de.single=dfm_trim(DFM.api.2011.de.single,min_termfreq = 5)
wf.api.2011.de.single=austin::wordfish(quanteda::as.wfm(DFM.api.2011.de.single),dir=c(left.fix,right.fix),verbose=T)
sum.wf.api.2011.de.single=data.frame(est.api.de=wf.api.2011.de.single$theta,
                                     lwr.api.de=wf.api.2011.de.single$theta-1.96*wf.api.2011.de.single$se.theta,
                                     upr.api.de=wf.api.2011.de.single$theta+1.96*wf.api.2011.de.single$se.theta,
                                     Name=wf.api.2011.de.single$docs,stringsAsFactors = F)
sum.wf.api.2011.de.single=left_join(x=sum.wf.api.2011.de.single,y=meta.2011.no.duplicates)

# Join all together - single
joined.wf.speaker.single=left_join(sum.wf.human.2011.fr.single,sum.wf.youtube.2011.fr.single%>%
                                     select(est.youtube.fr,lwr.youtube.fr,upr.youtube.fr,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.human.2011.de.single%>%
                                     select(est.human.de,lwr.human.de,upr.human.de,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.youtube.2011.de.single%>%
                                     select(est.youtube.de,lwr.youtube.de,upr.youtube.de,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.human.2011.en.single%>%
                                     select(est.human.en,lwr.human.en,upr.human.en,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.youtube.2011.en.single%>%
                                     select(est.youtube.en,lwr.youtube.en,upr.youtube.en,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.api.2011.en.single%>%
                                     select(est.api.en,lwr.api.en,upr.api.en,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.api.2011.de.single%>%
                                     select(est.api.de,lwr.api.de,upr.api.de,Name),by="Name")
joined.wf.speaker.single=left_join(joined.wf.speaker.single,sum.wf.api.2011.fr.single%>%
                                     select(est.api.fr,lwr.api.fr,upr.api.fr,Name),by="Name")

#######################################################################
# Sentiment by speaker - BASIC
#######################################################################
# English human - basic
DFM.human.2011.en.senti.basic=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.human.2011.en.senti.basic$Name=DFM.human.2011.en.senti.basic$document
DFM.human.2011.en.senti.basic=left_join(x=DFM.human.2011.en.senti.basic,y=meta.2011.no.duplicates)
DFM.human.2011.en.senti.basic$tokens=rowSums(dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.en.senti.basic=DFM.human.2011.en.senti.basic%>%
  mutate(tone.en.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# English Youtube - basic
DFM.youtube.2011.en.senti.basic=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.youtube.2011.en.senti.basic$Name=DFM.youtube.2011.en.senti.basic$document
DFM.youtube.2011.en.senti.basic=left_join(x=DFM.youtube.2011.en.senti.basic,y=meta.2011.no.duplicates)
DFM.youtube.2011.en.senti.basic$tokens=rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.en.senti.basic=DFM.youtube.2011.en.senti.basic%>%
  mutate(tone.en.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# English API - basic
DFM.api.2011.en.senti.basic=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.api.2011.en.senti.basic$Name=DFM.api.2011.en.senti.basic$document
DFM.api.2011.en.senti.basic=left_join(x=DFM.api.2011.en.senti.basic,y=meta.2011.no.duplicates)
DFM.api.2011.en.senti.basic$tokens=rowSums(dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.en.senti.basic=DFM.api.2011.en.senti.basic%>%
  mutate(tone.en.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# German human - basic
DFM.human.2011.de.senti.basic=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.human.2011.de.senti.basic$Name=DFM.human.2011.de.senti.basic$document
DFM.human.2011.de.senti.basic=left_join(x=DFM.human.2011.de.senti.basic,y=meta.2011.no.duplicates)
DFM.human.2011.de.senti.basic$tokens=rowSums(dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.de.senti.basic=DFM.human.2011.de.senti.basic%>%
  mutate(tone.de.human=(pos/tokens)-(neg/tokens),
         sentiment.de.human=log((pos+0.5)/(neg+0.5)))

# German Youtube - basic
DFM.youtube.2011.de.senti.basic=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
                                             convert(to="data.frame")
DFM.youtube.2011.de.senti.basic$Name=DFM.youtube.2011.de.senti.basic$document
DFM.youtube.2011.de.senti.basic=left_join(x=DFM.youtube.2011.de.senti.basic,y=meta.2011.no.duplicates)
DFM.youtube.2011.de.senti.basic$tokens=rowSums(dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.de.senti.basic=DFM.youtube.2011.de.senti.basic%>%
  mutate(tone.de.youtube=(pos/tokens)-(neg/tokens),
         sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))

# German API - basic
DFM.api.2011.de.senti.basic=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.api.2011.de.senti.basic$Name=DFM.api.2011.de.senti.basic$document
DFM.api.2011.de.senti.basic=left_join(x=DFM.api.2011.de.senti.basic,y=meta.2011.no.duplicates)
DFM.api.2011.de.senti.basic$tokens=rowSums(dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.de.senti.basic=DFM.api.2011.de.senti.basic%>%
  mutate(tone.de.api=(pos/tokens)-(neg/tokens),
         sentiment.de.api=log((pos+0.5)/(neg+0.5)))

# French human - basic
DFM.human.2011.fr.senti.basic=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.human.2011.fr.senti.basic$Name=DFM.human.2011.fr.senti.basic$document
DFM.human.2011.fr.senti.basic=left_join(x=DFM.human.2011.fr.senti.basic,y=meta.2011.no.duplicates)
DFM.human.2011.fr.senti.basic$tokens=rowSums(dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.fr.senti.basic=DFM.human.2011.fr.senti.basic%>%
  mutate(tone.fr.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# French Youtube - basic
DFM.youtube.2011.fr.senti.basic=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.fr.senti.basic$Name=DFM.youtube.2011.fr.senti.basic$document
DFM.youtube.2011.fr.senti.basic=left_join(x=DFM.youtube.2011.fr.senti.basic,y=meta.2011.no.duplicates)
DFM.youtube.2011.fr.senti.basic$tokens=rowSums(dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.fr.senti.basic=DFM.youtube.2011.fr.senti.basic%>%
  mutate(tone.fr.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# French API - basic
DFM.api.2011.fr.senti.basic=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.api.2011.fr.senti.basic$Name=DFM.api.2011.fr.senti.basic$document
DFM.api.2011.fr.senti.basic=left_join(x=DFM.api.2011.fr.senti.basic,y=meta.2011.no.duplicates)
DFM.api.2011.fr.senti.basic$tokens=rowSums(dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.fr.senti.basic=DFM.api.2011.fr.senti.basic%>%
  mutate(tone.fr.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# Combine
joined.senti.speaker.basic=left_join(DFM.human.2011.en.senti.basic,DFM.youtube.2011.en.senti.basic%>%
                                       select(tone.en.youtube,sentiment.en.youtube,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.api.2011.en.senti.basic%>%
                                       select(tone.en.api,sentiment.en.api,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.human.2011.fr.senti.basic%>%
                                       select(tone.fr.human,sentiment.fr.human,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.youtube.2011.fr.senti.basic%>%
                                       select(tone.fr.youtube,sentiment.fr.youtube,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.api.2011.fr.senti.basic%>%
                                       select(tone.fr.api,sentiment.fr.api,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.human.2011.de.senti.basic%>%
                                       select(tone.de.human,sentiment.de.human,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.youtube.2011.de.senti.basic%>%
                                       select(tone.de.youtube,sentiment.de.youtube,Number),by="Number")
joined.senti.speaker.basic=left_join(joined.senti.speaker.basic,DFM.api.2011.de.senti.basic%>%
                                       select(tone.de.api,sentiment.de.api,Number),by="Number")

#######################################################################
# Sentiment by Party - BASIC
#######################################################################

# English human - basic
DFM.human.2011.en.senti.basic.party=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.human.2011.en.senti.basic.party$Name=DFM.human.2011.en.senti.basic.party$document
DFM.human.2011.en.senti.basic.party$tokens=rowSums(dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.human.2011.en.senti.basic.party=DFM.human.2011.en.senti.basic.party%>%
  mutate(tone.en.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# English Youtube - basic
DFM.youtube.2011.en.senti.basic.party=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.youtube.2011.en.senti.basic.party$Name=DFM.youtube.2011.en.senti.basic.party$document
DFM.youtube.2011.en.senti.basic.party$tokens=rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.youtube.2011.en.senti.basic.party=DFM.youtube.2011.en.senti.basic.party%>%
  mutate(tone.en.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# English API - basic
DFM.api.2011.en.senti.basic.party=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.api.2011.en.senti.basic.party$Name=DFM.api.2011.en.senti.basic.party$document
DFM.api.2011.en.senti.basic.party$tokens=rowSums(dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.api.2011.en.senti.basic.party=DFM.api.2011.en.senti.basic.party%>%
  mutate(tone.en.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# German human - basic
DFM.human.2011.de.senti.basic.party=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.human.2011.de.senti.basic.party$Name=DFM.human.2011.de.senti.basic.party$document
DFM.human.2011.de.senti.basic.party$tokens=rowSums(dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.human.2011.de.senti.basic.party=DFM.human.2011.de.senti.basic.party%>%
  mutate(tone.de.human=(pos/tokens)-(neg/tokens),
         sentiment.de.human=log((pos+0.5)/(neg+0.5)))

# German Youtube - basic
DFM.youtube.2011.de.senti.basic.party=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.de.senti.basic.party$Name=DFM.youtube.2011.de.senti.basic.party$document
DFM.youtube.2011.de.senti.basic.party$tokens=rowSums(dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.youtube.2011.de.senti.basic.party=DFM.youtube.2011.de.senti.basic.party%>%
  mutate(tone.de.youtube=(pos/tokens)-(neg/tokens),
         sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))

# German API - basic
DFM.api.2011.de.senti.basic.party=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.api.2011.de.senti.basic.party$Name=DFM.api.2011.de.senti.basic.party$document
DFM.api.2011.de.senti.basic.party$tokens=rowSums(dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.api.2011.de.senti.basic.party=DFM.api.2011.de.senti.basic.party%>%
  mutate(tone.de.api=(pos/tokens)-(neg/tokens),
         sentiment.de.api=log((pos+0.5)/(neg+0.5)))

# French human - basic
DFM.human.2011.fr.senti.basic.party=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.human.2011.fr.senti.basic.party$Name=DFM.human.2011.fr.senti.basic.party$document
DFM.human.2011.fr.senti.basic.party$tokens=rowSums(dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.human.2011.fr.senti.basic.party=DFM.human.2011.fr.senti.basic.party%>%
  mutate(tone.fr.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# French Youtube - basic
DFM.youtube.2011.fr.senti.basic.party=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.fr.senti.basic.party$Name=DFM.youtube.2011.fr.senti.basic.party$document
DFM.youtube.2011.fr.senti.basic.party$tokens=rowSums(dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.youtube.2011.fr.senti.basic.party=DFM.youtube.2011.fr.senti.basic.party%>%
  mutate(tone.fr.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# French API - basic
DFM.api.2011.fr.senti.basic.party=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.api.2011.fr.senti.basic.party$Name=DFM.api.2011.fr.senti.basic.party$document
DFM.api.2011.fr.senti.basic.party$tokens=rowSums(dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Group"))
DFM.api.2011.fr.senti.basic.party=DFM.api.2011.fr.senti.basic.party%>%
  mutate(tone.fr.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# Combine
joined.senti.speaker.basic.party=left_join(DFM.human.2011.en.senti.basic.party,DFM.youtube.2011.en.senti.basic.party%>%
                                             select(tone.en.youtube,sentiment.en.youtube,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.api.2011.en.senti.basic.party%>%
                                             select(tone.en.api,sentiment.en.api,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.human.2011.fr.senti.basic.party%>%
                                             select(tone.fr.human,sentiment.fr.human,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.youtube.2011.fr.senti.basic.party%>%
                                             select(tone.fr.youtube,sentiment.fr.youtube,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.api.2011.fr.senti.basic.party%>%
                                             select(tone.fr.api,sentiment.fr.api,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.human.2011.de.senti.basic.party%>%
                                             select(tone.de.human,sentiment.de.human,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.youtube.2011.de.senti.basic.party%>%
                                             select(tone.de.youtube,sentiment.de.youtube,Name),by="Name")
joined.senti.speaker.basic.party=left_join(joined.senti.speaker.basic.party,DFM.api.2011.de.senti.basic.party%>%
                                             select(tone.de.api,sentiment.de.api,Name),by="Name")

#######################################################################
# Sentiment by speaker - STOPWORDS
#######################################################################

# English human - stopwords
DFM.human.2011.en.senti.stopwords=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("english"),dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.human.2011.en.senti.stopwords$Name=DFM.human.2011.en.senti.stopwords$document
DFM.human.2011.en.senti.stopwords=left_join(x=DFM.human.2011.en.senti.stopwords,y=meta.2011.no.duplicates)
DFM.human.2011.en.senti.stopwords$tokens=rowSums(dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.en.senti.stopwords=DFM.human.2011.en.senti.stopwords%>%
  mutate(tone.en.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# English Youtube - stopwords
DFM.youtube.2011.en.senti.stopwords=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("english"),dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.youtube.2011.en.senti.stopwords$Name=DFM.youtube.2011.en.senti.stopwords$document
DFM.youtube.2011.en.senti.stopwords=left_join(x=DFM.youtube.2011.en.senti.stopwords,y=meta.2011.no.duplicates)
DFM.youtube.2011.en.senti.stopwords$tokens=rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.en.senti.stopwords=DFM.youtube.2011.en.senti.stopwords%>%
  mutate(tone.en.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# English API - stopwords
DFM.api.2011.en.senti.stopwords=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("english"),dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.api.2011.en.senti.stopwords$Name=DFM.api.2011.en.senti.stopwords$document
DFM.api.2011.en.senti.stopwords=left_join(x=DFM.api.2011.en.senti.stopwords,y=meta.2011.no.duplicates)
DFM.api.2011.en.senti.stopwords$tokens=rowSums(dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.en.senti.stopwords=DFM.api.2011.en.senti.stopwords%>%
  mutate(tone.en.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# German human - stopwords
DFM.human.2011.de.senti.stopwords=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("german"),dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.human.2011.de.senti.stopwords$Name=DFM.human.2011.de.senti.stopwords$document
DFM.human.2011.de.senti.stopwords=left_join(x=DFM.human.2011.de.senti.stopwords,y=meta.2011.no.duplicates)
DFM.human.2011.de.senti.stopwords$tokens=rowSums(dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.de.senti.stopwords=DFM.human.2011.de.senti.stopwords%>%
  mutate(tone.de.human=(pos/tokens)-(neg/tokens),
         sentiment.de.human=log((pos+0.5)/(neg+0.5)))

# German Youtube - stopwords
DFM.youtube.2011.de.senti.stopwords=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("german"),dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.de.senti.stopwords$Name=DFM.youtube.2011.de.senti.stopwords$document
DFM.youtube.2011.de.senti.stopwords=left_join(x=DFM.youtube.2011.de.senti.stopwords,y=meta.2011.no.duplicates)
DFM.youtube.2011.de.senti.stopwords$tokens=rowSums(dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.de.senti.stopwords=DFM.youtube.2011.de.senti.stopwords%>%
  mutate(tone.de.youtube=(pos/tokens)-(neg/tokens),
         sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))

# German API - stopwords
DFM.api.2011.de.senti.stopwords=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("german"),dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.api.2011.de.senti.stopwords$Name=DFM.api.2011.de.senti.stopwords$document
DFM.api.2011.de.senti.stopwords=left_join(x=DFM.api.2011.de.senti.stopwords,y=meta.2011.no.duplicates)
DFM.api.2011.de.senti.stopwords$tokens=rowSums(dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.de.senti.stopwords=DFM.api.2011.de.senti.stopwords%>%
  mutate(tone.de.api=(pos/tokens)-(neg/tokens),
         sentiment.de.api=log((pos+0.5)/(neg+0.5)))

# French human - stopwords
DFM.human.2011.fr.senti.stopwords=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("french"),dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.human.2011.fr.senti.stopwords$Name=DFM.human.2011.fr.senti.stopwords$document
DFM.human.2011.fr.senti.stopwords=left_join(x=DFM.human.2011.fr.senti.stopwords,y=meta.2011.no.duplicates)
DFM.human.2011.fr.senti.stopwords$tokens=rowSums(dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.fr.senti.stopwords=DFM.human.2011.fr.senti.stopwords%>%
  mutate(tone.fr.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# French Youtube - stopwords
DFM.youtube.2011.fr.senti.stopwords=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("french"),dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.fr.senti.stopwords$Name=DFM.youtube.2011.fr.senti.stopwords$document
DFM.youtube.2011.fr.senti.stopwords=left_join(x=DFM.youtube.2011.fr.senti.stopwords,y=meta.2011.no.duplicates)
DFM.youtube.2011.fr.senti.stopwords$tokens=rowSums(dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.fr.senti.stopwords=DFM.youtube.2011.fr.senti.stopwords%>%
  mutate(tone.fr.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# French API - stopwords
DFM.api.2011.fr.senti.stopwords=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",remove=stopwords("french"),dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.api.2011.fr.senti.stopwords$Name=DFM.api.2011.fr.senti.stopwords$document
DFM.api.2011.fr.senti.stopwords=left_join(x=DFM.api.2011.fr.senti.stopwords,y=meta.2011.no.duplicates)
DFM.api.2011.fr.senti.stopwords$tokens=rowSums(dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.fr.senti.stopwords=DFM.api.2011.fr.senti.stopwords%>%
  mutate(tone.fr.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# Combine
joined.senti.speaker.stopwords=left_join(DFM.human.2011.en.senti.stopwords,DFM.youtube.2011.en.senti.stopwords%>%
                                           select(tone.en.youtube,sentiment.en.youtube,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.api.2011.en.senti.stopwords%>%
                                           select(tone.en.api,sentiment.en.api,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.human.2011.fr.senti.stopwords%>%
                                           select(tone.fr.human,sentiment.fr.human,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.youtube.2011.fr.senti.stopwords%>%
                                           select(tone.fr.youtube,sentiment.fr.youtube,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.api.2011.fr.senti.stopwords%>%
                                           select(tone.fr.api,sentiment.fr.api,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.human.2011.de.senti.stopwords%>%
                                           select(tone.de.human,sentiment.de.human,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.youtube.2011.de.senti.stopwords%>%
                                           select(tone.de.youtube,sentiment.de.youtube,Number),by="Number")
joined.senti.speaker.stopwords=left_join(joined.senti.speaker.stopwords,DFM.api.2011.de.senti.stopwords%>%
                                           select(tone.de.api,sentiment.de.api,Number),by="Number")

#######################################################################
# Sentiment by speaker - min 5 words
#######################################################################

# English human - min 5 words
DFM.human.2011.en.senti.single=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.en.senti.single=dfm_trim(DFM.human.2011.en.senti.single,min_termfreq = 5)
DFM.human.2011.en.senti.single=dfm(DFM.human.2011.en.senti.single,dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.human.2011.en.senti.single$Name=DFM.human.2011.en.senti.single$document
DFM.human.2011.en.senti.single=left_join(x=DFM.human.2011.en.senti.single,y=meta.2011.no.duplicates)
DFM.human.2011.en.senti.single$tokens=rowSums(dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.en.senti.single=DFM.human.2011.en.senti.single%>%
  mutate(tone.en.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# English Youtube - min 5 words
DFM.youtube.2011.en.senti.single=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.en.senti.single=dfm_trim(DFM.youtube.2011.en.senti.single,min_termfreq = 5)
DFM.youtube.2011.en.senti.single=dfm(DFM.youtube.2011.en.senti.single,dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.youtube.2011.en.senti.single$Name=DFM.youtube.2011.en.senti.single$document
DFM.youtube.2011.en.senti.single=left_join(x=DFM.youtube.2011.en.senti.single,y=meta.2011.no.duplicates)
DFM.youtube.2011.en.senti.single$tokens=rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.en.senti.single=DFM.youtube.2011.en.senti.single%>%
  mutate(tone.en.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# English API - min 5 words
DFM.api.2011.en.senti.single=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.en.senti.single=dfm_trim(DFM.api.2011.en.senti.single,min_termfreq = 5)
DFM.api.2011.en.senti.single=dfm(DFM.api.2011.en.senti.single,dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.api.2011.en.senti.single$Name=DFM.api.2011.en.senti.single$document
DFM.api.2011.en.senti.single=left_join(x=DFM.api.2011.en.senti.single,y=meta.2011.no.duplicates)
DFM.api.2011.en.senti.single$tokens=rowSums(dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.en.senti.single=DFM.api.2011.en.senti.single%>%
  mutate(tone.en.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# German human - min 5 words
DFM.human.2011.de.senti.single=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.de.senti.single=dfm_trim(DFM.human.2011.de.senti.single,min_termfreq = 5)
DFM.human.2011.de.senti.single=dfm(DFM.human.2011.de.senti.single,dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.human.2011.de.senti.single$Name=DFM.human.2011.de.senti.single$document
DFM.human.2011.de.senti.single=left_join(x=DFM.human.2011.de.senti.single,y=meta.2011.no.duplicates)
DFM.human.2011.de.senti.single$tokens=rowSums(dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.de.senti.single=DFM.human.2011.de.senti.single%>%
  mutate(tone.de.human=(pos/tokens)-(neg/tokens),
         sentiment.de.human=log((pos+0.5)/(neg+0.5)))

# German Youtube - min 5 words
DFM.youtube.2011.de.senti.single=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.de.senti.single=dfm_trim(DFM.youtube.2011.de.senti.single,min_termfreq = 5)
DFM.youtube.2011.de.senti.single=dfm(DFM.youtube.2011.de.senti.single,dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.de.senti.single$Name=DFM.youtube.2011.de.senti.single$document
DFM.youtube.2011.de.senti.single=left_join(x=DFM.youtube.2011.de.senti.single,y=meta.2011.no.duplicates)
DFM.youtube.2011.de.senti.single$tokens=rowSums(dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.de.senti.single=DFM.youtube.2011.de.senti.single%>%
  mutate(tone.de.youtube=(pos/tokens)-(neg/tokens),
         sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))

# German API - min 5 words
DFM.api.2011.de.senti.single=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.de.senti.single=dfm_trim(DFM.api.2011.de.senti.single,min_termfreq = 5)
DFM.api.2011.de.senti.single=dfm(DFM.api.2011.de.senti.single,dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.api.2011.de.senti.single$Name=DFM.api.2011.de.senti.single$document
DFM.api.2011.de.senti.single=left_join(x=DFM.api.2011.de.senti.single,y=meta.2011.no.duplicates)
DFM.api.2011.de.senti.single$tokens=rowSums(dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.de.senti.single=DFM.api.2011.de.senti.single%>%
  mutate(tone.de.api=(pos/tokens)-(neg/tokens),
         sentiment.de.api=log((pos+0.5)/(neg+0.5)))

# French human - min 5 words
DFM.human.2011.fr.senti.single=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.human.2011.fr.senti.single=dfm_trim(DFM.human.2011.fr.senti.single,min_termfreq = 5)
DFM.human.2011.fr.senti.single=dfm(DFM.human.2011.fr.senti.single,dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.human.2011.fr.senti.single$Name=DFM.human.2011.fr.senti.single$document
DFM.human.2011.fr.senti.single=left_join(x=DFM.human.2011.fr.senti.single,y=meta.2011.no.duplicates)
DFM.human.2011.fr.senti.single$tokens=rowSums(dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.fr.senti.single=DFM.human.2011.fr.senti.single%>%
  mutate(tone.fr.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# French Youtube - min 5 words
DFM.youtube.2011.fr.senti.single=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.youtube.2011.fr.senti.single=dfm_trim(DFM.youtube.2011.fr.senti.single,min_termfreq = 5)
DFM.youtube.2011.fr.senti.single=dfm(DFM.youtube.2011.fr.senti.single,dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.fr.senti.single$Name=DFM.youtube.2011.fr.senti.single$document
DFM.youtube.2011.fr.senti.single=left_join(x=DFM.youtube.2011.fr.senti.single,y=meta.2011.no.duplicates)
DFM.youtube.2011.fr.senti.single$tokens=rowSums(dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.fr.senti.single=DFM.youtube.2011.fr.senti.single%>%
  mutate(tone.fr.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# French API - min 5 words
DFM.api.2011.fr.senti.single=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name")
DFM.api.2011.fr.senti.single=dfm_trim(DFM.api.2011.fr.senti.single,min_termfreq = 5)
DFM.api.2011.fr.senti.single=dfm(DFM.api.2011.fr.senti.single,dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.api.2011.fr.senti.single$Name=DFM.api.2011.fr.senti.single$document
DFM.api.2011.fr.senti.single=left_join(x=DFM.api.2011.fr.senti.single,y=meta.2011.no.duplicates)
DFM.api.2011.fr.senti.single$tokens=rowSums(dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.fr.senti.single=DFM.api.2011.fr.senti.single%>%
  mutate(tone.fr.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# Combine
joined.senti.speaker.single=left_join(DFM.human.2011.en.senti.single,DFM.youtube.2011.en.senti.single%>%
                                        select(tone.en.youtube,sentiment.en.youtube,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.api.2011.en.senti.single%>%
                                        select(tone.en.api,sentiment.en.api,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.human.2011.fr.senti.single%>%
                                        select(tone.fr.human,sentiment.fr.human,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.youtube.2011.fr.senti.single%>%
                                        select(tone.fr.youtube,sentiment.fr.youtube,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.api.2011.fr.senti.single%>%
                                        select(tone.fr.api,sentiment.fr.api,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.human.2011.de.senti.single%>%
                                        select(tone.de.human,sentiment.de.human,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.youtube.2011.de.senti.single%>%
                                        select(tone.de.youtube,sentiment.de.youtube,Number),by="Number")
joined.senti.speaker.single=left_join(joined.senti.speaker.single,DFM.api.2011.de.senti.single%>%
                                        select(tone.de.api,sentiment.de.api,Number),by="Number")

#######################################################################
# Sentiment by Speaker - 100 TOKENS
#######################################################################

# English human - 100
DFM.human.2011.en.senti.100=dfm(human.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.human.2011.en.senti.100$Name=DFM.human.2011.en.senti.100$document
DFM.human.2011.en.senti.100=left_join(x=DFM.human.2011.en.senti.100,y=meta.2011.no.duplicates)
DFM.human.2011.en.senti.100$tokens=rowSums(dfm(human.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.en.senti.100=DFM.human.2011.en.senti.100%>%
  mutate(tone.en.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# English youtube - 100
DFM.youtube.2011.en.senti.100=dfm(youtube.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.youtube.2011.en.senti.100$Name=DFM.youtube.2011.en.senti.100$document
DFM.youtube.2011.en.senti.100=left_join(x=DFM.youtube.2011.en.senti.100,y=meta.2011.no.duplicates)
DFM.youtube.2011.en.senti.100$tokens=rowSums(dfm(youtube.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.en.senti.100=DFM.youtube.2011.en.senti.100%>%
  mutate(tone.en.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# English api - 100
DFM.api.2011.en.senti.100=dfm(api.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.api.2011.en.senti.100$Name=DFM.api.2011.en.senti.100$document
DFM.api.2011.en.senti.100=left_join(x=DFM.api.2011.en.senti.100,y=meta.2011.no.duplicates)
DFM.api.2011.en.senti.100$tokens=rowSums(dfm(api.2011.en.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.en.senti.100=DFM.api.2011.en.senti.100%>%
  mutate(tone.en.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# German human - 100
DFM.human.2011.de.senti.100=dfm(human.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.human.2011.de.senti.100$Name=DFM.human.2011.de.senti.100$document
DFM.human.2011.de.senti.100=left_join(x=DFM.human.2011.de.senti.100,y=meta.2011.no.duplicates)
DFM.human.2011.de.senti.100$tokens=rowSums(dfm(human.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.de.senti.100=DFM.human.2011.de.senti.100%>%
  mutate(tone.de.human=(pos/tokens)-(neg/tokens),
         sentiment.de.human=log((pos+0.5)/(neg+0.5)))

# German youtube - 100
DFM.youtube.2011.de.senti.100=dfm(youtube.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.de.senti.100$Name=DFM.youtube.2011.de.senti.100$document
DFM.youtube.2011.de.senti.100=left_join(x=DFM.youtube.2011.de.senti.100,y=meta.2011.no.duplicates)
DFM.youtube.2011.de.senti.100$tokens=rowSums(dfm(youtube.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.de.senti.100=DFM.youtube.2011.de.senti.100%>%
  mutate(tone.de.youtube=(pos/tokens)-(neg/tokens),
         sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))

# German api - 100
DFM.api.2011.de.senti.100=dfm(api.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.api.2011.de.senti.100$Name=DFM.api.2011.de.senti.100$document
DFM.api.2011.de.senti.100=left_join(x=DFM.api.2011.de.senti.100,y=meta.2011.no.duplicates)
DFM.api.2011.de.senti.100$tokens=rowSums(dfm(api.2011.de.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.de.senti.100=DFM.api.2011.de.senti.100%>%
  mutate(tone.de.api=(pos/tokens)-(neg/tokens),
         sentiment.de.api=log((pos+0.5)/(neg+0.5)))

# French human - 100
DFM.human.2011.fr.senti.100=dfm(human.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.human.2011.fr.senti.100$Name=DFM.human.2011.fr.senti.100$document
DFM.human.2011.fr.senti.100=left_join(x=DFM.human.2011.fr.senti.100,y=meta.2011.no.duplicates)
DFM.human.2011.fr.senti.100$tokens=rowSums(dfm(human.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.fr.senti.100=DFM.human.2011.fr.senti.100%>%
  mutate(tone.fr.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# French youtube - 100
DFM.youtube.2011.fr.senti.100=dfm(youtube.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.fr.senti.100$Name=DFM.youtube.2011.fr.senti.100$document
DFM.youtube.2011.fr.senti.100=left_join(x=DFM.youtube.2011.fr.senti.100,y=meta.2011.no.duplicates)
DFM.youtube.2011.fr.senti.100$tokens=rowSums(dfm(youtube.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.fr.senti.100=DFM.youtube.2011.fr.senti.100%>%
  mutate(tone.fr.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# French api - 100
DFM.api.2011.fr.senti.100=dfm(api.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.api.2011.fr.senti.100$Name=DFM.api.2011.fr.senti.100$document
DFM.api.2011.fr.senti.100=left_join(x=DFM.api.2011.fr.senti.100,y=meta.2011.no.duplicates)
DFM.api.2011.fr.senti.100$tokens=rowSums(dfm(api.2011.fr.100,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.fr.senti.100=DFM.api.2011.fr.senti.100%>%
  mutate(tone.fr.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# Combine English
joined.senti.speaker.100.en=left_join(DFM.human.2011.en.senti.100,DFM.youtube.2011.en.senti.100%>%
                                        select(tone.en.youtube,sentiment.en.youtube,Number),by="Number")
joined.senti.speaker.100.en=left_join(joined.senti.speaker.100.en,DFM.api.2011.en.senti.100%>%
                                        select(tone.en.api,sentiment.en.api,Number),by="Number")

# Combine French
joined.senti.speaker.100.fr=left_join(DFM.human.2011.fr.senti.100,DFM.youtube.2011.fr.senti.100%>%
                                        select(tone.fr.youtube,sentiment.fr.youtube,Number),by="Number")
joined.senti.speaker.100.fr=left_join(joined.senti.speaker.100.fr,DFM.api.2011.fr.senti.100%>%
                                        select(tone.fr.api,sentiment.fr.api,Number),by="Number")

# Combine German
joined.senti.speaker.100.de=left_join(DFM.human.2011.de.senti.100,DFM.youtube.2011.de.senti.100%>%
                                        select(tone.de.youtube,sentiment.de.youtube,Number),by="Number")
joined.senti.speaker.100.de=left_join(joined.senti.speaker.100.de,DFM.api.2011.de.senti.100%>%
                                        select(tone.de.api,sentiment.de.api,Number),by="Number")

#######################################################################
# Sentiment by Speaker - 200 TOKENS
#######################################################################

# English human - 200
DFM.human.2011.en.senti.200=dfm(human.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.human.2011.en.senti.200$Name=DFM.human.2011.en.senti.200$document
DFM.human.2011.en.senti.200=left_join(x=DFM.human.2011.en.senti.200,y=meta.2011.no.duplicates)
DFM.human.2011.en.senti.200$tokens=rowSums(dfm(human.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.en.senti.200=DFM.human.2011.en.senti.200%>%
  mutate(tone.en.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# English youtube - 200
DFM.youtube.2011.en.senti.200=dfm(youtube.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.youtube.2011.en.senti.200$Name=DFM.youtube.2011.en.senti.200$document
DFM.youtube.2011.en.senti.200=left_join(x=DFM.youtube.2011.en.senti.200,y=meta.2011.no.duplicates)
DFM.youtube.2011.en.senti.200$tokens=rowSums(dfm(youtube.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.en.senti.200=DFM.youtube.2011.en.senti.200%>%
  mutate(tone.en.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# English api - 200
DFM.api.2011.en.senti.200=dfm(api.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_en)%>%
  convert(to="data.frame")
DFM.api.2011.en.senti.200$Name=DFM.api.2011.en.senti.200$document
DFM.api.2011.en.senti.200=left_join(x=DFM.api.2011.en.senti.200,y=meta.2011.no.duplicates)
DFM.api.2011.en.senti.200$tokens=rowSums(dfm(api.2011.en.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.en.senti.200=DFM.api.2011.en.senti.200%>%
  mutate(tone.en.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.en.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# German human - 200
DFM.human.2011.de.senti.200=dfm(human.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.human.2011.de.senti.200$Name=DFM.human.2011.de.senti.200$document
DFM.human.2011.de.senti.200=left_join(x=DFM.human.2011.de.senti.200,y=meta.2011.no.duplicates)
DFM.human.2011.de.senti.200$tokens=rowSums(dfm(human.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.de.senti.200=DFM.human.2011.de.senti.200%>%
  mutate(tone.de.human=(pos/tokens)-(neg/tokens),
         sentiment.de.human=log((pos+0.5)/(neg+0.5)))

# German youtube - 200
DFM.youtube.2011.de.senti.200=dfm(youtube.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.de.senti.200$Name=DFM.youtube.2011.de.senti.200$document
DFM.youtube.2011.de.senti.200=left_join(x=DFM.youtube.2011.de.senti.200,y=meta.2011.no.duplicates)
DFM.youtube.2011.de.senti.200$tokens=rowSums(dfm(youtube.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.de.senti.200=DFM.youtube.2011.de.senti.200%>%
  mutate(tone.de.youtube=(pos/tokens)-(neg/tokens),
         sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))

# German api - 200
DFM.api.2011.de.senti.200=dfm(api.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_de_e)%>%
  convert(to="data.frame")
DFM.api.2011.de.senti.200$Name=DFM.api.2011.de.senti.200$document
DFM.api.2011.de.senti.200=left_join(x=DFM.api.2011.de.senti.200,y=meta.2011.no.duplicates)
DFM.api.2011.de.senti.200$tokens=rowSums(dfm(api.2011.de.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.de.senti.200=DFM.api.2011.de.senti.200%>%
  mutate(tone.de.api=(pos/tokens)-(neg/tokens),
         sentiment.de.api=log((pos+0.5)/(neg+0.5)))

# French human - 200
DFM.human.2011.fr.senti.200=dfm(human.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.human.2011.fr.senti.200$Name=DFM.human.2011.fr.senti.200$document
DFM.human.2011.fr.senti.200=left_join(x=DFM.human.2011.fr.senti.200,y=meta.2011.no.duplicates)
DFM.human.2011.fr.senti.200$tokens=rowSums(dfm(human.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.human.2011.fr.senti.200=DFM.human.2011.fr.senti.200%>%
  mutate(tone.fr.human=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.human=log((pos.pos+0.5)/(neg.neg+0.5)))

# French youtube - 200
DFM.youtube.2011.fr.senti.200=dfm(youtube.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.youtube.2011.fr.senti.200$Name=DFM.youtube.2011.fr.senti.200$document
DFM.youtube.2011.fr.senti.200=left_join(x=DFM.youtube.2011.fr.senti.200,y=meta.2011.no.duplicates)
DFM.youtube.2011.fr.senti.200$tokens=rowSums(dfm(youtube.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.youtube.2011.fr.senti.200=DFM.youtube.2011.fr.senti.200%>%
  mutate(tone.fr.youtube=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.youtube=log((pos.pos+0.5)/(neg.neg+0.5)))

# French api - 200
DFM.api.2011.fr.senti.200=dfm(api.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = extendeddict_fr_e)%>%
  convert(to="data.frame")
DFM.api.2011.fr.senti.200$Name=DFM.api.2011.fr.senti.200$document
DFM.api.2011.fr.senti.200=left_join(x=DFM.api.2011.fr.senti.200,y=meta.2011.no.duplicates)
DFM.api.2011.fr.senti.200$tokens=rowSums(dfm(api.2011.fr.200,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
DFM.api.2011.fr.senti.200=DFM.api.2011.fr.senti.200%>%
  mutate(tone.fr.api=(pos.pos/tokens)-(neg.neg/tokens),
         sentiment.fr.api=log((pos.pos+0.5)/(neg.neg+0.5)))

# Combine English
joined.senti.speaker.200.en=left_join(DFM.human.2011.en.senti.200,DFM.youtube.2011.en.senti.200%>%
                                        select(tone.en.youtube,sentiment.en.youtube,Number),by="Number")
joined.senti.speaker.200.en=left_join(joined.senti.speaker.200.en,DFM.api.2011.en.senti.200%>%
                                        select(tone.en.api,sentiment.en.api,Number),by="Number")
# Combine French
joined.senti.speaker.200.fr=left_join(DFM.human.2011.fr.senti.200,DFM.youtube.2011.fr.senti.200%>%
                                        select(tone.fr.youtube,sentiment.fr.youtube,Number),by="Number")
joined.senti.speaker.200.fr=left_join(joined.senti.speaker.200.fr,DFM.api.2011.fr.senti.200%>%
                                        select(tone.fr.api,sentiment.fr.api,Number),by="Number")
# Combine German
joined.senti.speaker.200.de=left_join(DFM.human.2011.de.senti.200,DFM.youtube.2011.de.senti.200%>%
                                        select(tone.de.youtube,sentiment.de.youtube,Number),by="Number")
joined.senti.speaker.200.de=left_join(joined.senti.speaker.200.de,DFM.api.2011.de.senti.200%>%
                                        select(tone.de.api,sentiment.de.api,Number),by="Number")

save(joined.wf.speaker.basic,joined.wf.speaker.stemming,joined.wf.party.basic,
     joined.wf.100.fr,joined.wf.100.en,joined.wf.100.de,
     joined.wf.200.fr,joined.wf.200.en,joined.wf.200.de,
     joined.wf.speaker.stopwords,joined.wf.speaker.single,
     joined.senti.speaker.basic,joined.senti.speaker.basic.party,
     joined.senti.speaker.stopwords,joined.senti.speaker.single,
     joined.senti.speaker.100.en,joined.senti.speaker.200.en,
     joined.senti.speaker.100.de,joined.senti.speaker.200.de,
     joined.senti.speaker.100.fr,joined.senti.speaker.200.fr,
     file="generated_data/SOTU_wordfish_sentiment.RData")

#######################################################################
# Plots
#######################################################################

# Load the following file if you want to skip calculating the previous data
#load(file="generated_data/SOTU_wordfish_sentiment")

table.cors.basic=data.frame(round(cor(joined.wf.speaker.basic%>%select(grep("est",names(joined.wf.speaker.basic)))),2),stringsAsFactors = F)
names(table.cors.basic)=gsub("est.human.","human ",names(table.cors.basic))
table.cors.basic$to_what=row.names(table.cors.basic)
table.cors.basic=table.cors.basic%>%select(grep("human",names(table.cors.basic)),to_what)%>%
  filter(grepl("youtube",table.cors.basic$to_what)|grepl("api",table.cors.basic$to_what))
table.cors.basic

table.cors.stopwords=data.frame(round(cor(joined.wf.speaker.stopwords%>%select(grep("est",names(joined.wf.speaker.stopwords)))),2),stringsAsFactors = F)
names(table.cors.stopwords)=gsub("est.human.","human ",names(table.cors.stopwords))
table.cors.stopwords$to_what=row.names(table.cors.stopwords)
table.cors.stopwords=table.cors.stopwords%>%select(grep("human",names(table.cors.stopwords)),to_what)%>%
  filter(grepl("youtube",table.cors.stopwords$to_what)|grepl("api",table.cors.stopwords$to_what)|grepl("protocol",table.cors.stopwords$to_what))
table.cors.stopwords

table.cors.stemming=data.frame(round(cor(joined.wf.speaker.stemming%>%select(grep("est",names(joined.wf.speaker.stemming)))),2),stringsAsFactors = F)
names(table.cors.stemming)=gsub("est.human.","human ",names(table.cors.stemming))
table.cors.stemming$to_what=row.names(table.cors.stemming)
table.cors.stemming=table.cors.stemming%>%select(grep("human",names(table.cors.stemming)),to_what)%>%
  filter(grepl("youtube",table.cors.stemming$to_what)|grepl("api",table.cors.stemming$to_what)|grepl("protocol",table.cors.stemming$to_what))
table.cors.stemming

table.cors.single=data.frame(round(cor(joined.wf.speaker.single%>%select(grep("est",names(joined.wf.speaker.single)))),2),stringsAsFactors = F)
names(table.cors.single)=gsub("est.human.","human ",names(table.cors.single))
table.cors.single$to_what=row.names(table.cors.single)
table.cors.single=table.cors.single%>%select(grep("human",names(table.cors.single)),to_what)%>%
  filter(grepl("youtube",table.cors.single$to_what)|grepl("api",table.cors.single$to_what)|grepl("protocol",table.cors.single$to_what))
table.cors.single

table.cors.100.en=data.frame(round(cor(joined.wf.100.en%>%select(grep("est",names(joined.wf.100.en)))),2),stringsAsFactors = F)
names(table.cors.100.en)=gsub("est.human.","human ",names(table.cors.100.en))
table.cors.100.en$to_what=row.names(table.cors.100.en)
table.cors.100.en=table.cors.100.en%>%select(grep("human",names(table.cors.100.en)),to_what)%>%
  filter(grepl("youtube",table.cors.100.en$to_what)|grepl("api",table.cors.100.en$to_what)|grepl("protocol",table.cors.100.en$to_what))
table.cors.100.en

table.cors.100.fr=data.frame(round(cor(joined.wf.100.fr%>%select(grep("est",names(joined.wf.100.fr)))),2),stringsAsFactors = F)
names(table.cors.100.fr)=gsub("est.human.","human ",names(table.cors.100.fr))
table.cors.100.fr$to_what=row.names(table.cors.100.fr)
table.cors.100.fr=table.cors.100.fr%>%select(grep("human",names(table.cors.100.fr)),to_what)%>%
  filter(grepl("youtube",table.cors.100.fr$to_what)|grepl("api",table.cors.100.fr$to_what))
table.cors.100.fr

table.cors.100.de=data.frame(round(cor(joined.wf.100.de%>%select(grep("est",names(joined.wf.100.de)))),2),stringsAsFactors = F)
names(table.cors.100.de)=gsub("est.human.","human ",names(table.cors.100.de))
table.cors.100.de$to_what=row.names(table.cors.100.de)
table.cors.100.de=table.cors.100.de%>%select(grep("human",names(table.cors.100.de)),to_what)%>%
  filter(grepl("youtube",table.cors.100.de$to_what)|grepl("api",table.cors.100.de$to_what)|grepl("protocol",table.cors.100.de$to_what))
table.cors.100.de

table.cors.200.en=data.frame(round(cor(joined.wf.200.en%>%select(grep("est",names(joined.wf.200.en)))),2),stringsAsFactors = F)
names(table.cors.200.en)=gsub("est.human.","human ",names(table.cors.200.en))
table.cors.200.en$to_what=row.names(table.cors.200.en)
table.cors.200.en=table.cors.200.en%>%select(grep("human",names(table.cors.200.en)),to_what)%>%
  filter(grepl("youtube",table.cors.200.en$to_what)|grepl("api",table.cors.200.en$to_what)|grepl("protocol",table.cors.200.en$to_what))
table.cors.200.en

table.cors.200.fr=data.frame(round(cor(joined.wf.200.fr%>%select(grep("est",names(joined.wf.200.fr)))),2),stringsAsFactors = F)
names(table.cors.200.fr)=gsub("est.human.","human ",names(table.cors.200.fr))
table.cors.200.fr$to_what=row.names(table.cors.200.fr)
table.cors.200.fr=table.cors.200.fr%>%select(grep("human",names(table.cors.200.fr)),to_what)%>%
  filter(grepl("youtube",table.cors.200.fr$to_what)|grepl("api",table.cors.200.fr$to_what))
table.cors.200.fr

table.cors.200.de=data.frame(round(cor(joined.wf.200.de%>%select(grep("est",names(joined.wf.200.de)))),2),stringsAsFactors = F)
names(table.cors.200.de)=gsub("est.human.","human ",names(table.cors.200.de))
table.cors.200.de$to_what=row.names(table.cors.200.de)
table.cors.200.de=table.cors.200.de%>%select(grep("human",names(table.cors.200.de)),to_what)%>%
  filter(grepl("youtube",table.cors.200.de$to_what)|grepl("api",table.cors.200.de$to_what))
table.cors.200.de

allcors=data.frame(language=rep(c(rep("EN",6),rep("DE",6),rep("FR",6)),2),
                   preprocessing=rep(c(rep(c("Baseline","Stopwords","Stemming","Min Count = 5","Min 100 words","Min 200 words"),3)),2),
                   type=c(rep("Youtube",18),rep("API",18)),stringsAsFactors = F)
allcors=allcors%>%
  mutate(
    cor=case_when(
      language=="EN"&preprocessing=="Baseline"&type=="Youtube" ~ as.numeric(table.cors.basic%>%filter(to_what=="est.youtube.en")%>%select('human en')),
      language=="EN"&preprocessing=="Stopwords"&type=="Youtube" ~ as.numeric(table.cors.stopwords%>%filter(to_what=="est.youtube.en")%>%select('human en')),
      language=="EN"&preprocessing=="Stemming"&type=="Youtube" ~ as.numeric(table.cors.stemming%>%filter(to_what=="est.youtube.en")%>%select('human en')),
      language=="EN"&preprocessing=="Min Count = 5"&type=="Youtube" ~ as.numeric(table.cors.single%>%filter(to_what=="est.youtube.en")%>%select('human en')),
      language=="EN"&preprocessing=="Min 100 words"&type=="Youtube" ~ as.numeric(table.cors.100.en%>%filter(to_what=="est.youtube.en")%>%select('human en')),
      language=="EN"&preprocessing=="Min 200 words"&type=="Youtube" ~ as.numeric(table.cors.200.en%>%filter(to_what=="est.youtube.en")%>%select('human en')),
      language=="DE"&preprocessing=="Baseline"&type=="Youtube" ~ as.numeric(table.cors.basic%>%filter(to_what=="est.youtube.de")%>%select('human de')),
      language=="DE"&preprocessing=="Stopwords"&type=="Youtube" ~ as.numeric(table.cors.stopwords%>%filter(to_what=="est.youtube.de")%>%select('human de')),
      language=="DE"&preprocessing=="Stemming"&type=="Youtube" ~ as.numeric(table.cors.stemming%>%filter(to_what=="est.youtube.de")%>%select('human de')),
      language=="DE"&preprocessing=="Min Count = 5"&type=="Youtube" ~ as.numeric(table.cors.single%>%filter(to_what=="est.youtube.de")%>%select('human de')),
      language=="DE"&preprocessing=="Min 100 words"&type=="Youtube" ~ as.numeric(table.cors.100.de%>%filter(to_what=="est.youtube.de")%>%select('human de')),
      language=="DE"&preprocessing=="Min 200 words"&type=="Youtube" ~ as.numeric(table.cors.200.de%>%filter(to_what=="est.youtube.de")%>%select('human de')),
      language=="FR"&preprocessing=="Baseline"&type=="Youtube" ~ as.numeric(table.cors.basic%>%filter(to_what=="est.youtube.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Stopwords"&type=="Youtube" ~ as.numeric(table.cors.stopwords%>%filter(to_what=="est.youtube.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Stemming"&type=="Youtube" ~ as.numeric(table.cors.stemming%>%filter(to_what=="est.youtube.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Min Count = 5"&type=="Youtube" ~ as.numeric(table.cors.single%>%filter(to_what=="est.youtube.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Min 100 words"&type=="Youtube" ~ as.numeric(table.cors.100.fr%>%filter(to_what=="est.youtube.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Min 200 words"&type=="Youtube" ~ as.numeric(table.cors.200.fr%>%filter(to_what=="est.youtube.fr")%>%select('human fr')),
      language=="EN"&preprocessing=="Baseline"&type=="API" ~ as.numeric(table.cors.basic%>%filter(to_what=="est.api.en")%>%select('human en')),
      language=="EN"&preprocessing=="Stopwords"&type=="API" ~ as.numeric(table.cors.stopwords%>%filter(to_what=="est.api.en")%>%select('human en')),
      language=="EN"&preprocessing=="Stemming"&type=="API" ~ as.numeric(table.cors.stemming%>%filter(to_what=="est.api.en")%>%select('human en')),
      language=="EN"&preprocessing=="Min Count = 5"&type=="API" ~ as.numeric(table.cors.single%>%filter(to_what=="est.api.en")%>%select('human en')),
      language=="EN"&preprocessing=="Min 100 words"&type=="API" ~ as.numeric(table.cors.100.en%>%filter(to_what=="est.api.en")%>%select('human en')),
      language=="EN"&preprocessing=="Min 200 words"&type=="API" ~ as.numeric(table.cors.200.en%>%filter(to_what=="est.api.en")%>%select('human en')),
      language=="DE"&preprocessing=="Baseline"&type=="API" ~ as.numeric(table.cors.basic%>%filter(to_what=="est.api.de")%>%select('human de')),
      language=="DE"&preprocessing=="Stopwords"&type=="API" ~ as.numeric(table.cors.stopwords%>%filter(to_what=="est.api.de")%>%select('human de')),
      language=="DE"&preprocessing=="Stemming"&type=="API" ~ as.numeric(table.cors.stemming%>%filter(to_what=="est.api.de")%>%select('human de')),
      language=="DE"&preprocessing=="Min Count = 5"&type=="API" ~ as.numeric(table.cors.single%>%filter(to_what=="est.api.de")%>%select('human de')),
      language=="DE"&preprocessing=="Min 100 words"&type=="API" ~ as.numeric(table.cors.100.de%>%filter(to_what=="est.api.de")%>%select('human de')),
      language=="DE"&preprocessing=="Min 200 words"&type=="API" ~ as.numeric(table.cors.200.de%>%filter(to_what=="est.api.de")%>%select('human de')),
      language=="FR"&preprocessing=="Baseline"&type=="API" ~ as.numeric(table.cors.basic%>%filter(to_what=="est.api.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Stopwords"&type=="API" ~ as.numeric(table.cors.stopwords%>%filter(to_what=="est.api.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Stemming"&type=="API" ~ as.numeric(table.cors.stemming%>%filter(to_what=="est.api.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Min Count = 5"&type=="API" ~ as.numeric(table.cors.single%>%filter(to_what=="est.api.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Min 100 words"&type=="API" ~ as.numeric(table.cors.100.fr%>%filter(to_what=="est.api.fr")%>%select('human fr')),
      language=="FR"&preprocessing=="Min 200 words"&type=="API" ~ as.numeric(table.cors.200.fr%>%filter(to_what=="est.api.fr")%>%select('human fr')),
      TRUE ~ 0
    )
  )
allcors$preprocessing_language=paste(allcors$preprocessing,allcors$language)
allcors$preprocessing_language=factor(allcors$preprocessing_language, levels=unique(allcors$preprocessing_language))
allcors$preprocessing_language=factor(allcors$preprocessing_language, levels=rev(levels(allcors$preprocessing_language)))

allcors$type=as.factor(allcors$type)
allcors$language=as.factor(allcors$language)
allcors$language=relevel(allcors$language,"EN")

##########################################################
# Sentiment
##########################################################

table.cors.senti.basic=data.frame(round(cor(joined.senti.speaker.basic%>%select(grep("sentiment",names(joined.senti.speaker.basic)))),2),stringsAsFactors = F)
table.cors.senti.basic$to_what=row.names(table.cors.senti.basic)
table.cors.senti.basic=table.cors.senti.basic%>%select(grep("human",names(table.cors.senti.basic)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.basic$to_what)|grepl("api",table.cors.senti.basic$to_what)|grepl("protocol",table.cors.senti.basic$to_what))
table.cors.senti.basic

table.cors.senti.stopwords=data.frame(round(cor(joined.senti.speaker.stopwords%>%select(grep("sentiment",names(joined.senti.speaker.stopwords)))),2),stringsAsFactors = F)
table.cors.senti.stopwords$to_what=row.names(table.cors.senti.stopwords)
table.cors.senti.stopwords=table.cors.senti.stopwords%>%select(grep("human",names(table.cors.senti.stopwords)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.stopwords$to_what)|grepl("api",table.cors.senti.stopwords$to_what)|grepl("protocol",table.cors.senti.stopwords$to_what))
table.cors.senti.stopwords

table.cors.senti.single=data.frame(round(cor(joined.senti.speaker.single%>%select(grep("sentiment",names(joined.senti.speaker.single)))),2),stringsAsFactors = F)
table.cors.senti.single$to_what=row.names(table.cors.senti.single)
table.cors.senti.single=table.cors.senti.single%>%select(grep("human",names(table.cors.senti.single)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.single$to_what)|grepl("api",table.cors.senti.single$to_what)|grepl("protocol",table.cors.senti.single$to_what))
table.cors.senti.single

table.cors.senti.100.en=data.frame(round(cor(joined.senti.speaker.100.en%>%select(grep("sentiment",names(joined.senti.speaker.100.en)))),2),stringsAsFactors = F)
table.cors.senti.100.en$to_what=row.names(table.cors.senti.100.en)
table.cors.senti.100.en=table.cors.senti.100.en%>%select(grep("human",names(table.cors.senti.100.en)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.100.en$to_what)|grepl("api",table.cors.senti.100.en$to_what)|grepl("protocol",table.cors.senti.100.en$to_what))
table.cors.senti.100.en

table.cors.senti.100.fr=data.frame(round(cor(joined.senti.speaker.100.fr%>%select(grep("sentiment",names(joined.senti.speaker.100.fr)))),2),stringsAsFactors = F)
table.cors.senti.100.fr$to_what=row.names(table.cors.senti.100.fr)
table.cors.senti.100.fr=table.cors.senti.100.fr%>%select(grep("human",names(table.cors.senti.100.fr)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.100.fr$to_what)|grepl("api",table.cors.senti.100.fr$to_what)|grepl("protocol",table.cors.senti.100.fr$to_what))
table.cors.senti.100.fr

table.cors.senti.100.de=data.frame(round(cor(joined.senti.speaker.100.de%>%select(grep("sentiment",names(joined.senti.speaker.100.de)))),2),stringsAsFactors = F)
table.cors.senti.100.de$to_what=row.names(table.cors.senti.100.de)
table.cors.senti.100.de=table.cors.senti.100.de%>%select(grep("human",names(table.cors.senti.100.de)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.100.de$to_what)|grepl("api",table.cors.senti.100.de$to_what)|grepl("protocol",table.cors.senti.100.de$to_what))
table.cors.senti.100.de

table.cors.senti.200.en=data.frame(round(cor(joined.senti.speaker.200.en%>%select(grep("sentiment",names(joined.senti.speaker.200.en)))),2),stringsAsFactors = F)
table.cors.senti.200.en$to_what=row.names(table.cors.senti.200.en)
table.cors.senti.200.en=table.cors.senti.200.en%>%select(grep("human",names(table.cors.senti.200.en)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.200.en$to_what)|grepl("api",table.cors.senti.200.en$to_what)|grepl("protocol",table.cors.senti.200.en$to_what))
table.cors.senti.200.en

table.cors.senti.200.fr=data.frame(round(cor(joined.senti.speaker.200.fr%>%select(grep("sentiment",names(joined.senti.speaker.200.fr)))),2),stringsAsFactors = F)
table.cors.senti.200.fr$to_what=row.names(table.cors.senti.200.fr)
table.cors.senti.200.fr=table.cors.senti.200.fr%>%select(grep("human",names(table.cors.senti.200.fr)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.200.fr$to_what)|grepl("api",table.cors.senti.200.fr$to_what)|grepl("protocol",table.cors.senti.200.fr$to_what))
table.cors.senti.200.fr

table.cors.senti.200.de=data.frame(round(cor(joined.senti.speaker.200.de%>%select(grep("sentiment",names(joined.senti.speaker.200.de)))),2),stringsAsFactors = F)
table.cors.senti.200.de$to_what=row.names(table.cors.senti.200.de)
table.cors.senti.200.de=table.cors.senti.200.de%>%select(grep("human",names(table.cors.senti.200.de)),to_what)%>%
  filter(grepl("youtube",table.cors.senti.200.de$to_what)|grepl("api",table.cors.senti.200.de$to_what)|grepl("protocol",table.cors.senti.200.de$to_what))
table.cors.senti.200.de

allcors_senti=data.frame(language=rep(c(rep("EN",5),rep("DE",5),rep("FR",5)),2),
                         preprocessing=rep(c(rep(c("Baseline","Stopwords","Min count = 5","Min 100 words","Min 200 words"),3)),2),
                         type=c(rep("Youtube",15),rep("API",15)),stringsAsFactors = F)

allcors_senti=allcors_senti%>%
  mutate(
    cor=case_when(
      language=="EN"&preprocessing=="Baseline"&type=="Youtube" ~ as.numeric(table.cors.senti.basic%>%filter(to_what=="sentiment.en.youtube")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Stopwords"&type=="Youtube" ~ as.numeric(table.cors.senti.stopwords%>%filter(to_what=="sentiment.en.youtube")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Min count = 5"&type=="Youtube" ~ as.numeric(table.cors.senti.single%>%filter(to_what=="sentiment.en.youtube")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Min 100 words"&type=="Youtube" ~ as.numeric(table.cors.senti.100.en%>%filter(to_what=="sentiment.en.youtube")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Min 200 words"&type=="Youtube" ~ as.numeric(table.cors.senti.200.en%>%filter(to_what=="sentiment.en.youtube")%>%select('sentiment.en.human')),
      language=="DE"&preprocessing=="Baseline"&type=="Youtube" ~ as.numeric(table.cors.senti.basic%>%filter(to_what=="sentiment.de.youtube")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Stopwords"&type=="Youtube" ~ as.numeric(table.cors.senti.stopwords%>%filter(to_what=="sentiment.de.youtube")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Min count = 5"&type=="Youtube" ~ as.numeric(table.cors.senti.single%>%filter(to_what=="sentiment.de.youtube")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Min 100 words"&type=="Youtube" ~ as.numeric(table.cors.senti.100.de%>%filter(to_what=="sentiment.de.youtube")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Min 200 words"&type=="Youtube" ~ as.numeric(table.cors.senti.200.de%>%filter(to_what=="sentiment.de.youtube")%>%select('sentiment.de.human')),
      language=="FR"&preprocessing=="Baseline"&type=="Youtube" ~ as.numeric(table.cors.senti.basic%>%filter(to_what=="sentiment.fr.youtube")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Stopwords"&type=="Youtube" ~ as.numeric(table.cors.senti.stopwords%>%filter(to_what=="sentiment.fr.youtube")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Min count = 5"&type=="Youtube" ~ as.numeric(table.cors.senti.single%>%filter(to_what=="sentiment.fr.youtube")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Min 100 words"&type=="Youtube" ~ as.numeric(table.cors.senti.100.fr%>%filter(to_what=="sentiment.fr.youtube")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Min 200 words"&type=="Youtube" ~ as.numeric(table.cors.senti.200.fr%>%filter(to_what=="sentiment.fr.youtube")%>%select('sentiment.fr.human')),
      language=="EN"&preprocessing=="Baseline"&type=="API" ~ as.numeric(table.cors.senti.basic%>%filter(to_what=="sentiment.en.api")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Stopwords"&type=="API" ~ as.numeric(table.cors.senti.stopwords%>%filter(to_what=="sentiment.en.api")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Min count = 5"&type=="API" ~ as.numeric(table.cors.senti.single%>%filter(to_what=="sentiment.en.api")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Min 100 words"&type=="API" ~ as.numeric(table.cors.senti.100.en%>%filter(to_what=="sentiment.en.api")%>%select('sentiment.en.human')),
      language=="EN"&preprocessing=="Min 200 words"&type=="API" ~ as.numeric(table.cors.senti.200.en%>%filter(to_what=="sentiment.en.api")%>%select('sentiment.en.human')),
      language=="DE"&preprocessing=="Baseline"&type=="API" ~ as.numeric(table.cors.senti.basic%>%filter(to_what=="sentiment.de.api")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Stopwords"&type=="API" ~ as.numeric(table.cors.senti.stopwords%>%filter(to_what=="sentiment.de.api")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Min count = 5"&type=="API" ~ as.numeric(table.cors.senti.single%>%filter(to_what=="sentiment.de.api")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Min 100 words"&type=="API" ~ as.numeric(table.cors.senti.100.de%>%filter(to_what=="sentiment.de.api")%>%select('sentiment.de.human')),
      language=="DE"&preprocessing=="Min 200 words"&type=="API" ~ as.numeric(table.cors.senti.200.de%>%filter(to_what=="sentiment.de.api")%>%select('sentiment.de.human')),
      language=="FR"&preprocessing=="Baseline"&type=="API" ~ as.numeric(table.cors.senti.basic%>%filter(to_what=="sentiment.fr.api")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Stopwords"&type=="API" ~ as.numeric(table.cors.senti.stopwords%>%filter(to_what=="sentiment.fr.api")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Min count = 5"&type=="API" ~ as.numeric(table.cors.senti.single%>%filter(to_what=="sentiment.fr.api")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Min 100 words"&type=="API" ~ as.numeric(table.cors.senti.100.fr%>%filter(to_what=="sentiment.fr.api")%>%select('sentiment.fr.human')),
      language=="FR"&preprocessing=="Min 200 words"&type=="API" ~ as.numeric(table.cors.senti.200.fr%>%filter(to_what=="sentiment.fr.api")%>%select('sentiment.fr.human')),
      TRUE ~ 0
    )
  )

allcors_senti$preprocessing_language=paste(allcors_senti$preprocessing,allcors_senti$language)
allcors_senti$preprocessing_language=factor(allcors_senti$preprocessing_language, levels=unique(allcors_senti$preprocessing_language))
allcors_senti$preprocessing_language=factor(allcors_senti$preprocessing_language, levels=rev(levels(allcors_senti$preprocessing_language)))

allcors_senti$type=as.factor(allcors_senti$type)
allcors_senti$language=as.factor(allcors_senti$language)
allcors_senti$language=relevel(allcors_senti$language,"EN")

min_value=min(allcors$cor,allcors_senti$cor)-0.05

######
# This is Figure 2a)
pointplot_sotu_wordfish=ggplot(allcors,aes(y=preprocessing_language,x=cor,shape=type,fill=language))+
  geom_point(size=4)+
  scale_shape_manual(values=c(22,23), labels=c('API','YouTube'))+
  scale_fill_manual(values=c("red","blue","darkgreen"), name='Language', labels=c('EN','DE','FR'),
                    guide = guide_legend(override.aes = list(shape=15,color=c("red","blue","darkgreen"))))+
  labs(y="",x="Correlation of Wordfish estimates")+ 
  geom_vline(xintercept=0.9,linetype = "longdash")+
  geom_hline(yintercept=6.5,color='grey', size=1)+
  geom_hline(yintercept=12.5,color='grey', size=1)+
  annotate("text", x=0.55, y=13.5, label="English",colour="red",size=8,hjust = 0) +
  annotate("text", x=0.55, y=7.5, label="German",colour="blue",size=8,hjust = 0) +
  annotate("text", x=0.55, y=1.5, label="French",colour="darkgreen",size=8,hjust = 0) +
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"),
        legend.text=element_text(size=14,family="Verdana"),
        legend.title=element_text(size=16,family="Verdana"),
        legend.position="bottom")+
  scale_x_continuous(limits = c(min_value,1))+
  scale_y_discrete(labels = rev(allcors$preprocessing))+
  guides(color=guide_legend("language"), fill = FALSE)+ 
  guides(shape = guide_legend(override.aes = list(fill = "black")))
pointplot_sotu_wordfish=pointplot_sotu_wordfish+theme(legend.title=element_blank())
pointplot_sotu_wordfish

pdf("graphs_paper/Figure_2a.pdf", width = 12, height = 8) # Open a new pdf file
pointplot_sotu_wordfish
dev.off()

######
# This is Figure 2b)
pointplot_sotu_senti=ggplot(allcors_senti,aes(y=preprocessing_language,x=cor,shape=type,fill=language))+
  geom_point(size=4)+
  scale_shape_manual(values=c(22,23), labels=c('API','YouTube'))+
  scale_fill_manual(values=c("red","blue","darkgreen"), name='Language', labels=c('EN','DE','FR'),
                    guide = guide_legend(override.aes = list(shape=15,color=c("red","blue","darkgreen"))))+
  labs(y="",x="Correlation of sentiment estimates")+ 
  geom_vline(xintercept=0.9,linetype = "longdash")+
  geom_hline(yintercept=5.5,color='grey', size=1)+
  geom_hline(yintercept=10.5,color='grey', size=1)+
  annotate("text", x=0.55, y=11.5, label="English",colour="red",size=8,hjust = 0) +
  annotate("text", x=0.55, y=6.5, label="German",colour="blue",size=8,hjust = 0) +
  annotate("text", x=0.55, y=1.5, label="French",colour="darkgreen",size=8,hjust = 0) +
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=18,colour="black",family="Verdana"),
        axis.title=element_text(size=18,family="Verdana"),
        legend.background = element_rect(color = "black",
                                         fill = "white", size = 0.5, linetype = "solid"),
        legend.text=element_text(size=14,family="Verdana"),
        legend.title=element_text(size=16,family="Verdana"),
        legend.position="bottom")+
  scale_x_continuous(limits = c(min_value,1))+
  scale_y_discrete(labels = rev(allcors_senti$preprocessing))+
  guides(color=guide_legend("language"), fill = FALSE)+ 
  guides(shape = guide_legend(override.aes = list(fill = "black")))
pointplot_sotu_senti=pointplot_sotu_senti+theme(legend.title=element_blank())
pointplot_sotu_senti

pdf("graphs_paper/Figure_2b.pdf", width = 12, height = 8) # Open a new pdf file
pointplot_sotu_senti
dev.off()

##################################################
# Plot Wordfish Estimates
##################################################

wf_estimates_long=data.frame(preprocessing=c(rep("Human Transcription",58),rep("YouTube",58)),
                             Name=c(rep(joined.wf.speaker.basic$Name,2)),
                             stringsAsFactors = F)
wf_estimates_long$wf_est=c(joined.wf.speaker.basic$est.human.en,joined.wf.speaker.basic$est.youtube.en)
wf_estimates_long$Name[wf_estimates_long$Name=="Jaroslav Paška"]="Jaroslav Paska"

######
# This is Figure A5 in Appendix 3
graph.names.wf.2011.en<-ggplot(data=wf_estimates_long, mapping=aes(y=reorder(Name,wf_est), x=wf_est,shape=preprocessing))+
  geom_point(size=4)+
  labs(x="Position estimates",y="")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position=c(0.25, 0.9),
        legend.text=element_text(family="Verdana"))+
  labs(shape=NULL)
graph.names.wf.2011.en

pdf("graphs_paper/Figure_A5.pdf", height = 9) # Open a new pdf file
graph.names.wf.2011.en
dev.off()

wordfishs_long_parties=data.frame(preprocessing=c(rep("Human Transcription",10),rep("YouTube",10)),
                                  Name=c(rep(joined.wf.party.basic$Name,2)),
                                  stringsAsFactors = F)
wordfishs_long_parties$wf_est=c(joined.wf.party.basic$est.human.en,joined.wf.party.basic$est.youtube.en)

######
# This is Figure A6 in Appendix 3
graph.parties.wf.2011.en<-ggplot(data=wordfishs_long_parties, mapping=aes(y=reorder(Name,wf_est), x=wf_est,shape=preprocessing))+
  geom_point(size=4)+
  labs(x="Position estimates",y="")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position=c(0.25, 0.9),
        legend.text=element_text(family="Verdana"))+
  labs(shape=NULL)
graph.parties.wf.2011.en

pdf("graphs_paper/Figure_A6.pdf", height = 7) # Open a new pdf file
graph.parties.wf.2011.en
dev.off()

##################################################
# Protocol to Youtube Comparison
##################################################
# English Protocol - BASIC
DFM.protocol.2011.en=dfm(protocol.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.protocol.2011.en=dfm_trim(DFM.protocol.2011.en,min_termfreq  = 5)
wf.protocol.2011.en=austin::wordfish(quanteda::as.wfm(DFM.protocol.2011.en),dir=c(2,6),verbose=T)
sum.wf.protocol.2011.en=data.frame(est.protocol.en=wf.protocol.2011.en$theta,
                                   lwr.protocol.en=wf.protocol.2011.en$theta-1.96*wf.protocol.2011.en$se.theta,
                                   upr.protocol.en=wf.protocol.2011.en$theta+1.96*wf.protocol.2011.en$se.theta,
                                   Name=wf.protocol.2011.en$docs,stringsAsFactors = F)

meta.2011=read.csv("metadata/metadata_sotu_2011.csv",header=T,sep=";",stringsAsFactors = F)
meta.2011$Name_Protocol=trimws(meta.2011$Name_Protocol)
names(meta.2011)[1]<-"Number"
meta.2011.no.duplicates=meta.2011[!duplicated(meta.2011$Name),]

# English youtube - BASIC
DFM.youtube.2011.en=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Group")
DFM.youtube.2011.en=dfm_trim(DFM.youtube.2011.en,min_termfreq = 5)
wf.youtube.2011.en=austin::wordfish(quanteda::as.wfm(DFM.youtube.2011.en),dir=c(2,6),verbose=T)
sum.wf.youtube.2011.en=data.frame(est.youtube.en=wf.youtube.2011.en$theta,
                                  lwr.youtube.en=wf.youtube.2011.en$theta-1.96*wf.youtube.2011.en$se.theta,
                                  upr.youtube.en=wf.youtube.2011.en$theta+1.96*wf.youtube.2011.en$se.theta,
                                  Name=wf.youtube.2011.en$docs,stringsAsFactors = F)


wf_estimates_long=data.frame(preprocessing=c(rep("Protocol",10),rep("YouTube",10)),
                             Name=c(rep(sum.wf.protocol.2011.en$Name,2)),
                             stringsAsFactors = F)
wf_estimates_long$wf_est=c(sum.wf.protocol.2011.en$est.protocol.en,sum.wf.youtube.2011.en$est.youtube.en)
wf_estimates_long$Name[wf_estimates_long$Name=="Jaroslav Paška"]="Jaroslav Paska"
cor(sum.wf.protocol.2011.en$est.protocol.en,sum.wf.youtube.2011.en$est.youtube.en)

######
# This is Figure A8 in Appendix 5
graph.names.wf.prot.yt.2011.en<-ggplot(data=wf_estimates_long, mapping=aes(y=reorder(Name,wf_est), x=wf_est,shape=preprocessing))+
  geom_point(size=4)+
  labs(x="Position estimates",y="")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position=c(0.25, 0.9),
        legend.text=element_text(family="Verdana"))+
  labs(shape=NULL)
graph.names.wf.prot.yt.2011.en

pdf("graphs_paper/Figure_A8.pdf", height = 7) # Open a new pdf file
graph.names.wf.prot.yt.2011.en
dev.off()

##################################################
# Sentiment
senti_estimates_long=data.frame(preprocessing=c(rep("Human Transcription",58),rep("YouTube",58)),
                                Name=c(rep(joined.senti.speaker.basic$Name,2)),
                                stringsAsFactors = F)
senti_estimates_long$senti_est=c(joined.senti.speaker.basic$sentiment.en.human,joined.senti.speaker.basic$sentiment.en.youtube)
senti_estimates_long$Name[senti_estimates_long$Name=="Jaroslav Paška"]="Jaroslav Paska"

######
# This is Figure A9 in Appendix 6
graph.names.senti.2011.en<-ggplot(data=senti_estimates_long, mapping=aes(y=reorder(Name,senti_est), x=senti_est,shape=preprocessing))+
  geom_point(size=4)+
  labs(x="Sentiment",y="")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position=c(0.25, 0.9),
        legend.text=element_text(family="Verdana"))+
  labs(shape=NULL)
graph.names.senti.2011.en

pdf("graphs_paper/Figure_A9.pdf", height = 9) # Open a new pdf file
graph.names.senti.2011.en
dev.off()

senti_long_parties=data.frame(preprocessing=c(rep("Human Transcription",10),rep("YouTube",10)),
                              Name=c(rep(joined.senti.speaker.basic.party$Name,2)),
                              stringsAsFactors = F)
senti_long_parties$senti_est=c(joined.senti.speaker.basic.party$sentiment.en.human,joined.senti.speaker.basic.party$sentiment.en.youtube)

######
# This is Figure A10 in Appendix 6
graph.parties.wf.2011.en<-ggplot(data=senti_long_parties, mapping=aes(y=reorder(Name,senti_est), x=senti_est,shape=preprocessing))+
  geom_point(size=4)+
  labs(x="Sentiment",y="")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.text=element_text(size=12,colour="black",family="Verdana"),
        axis.title=element_text(size=12,family="Verdana"),
        legend.position=c(0.25, 0.9),
        legend.text=element_text(family="Verdana"))+
  labs(shape=NULL)
graph.parties.wf.2011.en

pdf("graphs_paper/Figure_A10.pdf", height = 7) # Open a new pdf file
graph.parties.wf.2011.en
dev.off()

#####################################################
# Structural Topic Models for Appendix
#####################################################
set.seed(1711)

# Splitting the speeches in 20 words
dataframe.20.sotu=data.frame(text=NA,
                             actor=NA,
                             position=NA)

for(i in 1:length(human.2011.en$documents$texts)){
  toks=unlist(tokens(human.2011.en$documents$texts[[i]],ngrams=20,concatenator = " "))
  toks=data.frame(toks[seq(1,length(toks),20)])
  names(toks)="text"
  toks$actor=human.2011.en$documents$Name[[i]]
  toks$position=(1:nrow(toks))/nrow(toks)
  dataframe.20.sotu=rbind(dataframe.20.sotu,toks)
}
dataframe.20.sotu=dataframe.20.sotu[-1,]
dataframe.20.sotu.human=dataframe.20.sotu


# Splitting the speeches in 20 words
dataframe.20.sotu=data.frame(text=NA,
                             actor=NA,
                             position=NA)

for(i in 1:length(youtube.2011.en$documents$texts)){
  toks=unlist(tokens(youtube.2011.en$documents$texts[[i]],ngrams=20,concatenator = " "))
  toks=data.frame(toks[seq(1,length(toks),20)])
  names(toks)="text"
  toks$actor=youtube.2011.en$documents$Name[[i]]
  toks$position=(1:nrow(toks))/nrow(toks)
  dataframe.20.sotu=rbind(dataframe.20.sotu,toks)
}
dataframe.20.sotu=dataframe.20.sotu[-1,]
dataframe.20.sotu.youtube=dataframe.20.sotu

dataframe.20.sotu.youtube$mode="YouTube"
dataframe.20.sotu.human$mode="Human Transcription"

data.frame.20.sotu.all=rbind(dataframe.20.sotu.human,dataframe.20.sotu.youtube)

sotu_20_corpus <- corpus(data.frame.20.sotu.all, text_field = "text")

temp<-textProcessor(documents=sotu_20_corpus$documents$texts,metadata=docvars(sotu_20_corpus))
out <- prepDocuments(temp$documents, temp$vocab, temp$meta)

vocab <- out$vocab
meta <- out$meta

stm_sotu <- stm(documents = out$documents, vocab = out$vocab,
                K = 10, prevalence =~ mode,
                data = out$meta,
                init.type = "Spectral")

prep_sotu <- estimateEffect(1:10 ~ mode, stm_sotu,
                            meta = out$meta, uncertainty = "Global")
######
# This is Figure A11 in Appendix 7
pdf("graphs_paper/Figure_A11.pdf", width = 12, height = 8) # Open a new pdf file
plot(prep_sotu, "mode", topics = 1:10, method = "difference",cov.value1 = "Human Transcription", cov.value2 = "YouTube",xlab = "Topic being more prevalent in corpus transcribed by humans")
dev.off()

#######################################################################
# Short Dictionary For Appendix
#######################################################################
#######################################################################
# Read in data
#######################################################################
save_results_shortdict=data.frame(ori_api_en=rep(NA,1000),
                                  ori_youtube_en=rep(NA,1000),
                                  ori_api_fr=rep(NA,1000),
                                  ori_youtube_fr=rep(NA,1000),
                                  ori_api_de=rep(NA,1000),
                                  ori_youtube_de=rep(NA,1000),stringsAsFactors = F)

set.seed(1711)
for(i in 1:1000){
  lexi.orig.en=extendeddict_en
  neg.lexi.en=data.frame(unlist(lexi.orig.en[1]),stringsAsFactors = F)
  pos.lexi.en=data.frame(unlist(lexi.orig.en[2]),stringsAsFactors = F)
  lexi.orig.short.en=dictionary(list(neg=sample(neg.lexi.en[,1],0.1*length(neg.lexi.en[,1]),replace = F),
                                  pos=sample(pos.lexi.en[,1],0.1*length(pos.lexi.en[,1]),replace = F)))
  lexi.orig.de=extendeddict_de_e
  neg.lexi.de=data.frame(unlist(lexi.orig.de[1]),stringsAsFactors = F)
  pos.lexi.de=data.frame(unlist(lexi.orig.de[2]),stringsAsFactors = F)
  lexi.orig.short.de=dictionary(list(neg=sample(neg.lexi.de[,1],0.1*length(neg.lexi.de[,1]),replace = F),
                                     pos=sample(pos.lexi.de[,1],0.1*length(pos.lexi.de[,1]),replace = F)))
  lexi.orig.fr=extendeddict_fr_e
  neg.lexi.fr=data.frame(unlist(lexi.orig.fr[1]),stringsAsFactors = F)
  pos.lexi.fr=data.frame(unlist(lexi.orig.fr[2]),stringsAsFactors = F)
  lexi.orig.short.fr=dictionary(list(neg=sample(neg.lexi.fr[,1],0.1*length(neg.lexi.fr[,1]),replace = F),
                                     pos=sample(pos.lexi.fr[,1],0.1*length(pos.lexi.fr[,1]),replace = F)))
  
  #######################################################################
  # Sentiment by speaker - BASIC with short dictionaries
  #######################################################################
  # English human - basic
  DFM.human.2011.en.senti.basic=dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.en)%>%
    convert(to="data.frame")
  DFM.human.2011.en.senti.basic$Name=DFM.human.2011.en.senti.basic$document
  DFM.human.2011.en.senti.basic=left_join(x=DFM.human.2011.en.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.human.2011.en.senti.basic$tokens=rowSums(dfm(human.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.human.2011.en.senti.basic.short=DFM.human.2011.en.senti.basic%>%
    mutate(sentiment.en.human=log((pos+0.5)/(neg+0.5)))
  
  # English Youtube - basic
  DFM.youtube.2011.en.senti.basic=dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.en)%>%
    convert(to="data.frame")
  DFM.youtube.2011.en.senti.basic$Name=DFM.youtube.2011.en.senti.basic$document
  DFM.youtube.2011.en.senti.basic=left_join(x=DFM.youtube.2011.en.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.youtube.2011.en.senti.basic$tokens=rowSums(dfm(youtube.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.youtube.2011.en.senti.basic.short=DFM.youtube.2011.en.senti.basic%>%
    mutate(sentiment.en.youtube=log((pos+0.5)/(neg+0.5)))
  
  # English API - basic
  DFM.api.2011.en.senti.basic=dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.en)%>%
    convert(to="data.frame")
  DFM.api.2011.en.senti.basic$Name=DFM.api.2011.en.senti.basic$document
  DFM.api.2011.en.senti.basic=left_join(x=DFM.api.2011.en.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.api.2011.en.senti.basic$tokens=rowSums(dfm(api.2011.en,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.api.2011.en.senti.basic.short=DFM.api.2011.en.senti.basic%>%
    mutate(sentiment.en.api=log((pos+0.5)/(neg+0.5)))
  
  # German human - basic
  DFM.human.2011.de.senti.basic=dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.de)%>%
    convert(to="data.frame")
  DFM.human.2011.de.senti.basic$Name=DFM.human.2011.de.senti.basic$document
  DFM.human.2011.de.senti.basic=left_join(x=DFM.human.2011.de.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.human.2011.de.senti.basic$tokens=rowSums(dfm(human.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.human.2011.de.senti.basic.short=DFM.human.2011.de.senti.basic%>%
    mutate(sentiment.de.human=log((pos+0.5)/(neg+0.5)))
  
  # German Youtube - basic
  DFM.youtube.2011.de.senti.basic=dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.de)%>%
    convert(to="data.frame")
  DFM.youtube.2011.de.senti.basic$Name=DFM.youtube.2011.de.senti.basic$document
  DFM.youtube.2011.de.senti.basic=left_join(x=DFM.youtube.2011.de.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.youtube.2011.de.senti.basic$tokens=rowSums(dfm(youtube.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.youtube.2011.de.senti.basic.short=DFM.youtube.2011.de.senti.basic%>%
    mutate(sentiment.de.youtube=log((pos+0.5)/(neg+0.5)))
  
  # German API - basic
  DFM.api.2011.de.senti.basic=dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.de)%>%
    convert(to="data.frame")
  DFM.api.2011.de.senti.basic$Name=DFM.api.2011.de.senti.basic$document
  DFM.api.2011.de.senti.basic=left_join(x=DFM.api.2011.de.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.api.2011.de.senti.basic$tokens=rowSums(dfm(api.2011.de,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.api.2011.de.senti.basic.short=DFM.api.2011.de.senti.basic%>%
    mutate(sentiment.de.api=log((pos+0.5)/(neg+0.5)))
  
  # French human - basic
  DFM.human.2011.fr.senti.basic=dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.fr)%>%
    convert(to="data.frame")
  DFM.human.2011.fr.senti.basic$Name=DFM.human.2011.fr.senti.basic$document
  DFM.human.2011.fr.senti.basic=left_join(x=DFM.human.2011.fr.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.human.2011.fr.senti.basic$tokens=rowSums(dfm(human.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.human.2011.fr.senti.basic.short=DFM.human.2011.fr.senti.basic%>%
    mutate(sentiment.fr.human=log((pos+0.5)/(neg+0.5)))
  
  # French Youtube - basic
  DFM.youtube.2011.fr.senti.basic=dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.fr)%>%
    convert(to="data.frame")
  DFM.youtube.2011.fr.senti.basic$Name=DFM.youtube.2011.fr.senti.basic$document
  DFM.youtube.2011.fr.senti.basic=left_join(x=DFM.youtube.2011.fr.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.youtube.2011.fr.senti.basic$tokens=rowSums(dfm(youtube.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.youtube.2011.fr.senti.basic.short=DFM.youtube.2011.fr.senti.basic%>%
    mutate(sentiment.fr.youtube=log((pos+0.5)/(neg+0.5)))
  
  # French API - basic
  DFM.api.2011.fr.senti.basic=dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name",dictionary = lexi.orig.short.fr)%>%
    convert(to="data.frame")
  DFM.api.2011.fr.senti.basic$Name=DFM.api.2011.fr.senti.basic$document
  DFM.api.2011.fr.senti.basic=left_join(x=DFM.api.2011.fr.senti.basic,y=meta.2011.no.duplicates,by="Name")
  DFM.api.2011.fr.senti.basic$tokens=rowSums(dfm(api.2011.fr,remove_punct = TRUE,remove_numbers=TRUE,groups="Name"))
  DFM.api.2011.fr.senti.basic.short=DFM.api.2011.fr.senti.basic%>%
    mutate(sentiment.fr.api=log((pos+0.5)/(neg+0.5)))
  
  # Combine
  joined.senti.speaker.basic.short=left_join(DFM.human.2011.en.senti.basic.short,DFM.youtube.2011.en.senti.basic.short%>%
                                               select(sentiment.en.youtube,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.api.2011.en.senti.basic.short%>%
                                               select(sentiment.en.api,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.human.2011.fr.senti.basic.short%>%
                                               select(sentiment.fr.human,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.youtube.2011.fr.senti.basic.short%>%
                                               select(sentiment.fr.youtube,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.api.2011.fr.senti.basic.short%>%
                                               select(sentiment.fr.api,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.human.2011.de.senti.basic.short%>%
                                               select(sentiment.de.human,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.youtube.2011.de.senti.basic.short%>%
                                               select(sentiment.de.youtube,Number),by="Number")
  joined.senti.speaker.basic.short=left_join(joined.senti.speaker.basic.short,DFM.api.2011.de.senti.basic.short%>%
                                               select(sentiment.de.api,Number),by="Number")
  
  table.cors.senti.basic.short=data.frame(round(cor(joined.senti.speaker.basic.short%>%select(grep("sentiment",names(joined.senti.speaker.basic.short)))),2),stringsAsFactors = F)
  table.cors.senti.basic.short$to_what=row.names(table.cors.senti.basic.short)
  table.cors.senti.basic.short=table.cors.senti.basic.short%>%select(grep("human",names(table.cors.senti.basic.short)),to_what)%>%
    filter(grepl("youtube",table.cors.senti.basic.short$to_what)|grepl("api",table.cors.senti.basic.short$to_what)|grepl("protocol",table.cors.senti.basic.short$to_what))
  table.cors.senti.basic.short
  
  save_results_shortdict$ori_api_en[i]=table.cors.senti.basic.short$sentiment.en.human[table.cors.senti.basic.short$to_what=="sentiment.en.api"]
  save_results_shortdict$ori_youtube_en[i]=table.cors.senti.basic.short$sentiment.en.human[table.cors.senti.basic.short$to_what=="sentiment.en.youtube"]
  save_results_shortdict$ori_api_fr[i]=table.cors.senti.basic.short$sentiment.fr.human[table.cors.senti.basic.short$to_what=="sentiment.fr.api"]
  save_results_shortdict$ori_youtube_fr[i]=table.cors.senti.basic.short$sentiment.fr.human[table.cors.senti.basic.short$to_what=="sentiment.fr.youtube"]
  save_results_shortdict$ori_api_de[i]=table.cors.senti.basic.short$sentiment.de.human[table.cors.senti.basic.short$to_what=="sentiment.de.api"]
  save_results_shortdict$ori_youtube_de[i]=table.cors.senti.basic.short$sentiment.de.human[table.cors.senti.basic.short$to_what=="sentiment.de.youtube"]
  print(i)
}

#################
# This is the information needed for Table A1 in Appendix 8
(ori_api_en_mean=mean(save_results_shortdict$ori_api_en))
(ori_api_en_lower=sort(save_results_shortdict$ori_api_en)[50])
(ori_api_en_upper=sort(save_results_shortdict$ori_api_en)[950])

(ori_youtube_en_mean=mean(save_results_shortdict$ori_youtube_en))
(ori_youtube_en_lower=sort(save_results_shortdict$ori_youtube_en)[50])
(ori_youtube_en_upper=sort(save_results_shortdict$ori_youtube_en)[950])

(ori_api_fr_mean=mean(save_results_shortdict$ori_api_fr))
(ori_api_fr_lower=sort(save_results_shortdict$ori_api_fr)[50])
(ori_api_fr_upper=sort(save_results_shortdict$ori_api_fr)[950])

(ori_youtube_fr_mean=mean(save_results_shortdict$ori_youtube_fr))
(ori_youtube_fr_lower=sort(save_results_shortdict$ori_youtube_fr)[50])
(ori_youtube_fr_upper=sort(save_results_shortdict$ori_youtube_fr)[950])

(ori_api_de_mean=mean(save_results_shortdict$ori_api_de))
(ori_api_de_lower=sort(save_results_shortdict$ori_api_de)[50])
(ori_api_de_upper=sort(save_results_shortdict$ori_api_de)[950])

(ori_youtube_de_mean=mean(save_results_shortdict$ori_youtube_de))
(ori_youtube_de_lower=sort(save_results_shortdict$ori_youtube_de)[50])
(ori_youtube_de_upper=sort(save_results_shortdict$ori_youtube_de)[950])

###############
# This is the information for Table 3 in the paper.
d <- data.frame("Original English"=c(round(mean(save_results_shortdict$ori_youtube_en),2),
                                     paste0("[",
                                            sort(save_results_shortdict$ori_youtube_en)[50],
                                            "; ",
                                            sort(save_results_shortdict$ori_youtube_en)[950],
                                            "]"),
                                     round(mean(save_results_shortdict$ori_api_en),2),
                                     paste0("[",
                                            sort(save_results_shortdict$ori_api_en)[50],
                                            "; ",
                                            sort(save_results_shortdict$ori_api_en)[950],
                                            "]"),"","","","","","","",""),
                "Original French"=c("","","","",round(mean(save_results_shortdict$ori_youtube_fr),2),
                                     paste0("[",
                                            sort(save_results_shortdict$ori_youtube_fr)[50],
                                            "; ",
                                            sort(save_results_shortdict$ori_youtube_fr)[950],
                                            "]"),
                                     round(mean(save_results_shortdict$ori_api_fr),2),
                                     paste0("[",
                                            sort(save_results_shortdict$ori_api_fr)[50],
                                            "; ",
                                            sort(save_results_shortdict$ori_api_fr)[950],
                                            "]"),"","","",""),
                "Original German"=c("","","","","","","","",round(mean(save_results_shortdict$ori_youtube_de),2),
                                    paste0("[",
                                           sort(save_results_shortdict$ori_youtube_de)[50],
                                           "; ",
                                           sort(save_results_shortdict$ori_youtube_de)[950],
                                           "]"),
                                    round(mean(save_results_shortdict$ori_api_de),2),
                                    paste0("[",
                                           sort(save_results_shortdict$ori_api_de)[50],
                                           "; ",
                                           sort(save_results_shortdict$ori_api_de)[950],
                                           "]")))

d
d$cat=c("English YouTube","","English API","",
               "French YouTube","","French API","",
               "German YouTube","","German API","")
print(xtable(d,
             caption = "Table A1: Correlation of Sentiment Estimates with Short Dictionary"),
      caption.placement = "top",
      type="latex",
      file="tables/TableA1.tex")
