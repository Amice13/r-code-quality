# Step 1: Load Requisite Packages and set directory paths----------------------

rm(list=ls())
library(tm)
library(tidytext)
library(broom)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggraph)
library(quanteda)
library(jtools)
library(huxtable)


#Set the string below to the directory containing replication files 
#Paste the wording directory of the replication folder below
#e.g., primary_path <- "~/Dropbox/cm_experiment_rep/"
primary_path <- ""
doc_path <- paste0(primary_path,"speech_analysis/speech_text")
set.seed(10101)




# Step 2: Load Speech Data, add Variables--------------------------------------
setwd(doc_path)
#Load data as a list
filenames <- list.files(getwd(),pattern="*.txt")
files <- lapply(filenames,readLines)
#Transform into a 'volatile' corpus via tm
docs <- VCorpus(VectorSource(files))
setwd(primary_path)


#Transform into a tidy object
txt<-tidy(docs)
#adding some substantive variables
txt$doctitle<-filenames

txt$year<-str_extract(filenames, "\\d{4}")
txt$location<-str_extract(filenames, "([A-Za-z]+)")

#Logged Word Count
txt<- txt %>%
  unnest_tokens(word, text) %>%
  group_by(doctitle) %>%
  summarize(words = log(n())) %>%
  inner_join(txt,by='doctitle')


#Document length in characters
txt$length<-log(nchar(txt$text))

#Measuring tone with Lexicoder via Quanteda
corp_quanteda <- corpus(docs)
toks <- tokens(corp_quanteda, remove_punct = TRUE)
lexi<-dfm_lookup(dfm(toks), data_dictionary_LSD2015[1:2])
lexi<-convert(lexi, to="data.frame")
txt$lexi_neg<-lexi$negative
txt$lexi_pos<-lexi$positive
txt$lexi_tone<-txt$lexi_pos - txt$lexi_neg

#Count the number of references to "troops" per document
txt<- txt %>%
  unnest_tokens(word, text,to_lower=T) %>%
  filter(word=="troops") %>%
  group_by(doctitle) %>%
  summarize(troop_refs = n()) %>%
  right_join(txt,by='doctitle')
txt$troop_refs[is.na(txt$troop_refs)]<-0
txt$troops<-ifelse(txt$troop_refs>0,1,0)

#Extract references to military elites
tidytxt<-txt %>%
  unnest_tokens(trigram, text,to_lower=F,token="ngrams",n=3)
trigrams_separated<- tidytxt %>%
  separate(trigram, c('word1','word2','word3'),sep=" ")
trigrams_filtered<-trigrams_separated %>%
  filter((word1!="Secretary" & word1!="Inspector" & word1!="Attorney"  & 
          word1!="Comptroller" & word2 == "General" &
          word3!="Assembly" & word3!="Than" & word3!="Motors" &
          word3!="Noriega" & word3!="Noreiga" & word3!="Khalid" &
          word3!="cedras" & word3!="Government") | 
           (word2 == "Joint" & word3 == "Chiefs") | 
           (word2 == "Admiral"))


#Count the number of military elite references by document
mentions<-(rep(1,length(trigrams_filtered$doctitle)))
doctitle<-trigrams_filtered$doctitle
mil_refs<-as.data.frame(cbind(doctitle,mentions))
mil_refs$mentions<-as.numeric(mil_refs$mentions)
mil_refs<-as.data.frame(mil_refs %>%
                          group_by(doctitle) %>%
                          dplyr::summarize(mentions = sum(mentions, na.rm=TRUE))
)
txt<-merge(txt,mil_refs,by=c('doctitle'),all.x=T,all.y=F)
txt$mentions[is.na(txt$mentions)]<-0
txt$milref<-ifelse(txt$mentions>0,1,0)

#merge in metadata
metadata<-read.csv('speech_analysis/metadata.csv',stringsAsFactors = F)
txt<-merge(txt,metadata,by.x="doctitle",by.y="File.Name",all.x=T,all.y=F)

#Presidents
txt$president<-NA
txt$president[txt$President==1]<-"H.W. Bush"
txt$president[txt$President==2]<-"Clinton"
txt$president[txt$President==3]<-"W. Bush"
txt$president[txt$President==4]<-"Obama"
txt$president<-as.factor(txt$president)
txt$president<-relevel(txt$president,ref=4)

#clean
txt$text<-str_replace_all(txt$text, "[\r\n]" , "")
txt$text<-str_replace_all(txt$text, "," , "")




# Step 3: Dropping Duplicate Speeches------------------------------------------

# In the Maxey (2017) scraped speech data, speeches are recorded twice when 
# they pertain to multiple intervention locations. We remove duplicates first  
# based on word counts, and subsequently through manual inspection. 
txt<-distinct(txt,words,lexi_neg,lexi_pos,troop_refs,year,President, 
              .keep_all= TRUE)
txt<-txt[txt$doctitle!="Afghanistan2003-13.txt",]
txt<-txt[txt$doctitle!="Afghanistan2004-9",]
txt<-txt[txt$doctitle!="Afghanistan2009-13.txt",]




# Step 4: Descriptive Analyses-------------------------------------------------

#setwd('~/Dropbox/cm_experiment_rep/results')
#descriptives by year
txt2<- txt %>%
  group_by(year) %>%
  summarize(milref2= sum(milref),count = n())  

#Figure 3 in the main text
pdf("results/fig3.pdf",height=8,width=12)
par(las=1, mar=c(5,5.5,2,2), family="sans",mfrow=c(1,1))
barplot(txt2$count,names.arg=as.character(txt2$year),horiz=F,las=2,
        ylab="Number of Speeches",col="#13294b",border="white",
        cex.axis=1.5, cex.names=1.5,cex.lab=1.5)
barplot(txt2$milref2,names.arg=as.character(txt2$year),horiz=F,las=2,
        ylab="",col="#E84A27",add=T,border="white",cex.axis=1.5,
        cex.names=1.5,cex.lab=1.5)
text(0,57,"Military Reference",col="#E84A27",pos=4,cex=1.75)
text(0,60,"No Military Reference",col="#13294b",pos=4,cex=1.75)
dev.off()

#Numer of speeches featuring a reference to a military elite
table(txt$milref)

#Elite references vs. references to troops
table(txt$milref,txt$troops)

#Mean references by president
mean(txt$milref[txt$president=="H.W. Bush"])
mean(txt$milref[txt$president=="Clinton"])
mean(txt$milref[txt$president=="W. Bush"])
mean(txt$milref[txt$president=="Obama"])

#Mean tone by president
summary(txt$lexi_tone[txt$president=="H.W. Bush"])
summary(txt$lexi_tone[txt$president=="Clinton"])
summary(txt$lexi_tone[txt$president=="W. Bush"])
summary(txt$lexi_tone[txt$president=="Obama"])




# Step 5 Regression Analysis (Main Text)---------------------------------------

#Starting year at 0 for ease of interpretation
txt$year<-as.numeric(txt$year)
txt$year<-txt$year-1990

#Run models and export to table
m1 <- glm(milref~lexi_tone+words+president+year,data=txt,family="binomial")
summary(m1)
m2 <- glm(milref~lexi_tone+words+president+troops+year,
          data=txt,family="binomial")
summary(m2)
coef_names <- c("Tone" = "lexi_tone", "Speech Length" = "words",
                "Reference to 'Troops' in Speech" = "troops", 
                "Clinton" = "presidentClinton",
                "H.W. Bush" = "presidentH.W. Bush", 
                "Obama" = "presidentObama",
                "Year"="year",
                "Intercept" = "(Intercept)")
export_summs(m1, m2, scale = T, transform.response = FALSE,to.file ='docx',
             digits=3,coefs = coef_names,file.name="results/table_2.docx")


#Generate predicted probabilities and vizualize 
sim_data <- data.frame(
  president='W. Bush',
  year=17,
  words=mean(txt$words),
  troops=1,
  lexi_tone=seq(from=-68,to=311,length.out=101))
OutHats<-predict(m2,se.fit=TRUE,newdata=sim_data)
OutHatsUB<-OutHats$fit+(1.96*OutHats$se.fit)
OutHatsLB<-OutHats$fit-(1.96*OutHats$se.fit)
OutHats<-cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats<-data.frame(lapply(OutHats,binomial(link="logit")$linkinv))
both<-cbind(sim_data,OutHats)

pdf("results/fig4.pdf", height=6, width=6)
par(NULL, 
    mar=c(3,4,0,2),
    mfrow=c(1,1))
plot(NULL,
     xlim = c(-70,310), 
     ylim=c(-.05,.5),
     axes = F, xlab = NA, ylab = NA) 
lines(both$lexi_tone,both$fit,col="black",lwd=1.75)
lines(both$lexi_tone,both$OutHatsUB, lty=2,col="Gray60",lwd=1)
lines(both$lexi_tone,both$OutHatsLB, lty=2,col="Gray60",lwd=1)
axis(1,cex.axis=.85,las=1,at=seq(-50,300,50))
axis(2,cex.axis=.85,las=1,at=seq(0,.5,.1))
mtext("Pr(Military Elite Reference)",side = 2, line =2.5,cex=1) 
mtext(side = 1, "Tone",line=2, cex=.85) 
points(txt$lexi_tone[txt$milref==0],rep(-.05,
                                        length(txt$lexi_tone[txt$milref==0])),
       col="steelblue", cex=.75, pch="l" )
points(txt$lexi_tone[txt$milref==1],rep(-.01,
                                        length(txt$lexi_tone[txt$milref==1])) , 
       col="orangered", cex=.75, pch="l" )
text(-0.037,-.01,"Reference to Military Elites",pos=3,cex=.75,col="orangered")
text(-0.025,-.05,"No Reference To Military Elites",
     pos=3,cex=.75,col="steelblue")
dev.off()

cor(txt$lexi_tone,txt$words,use="pairwise.complete.obs")
cor(txt$milref,txt$words,use="pairwise.complete.obs")
cor(txt$lexi_tone,txt$milref,use="pairwise.complete.obs")




# Step 6 Regression Analysis (Appendix)----------------------------------------

#Analysis by President (Appendix)
#Table A3.1
mp_o<-glm(milref~lexi_tone+words+troops, data=subset(txt,president=='Obama'),
          family="binomial")
summary(mp_o)
mp_w<-glm(milref~lexi_tone+words+troops, data=subset(txt,president=='W. Bush'),
          family="binomial")
summary(mp_w)
mp_c<-glm(milref~lexi_tone+words+troops, data=subset(txt,president=='Clinton'),
          family="binomial")
summary(mp_c)
mp_hw<-glm(milref~lexi_tone+words+troops,
           data=subset(txt,president=='H.W. Bush'), family="binomial")
summary(mp_hw)
coef_names <- c("Tone" = "lexi_tone", "Speech Length" = "words",
                "Reference to 'Troops' in Speech" = "troops",
                "Intercept" = "(Intercept)")
export_summs(mp_o, mp_w, mp_c, mp_hw, scale = T, 
             transform.response = F,to.file ='docx',digits=3,
             coefs = coef_names,file.name="results/table_a3_1.docx")

#Table A3.2

txt$hw <- ifelse(txt$president=='H.W. Bush',1,0)
txt$clinton <- ifelse(txt$president=='Clinton',1,0)
txt$bush <- ifelse(txt$president=='W. Bush',1,0)
txt$obama <- ifelse(txt$president=='Obama',1,0)

txt$hw_tone <- txt$lexi_tone * txt$hw
txt$clinton_tone <- txt$lexi_tone * txt$clinton
txt$bush_tone <- txt$lexi_tone * txt$bush
txt$obama_tone <- txt$lexi_tone * txt$obama


mp_int1<-glm(milref~lexi_tone+ hw_tone + clinton_tone + obama_tone +
               + words + hw + clinton + obama + year,data=txt,
             family="binomial")
mp_int2<-glm(milref~lexi_tone+ hw_tone + clinton_tone + obama_tone +
               + words + hw + clinton + obama  + troops + year,data=txt,
             family="binomial")
summary(mp_int1)
summary(mp_int2)
export_summs(mp_int1, mp_int2, scale=F, transform.response = F,
             to.file ='docx',digits=3,file.name="results/table_a3_2.docx")




# Step 7 Analysis of Iraq War--------------------------------------------------

# Reload speeches (Some duplicative Iraq speeches dropped above)
setwd(doc_path)
#Load data as a list
filenames <- list.files(getwd(),pattern="*.txt")
files <- lapply(filenames,readLines)
#Transform into a 'volatile' corpus via tm
docs <- VCorpus(VectorSource(files))
setwd(primary_path)

#Transform into a tidy object
txt<-tidy(docs)

#adding some substantive variables
txt$doctitle <- filenames
txt$year <- str_extract(filenames, "\\d{4}")
txt$location <- str_extract(filenames, "([A-Za-z]+)")

#Count the number of references to "troops" per document
txt<- txt %>%
  unnest_tokens(word, text,to_lower=T) %>%
  filter(word=="troops") %>%
  group_by(doctitle) %>%
  summarize(troop_refs = n()) %>%
  right_join(txt,by='doctitle')
txt$troop_refs[is.na(txt$troop_refs)]<-0
txt$troops<-ifelse(txt$troop_refs>0,1,0)

#Extract references to military elites
tidytxt <- txt %>%
  unnest_tokens(trigram, text,to_lower=F,token="ngrams",n=3)
trigrams_separated <- tidytxt %>%
  separate(trigram, c('word1','word2','word3'),sep=" ")
trigrams_filtered <- trigrams_separated %>%
  filter((word1!="Secretary" & word1!="Inspector" & word1!="Attorney"  & 
            word1!="Comptroller" & word2 == "General" &
            word3!="Assembly" & word3!="Than" & word3!="Motors" &
            word3!="Noriega" & word3!="Noreiga" & word3!="Khalid" &
            word3!="cedras" & word3!="Government") | 
           (word2 == "Joint" & word3 == "Chiefs") | 
           (word2 == "Admiral"))


#Count the number of military elite references by document
mentions <- (rep(1,length(trigrams_filtered$doctitle)))
doctitle <- trigrams_filtered$doctitle
mil_refs <- as.data.frame(cbind(doctitle,mentions))
mil_refs$mentions <- as.numeric(mil_refs$mentions)
mil_refs <- mil_refs %>%
  group_by(doctitle) %>%
  dplyr::summarize(mentions = sum(mentions, na.rm=TRUE)) %>% 
  as.data.frame()
txt <- merge(txt,mil_refs,by=c('doctitle'),all.x=T,all.y=F)
txt$mentions[is.na(txt$mentions)] <- 0
txt$milref <- ifelse(txt$mentions>0,1,0)


#Load speech metadata and fatality data
#setwd('~/Dropbox/cm_experiment_rep/')
name_date <- read.csv('speech_analysis/metadata.csv')
name_date$Date<-as.Date(name_date$Date, "%m/%d/%y")
name_date$Month <- month(name_date$Date)
name_date$Year <- year(name_date$Date)
iraq <- read.csv('speech_analysis/iraq_monthly_03to08.csv')
name_date <- subset(name_date,Country=="Iraq" & Year>=2003)
base_features <- merge(name_date, iraq, by=c('Month','Year'), all.x=T, all.y=T)
base_features <- base_features[order(base_features$Year,base_features$Month), ]
base_features$File.Name <- as.character(base_features$File.Name)
monthly <- merge(base_features,txt,by.x=c('File.Name'),by.y=c('doctitle'),
                 all.x=T,all.y=T)
monthly <- subset(monthly,!is.na(Month))
monthly$speeches <- 1
monthly <- monthly %>%
          group_by(Month, Year) %>%
          dplyr::summarize(speeches = sum(speeches, na.rm=TRUE),
                           milrefs = sum(milref,na.rm=T),
                           mentions = sum(mentions,na.rm=T),
                           TroopsBOG = mean(TroopsBOG,na.rm=T),
                           MilitaryFatalities = mean(MilitaryFatalities,na.rm=T)) %>% 
          as.data.frame()

monthly <- monthly[order(monthly$Year,monthly$Month),]
monthly <- monthly[!is.na(monthly$TroopsBOG),]
monthly$t <- seq(1,nrow(monthly), 1)

pdf("results/fig5.pdf",height=8,width=12)
par(mfrow=c(2,1),mar=c(5,6,3,3),las=0)
plot(NULL,
     xlim = c(-1,71), 
     ylim = c(0,20), 
     axes = F, xlab = NA, ylab = NA)
rect(47,-1,90,25,col = rgb(0.5,0.5,0.5,1/4),border=F)
lines(lowess(monthly$t,monthly$mentions,f=1/4),col="gray40",lwd=4)
points(monthly$t,monthly$mentions,bg=c("gray80"), 
       col="black", cex=1, pch=21, lwd=1.5)
axis(2,cex.axis=1.35)
axis(1,at=c(-1,11,23,35,47,59,71), 
     labels=c("2003","2004","2005","2006","2007","2008","2009"),cex.axis=1.25)
box()
mtext(side = 2, "Mentions of Military Elites", line=3, cex=1.25)
text(47,19,"U.S. Troop Surge",pos=4,cex=1.5)
title('Mentions of Military Elites in Presidential Speeches on Iraq')

plot(NULL,
     xlim = c(-1,71), 
     ylim = c(1,150), 
     axes = F, xlab = NA, ylab = NA)
rect(47,-10,90,200,col = rgb(0.5,0.5,0.5,1/4),border=F)
lines(lowess(monthly$t,monthly$MilitaryFatalities,f=1/4),col="gray40",lwd=4)
points(monthly$t,monthly$MilitaryFatalities,bg=c("gray80"), col="black",
       cex=1, pch=21, lwd=1.5)
axis(2,cex.axis=1.25)
axis(1,at=c(-1,11,23,35,47,59,71), 
     labels=c("2003","2004","2005","2006","2007","2008","2009"),cex.axis=1.25)
box()
mtext(side = 2, "Military Fatalities", line=3, cex=1.25)
text(47,140,"U.S. Troop Surge",pos=4,cex=1.5)
title('U.S. Military Fatalities in Iraq')
dev.off()




