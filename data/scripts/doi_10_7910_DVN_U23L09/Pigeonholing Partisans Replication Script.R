############################################
#### Replication file for "Pigeonholing ####
#### Partisans" in Political Behavior   ####
#### Created 3.17.2018 by the authors   ####
############################################

### LOADING REQUIRED PACKAGES
library(stm)
library(readstata13)
library(tm)
library(Hmisc)
library(dplyr)
library(Rtsne)
library(geometry)
library(stargazer)
library(sandwich)
library(reshape)
library(ggplot2)
library(stringr)
library(betareg)
#These lines are for robust standard errors in the later OLS modeling commands
library(sandwich)
library(foreign)
if (!require("RCurl")) {
  install.packages("RCurl", dependencies = TRUE)
  library(RCurl)
}
url_robust <- "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),
     envir=.GlobalEnv)

### To import the data, set working directory to appropriate location
setwd("")

####Table 1: Most common stereotypes of Democrats and Republicans####
##To create this table, read in the data in the "long" format. This 
##allows you to see the most common responses, ranging from most to
##least frequent.
long=read.dta13("Pigeonholing partisans_long.dta")
sort(table(long$GenD_recode),decreasing=TRUE)[1:11]
#This lists the top 10 response about Democrats - the command calls for
#11 responses because the most common "response" is to leave it blank.
sort(table(long$GenR_recode),decreasing=TRUE)[1:11]

####Structural topic modeling####
#The following code is necessary to prepare the open-ended responses
#for the topic modeling

#Load in the dataset (wide format)
setwd("")
data<- read.dta13("Pigeonholing partisans.dta")

# MAKING VARIABLES NUMERIC
data$Hisp2 <- ifelse(data$Hisp == "Not Hispanic", 0,1)

# REMOVING CASES WITH MISSING DATA (stm won't work otherwise)
data2 <- data[complete.cases(data$Inc),]
data3 <- data2[complete.cases(data2$Gender),]
data4 <- data3[complete.cases(data3$WHITE),]
data5 <- data4[complete.cases(data4$Interest),]
data6 <- data5[complete.cases(data5$sample),]
data7 <- data6[complete.cases(data6$Hisp2),]
data8 <- data7[complete.cases(data7$Age),]
data9 <- data8[complete.cases(data8$PK_Scale),]
data10 <- data9[complete.cases(data9$PID7),]
data11 <- data10[complete.cases(data10$Education2),]
data12 <- data11[which(data11$Inc>0),]


####Table 2: Democratic stereotype topics####
#PREPPING TEXT for the stm analyses
processedD <- textProcessor(data12$GenD, metadata = data12)
outD <- prepDocuments(processedD$documents, processedD$vocab,
	processedD$meta, lower.thresh = 5)
docsD <- outD$documents
vocabD <- outD$vocab
metaD <- outD$meta

## Model selection
GenDSelect10SAGE <- selectModel(outD$documents, outD$vocab, K = 10,
	prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
	Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
	data = outD$meta, runs = 30, seed = 1, LDAbeta=FALSE)

#Selected model:
GenDChoice <- GenDSelect10SAGE$runout[[5]]

##Topics to put into Table 2##
summary(GenDChoice)

####Table 3: Republican stereotype topics####
#PREPPING TEXT for the stm analyses
processedR <- textProcessor(data12$GenR, metadata = data12)
outR <- prepDocuments(processedR$documents, processedR$vocab,
	processedR$meta, lower.thresh = 5)
docsR <- outR$documents
vocabR <- outR$vocab
metaR <- outR$meta

#Model selection
GenRSelect10SAGE <- selectModel(outR$documents, outR$vocab, K = 10,
	prevalence =~ Inc + Gender + WHITE + Interest + Education2 + 
	Hisp2 + Age + PK_Scale + PID7 + as.factor(sample), max.em.its = 500,
	data = outR$meta, runs = 30, seed = 1, LDAbeta=FALSE)

### Selected model
GenRChoice <- GenRSelect10SAGE$runout[[5]]

##Topics to put into Table 3##
summary(GenRChoice)

####For the rest of the analyses, we want these two data objects (newdataR and newdataD)
## To be combined into a single object. These commands do that:
#First, we need to merging the two datafiles by the ID varaible
#The way to do it would be to merge the IDs with the topics and then merge into the meta data. 

#Merge the IDs with the Democrat topics
temp1 <- cbind(metaD$ID, GenDProbs)

#We need to rename the topics so that they don't duplicate
temp1=rename(temp1,c(`metaD$ID`="ID", Topic1="DTopic1", Topic2="DTopic2", Topic3="DTopic3", Topic4="DTopic4",
                     Topic5="DTopic5", Topic6="DTopic6", Topic7="DTopic7", Topic8="DTopic8", Topic9="DTopic9", 
                     Topic10="DTopic10") )

#Also, let's rename the topics in the newdata files so they have similar/the same labels
newdataR=rename(newdataR, c(Topic1="RTopic1", Topic2="RTopic2", Topic3="RTopic3", Topic4="RTopic4",
                            Topic5="RTopic5", Topic6="RTopic6", Topic7="RTopic7", Topic8="RTopic8",
                            Topic9="RTopic9", Topic10="RTopic10"))
newdataD=rename(newdataD,c(Topic1="DTopic1", Topic2="DTopic2", Topic3="DTopic3", Topic4="DTopic4",
                           Topic5="DTopic5", Topic6="DTopic6", Topic7="DTopic7", Topic8="DTopic8", Topic9="DTopic9", 
                           Topic10="DTopic10") )

#Now let's merge this shorter dataset into the newdataR file
newdata=merge(newdataR, temp1, by="ID")

#Now, delete the old objects we no longer need
temp1=NULL

####Recoding of topics, Table 4####
newdata$issgro=(newdata$DTopic1+newdata$DTopic7+newdata$RTopic10)/2
newdata$trait=(newdata$DTopic2+newdata$DTopic3+newdata$DTopic8+newdata$RTopic6+newdata$RTopic7+newdata$RTopic8)/2
newdata$ambig=(newdata$DTopic4+newdata$DTopic5+newdata$DTopic9+newdata$DTopic10+newdata$RTopic2+newdata$RTopic3+
                 newdata$RTopic4+newdata$RTopic5+newdata$RTopic9)/2

####Alternative coding of topics, mentioned in fn 12:####
newdata$issgro2=(newdata$DTopic1+newdata$DTopic7+newdata$RTopic10+newdata$RTopic3+newdata$DTopic5)/2
newdata$trait2=(newdata$DTopic2+newdata$DTopic3+newdata$DTopic8+newdata$RTopic6+newdata$RTopic7+newdata$RTopic8+
                  newdata$RTopic9+newdata$DTopic9+newdata$DTopic10+newdata$DTopic4)/2
newdata$ambig2=(newdata$RTopic2+newdata$RTopic4+newdata$RTopic5)/2

####Table 5: frequency of topic types####
#Issues/groups (strict)
#Whole sample
mean(newdata$issgro, na.rm=T)
#Democrats
mean(newdata$issgro[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$issgro[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$issgro[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$issgro[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$issgro[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$issgro[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$issgro[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Traits(strict)
#Whole sample
mean(newdata$trait, na.rm=T)
#Democrats
mean(newdata$trait[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$trait[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$trait[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$trait[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$trait[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$trait[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$trait[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Ambiguous (strict)
#Whole sample
mean(newdata$ambig, na.rm=T)
#Democrats
mean(newdata$ambig[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$ambig[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$ambig[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$ambig[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$ambig[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$ambig[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$ambig[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Issues/groups(broad)
#Whole sample
mean(newdata$issgro2, na.rm=T)
#Democrats
mean(newdata$issgro2[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$issgro2[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$issgro2[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$issgro2[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$issgro2[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$issgro2[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$issgro2[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Traits (broad)
#Whole sample
mean(newdata$trait2, na.rm=T)
#Democrats
mean(newdata$trait2[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$trait2[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$trait2[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$trait2[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$trait2[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$trait2[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$trait2[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Ambiguous (broad)
#Whole sample
mean(newdata$ambig2, na.rm=T)
#Democrats
mean(newdata$ambig2[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$ambig2[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$ambig2[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$ambig2[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$ambig2[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$ambig2[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$ambig2[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Blank (R)
#Whole sample
mean(newdata$RTopic1, na.rm=T)
#Democrats
mean(newdata$RTopic1[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$RTopic1[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$RTopic1[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$RTopic1[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$RTopic1[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$RTopic1[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$RTopic1[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Blank (D)
#Whole sample
mean(newdata$DTopic6, na.rm=T)
#Democrats
mean(newdata$DTopic6[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$DTopic6[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$DTopic6[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$DTopic6[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$DTopic6[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$DTopic6[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$DTopic6[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

#Blank
#Whole sample
mean(newdata$blank, na.rm=T)
#Democrats
mean(newdata$blank[newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$blank[newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$blank[newdata$PID3_p==2], na.rm=T)

#National sample
mean(newdata$blank[newdata$sample=="National"], na.rm=T)
#Democrats
mean(newdata$blank[newdata$sample=="National"& newdata$PID3_p==1], na.rm=T)
#Republicans
mean(newdata$blank[newdata$sample=="National" & newdata$PID3_p==3], na.rm=T)
#Independents
mean(newdata$blank[newdata$sample=="National" & newdata$PID3_p==2], na.rm=T)

####Correlation between traits and issues/groups topics, fn 14####
cor.test(newdata$issgro, newdata$trait, use="complete.obs")
###UPDATE THIS VALUE TO BE -0.07 (P=0.02) FOR STRICT AND 0.22 (P=0.000) FOR BROAD

####Models behind Figure 1: OLS regression predicting topic type use####
#First thing we need to do is create a "blank/no response" grouping
newdata$blank=(newdata$RTopic1+newdata$DTopic6)/2
#Here are the models
m.issue=lm(issgro~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.issue, robust=T)
m.trait=lm(trait~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.trait, robust=T)
m.ambig=lm(ambig~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.ambig, robust=T)
m.blank=lm(blank~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
             PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(m.blank, robust=T)

####Figure 1####
coef=c(-0.061, -0.001, 0.008,
       -0.012, 0.016, -0.003,
       -0.020, -0.012, -0.003) 
se= c(0.006, 0.003, 0.002,
      0.004, 0.002, 0.001,
      0.007, 0.003, 0.002
)

t1=cbind(coef, se)
t1=data.frame(t1)
t1$names=c("Knowledge", "Knowledge", "Knowledge",
           "Interest", "Interest", "Interest", 
           "Education", "Education", "Education")
t1$names=factor(t1$names, levels=c("Education", "Interest", "Knowledge"))
t1$outcome=c("Don't know", "Traits", "Issues/Groups",
             "Don't know", "Traits", "Issues/Groups",
             "Don't know", "Traits", "Issues/Groups")
t1$outcome=factor(t1$outcome, levels=c("Traits", "Issues/Groups", "Don't know"))
plot1=ggplot(data = t1, aes(x=outcome, y=coef, ymin=I(coef-(1.96*se)), ymax=I(coef+(1.96*se))))+
  geom_pointrange(color="blue") +
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y= "Change in proportion of this kind of topic", 
       x="Topic type", title="Independent variables")

plot1=plot1+facet_grid(.~names)+theme(strip.text.x = element_text(size = 15))+theme(strip.background = element_blank())
#Flips the other orientation
plot1+coord_flip()

####Figure 2####
#First, we need to create a smaller dataset that isn't missing on any
#of the variables we use so the models are all comparable.
temp=subset(newdata, select=c("ID", "issgro", "trait", "ambig", "issgro2", "trait2", "ambig2", "Inc", 
                              "Gender", "WHITE", "Interest", "Education2", "Hisp2", "Age", 
                              "PK_Scale", "PID7", "issues", "GenR_vmean", "GenD_vmean", "FT_diff",
                              "Ext_diff", "Ideo_diff", "Ext_Congress_diff", "Ideo_Congress_diff",
                              "Ideo_f"))
newdata2=temp[complete.cases(temp),]

#Now for the models
FT=lm(FT_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
          Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(FT, robust=T)
#Issue extremity
ext=lm(Ext_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
       + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext, robust=T)
#Ideology:
ideo=lm(Ideo_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
        + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo, robust=T)
#Issue extremity in Congress
ext_c=lm(Ext_Congress_diff~ issgro + trait +ambig + Inc + Gender + WHITE + Interest + Education2 
         + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext_c, robust=T)
#Ideology in Congress
ideo_c=lm(Ideo_Congress_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
          + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_c, robust=T)
#Ideology (self)
ideo_f=lm(Ideo_f~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
            Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_f, robust=T)

##Graphing Figure 2
coef=c(0.192, 0.778, 0.634, 0.622, -0.080, 0.763,
       -0.036, 0.599,-0.044, 1.326, -0.105, 0.768) 
se= c(0.186, 0.139, 0.199, 0.136, 0.154, 0.160,
      0.144,0.135, 0.190, 0.153, 0.232, 0.205)
t1=cbind(coef, se)
t1=data.frame(t1)
t1$names=c("Ideological difference", "Ideological difference",
           "Ideological difference in Congress", "Ideological difference in Congress",
           "Extremity difference", "Extremity difference",
           "Extremity difference in Congress", "Extremity difference in Congress",
           "Feeling thermometer difference", "Feeling thermometer difference",
           "Self-rated ideology", "Self-rated ideology")
t1$names=factor(t1$names, levels=c("Feeling thermometer difference",
                                   "Self-rated ideology",
                                   "Extremity difference in Congress",
                                   "Extremity difference",
                                   "Ideological difference in Congress",
                                   "Ideological difference"))

t1$topic=c("Issues/Groups", "Traits",
           "Issues/Groups", "Traits",
           "Issues/Groups", "Traits",
           "Issues/Groups", "Traits",
           "Issues/Groups", "Traits",
           "Issues/Groups", "Traits")

#These commands do the actual graphing
plot1=ggplot(data = t1, aes(x=names, y=coef, ymin=I(coef-(1.96*se)), ymax=I(coef+(1.96*se))))+
  geom_pointrange(color="blue", size=1, fatten=2.5) +
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),  axis.text.y = element_text(size=16), 
        axis.text.x=element_text(size=16),axis.title.x = element_text(size=17),
        axis.title.y = element_text(size=17))+
  labs(title="",
       x="Polarization dependent variables",
       y= "Change in polarization (increases = more polarization)")

plot1=plot1+facet_grid(.~topic)+theme(strip.text.x = element_text(size = 18))+theme(strip.background = element_blank())

#Flips it, simply because I like the other orientation better.
plot1+coord_flip()

####Broader coding of topics, fn. 12 ####
#Fn. 12 notes that our findings are the same if we use the broader coding. These
#models show those results:
#Issue extremity
ext=lm(Ext_diff~ issgro2 + trait2 + ambig2 + Inc + Gender + WHITE + Interest + Education2 
       + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext, robust=T)

#Ideology:
ideo=lm(Ideo_diff~ issgro2 + trait2 + ambig2 + Inc + Gender + WHITE + Interest + Education2 
        + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo, robust=T)

#Model for issue extremity in Congress
ext_c=lm(Ext_Congress_diff~ issgro2 + trait2 +ambig2 + Inc + Gender + WHITE + Interest + Education2 
         + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext_c, robust=T)

#Ideology in Congress
ideo_c=lm(Ideo_Congress_diff~ issgro2 + trait2 + ambig2 + Inc + Gender + WHITE + Interest + Education2 
          + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_c, robust=T)

#Ideology (self)
ideo_f=lm(Ideo_f~ issgro2 + trait2 + ambig2 + Inc + Gender + WHITE + Interest + Education2 + 
            Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_f, robust=T)

#FT
FT=lm(FT_diff~ issgro2 + trait2 + ambig2 + Inc + Gender + WHITE + Interest + Education2 + 
        Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(FT, robust=T)

####Fn. 15 - alternative specifications####
#This note mentions verifying the results are the same with beta regression. These
#analyses demonstrate that point.
mb.issue=betareg(issgro~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                   PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.issue)
mb.trait=betareg(trait~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                   PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.trait)
mb.ambig=betareg(ambig~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                   PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.ambig)
mb.issue2=betareg(issgro2~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                    PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.issue2)
mb.trait2=betareg(trait2~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                    PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.trait2)
mb.ambig2=betareg(ambig2~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                    PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.ambig2)
mb.blank=betareg(blank~Inc + Gender + WHITE + Interest + Education2 + Hisp2 + Age+ 
                   PK_Scale + PID7 + as.factor(sample), data=newdata)
summary(mb.blank)


####Subject pool analyses, fn.21####
#Step 1 - make the FT difference variable for the subject pool
newdata$FT_diff_sp=(abs(newdata$FT_Dem-newdata$FT_Rep))/100
#Models
FT_sp=lm(FT_diff_sp~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + 
          Hisp2 + Age + PK_Scale + PID7+GenR_vmean+GenD_vmean, data=newdata)
summary(FT_sp, robust=T)

####Combining trait and group-based stereotypes, fn.22 ####
newdata$issgro3=(newdata$DTopic1+newdata$DTopic7+newdata$RTopic10)/2
newdata$trait3=(newdata$DTopic2+newdata$DTopic3+newdata$DTopic8+newdata$RTopic6+newdata$RTopic7+
                  newdata$RTopic8+newdata$DTopic4+newdata$DTopic5+newdata$DTopic9+newdata$DTopic10+
                  newdata$RTopic2+newdata$RTopic3+newdata$RTopic4+newdata$RTopic5+newdata$RTopic9)/2
temp=subset(newdata, select=c("ID", "issgro", "trait", "ambig", "issgro2", "trait2", "ambig2", 
                              "issgro3", "trait3",
                              "Inc", "Gender", "WHITE", "Interest", "Education2", "Hisp2", "Age", 
                              "PK_Scale", "PID7", "issues", "GenR_vmean", "GenD_vmean", "FT_diff",
                              "Ext_diff", "Ideo_diff", "Ext_Congress_diff", "Ideo_Congress_diff",
                              "Ideo_f"))
newdata2=temp[complete.cases(temp),]
#Redo the models now:
ext=lm(Ext_diff~ issgro3 + trait3 + Inc + Gender + WHITE + Interest + Education2 
       + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext, robust=T)

#Ideology:
ideo=lm(Ideo_diff~ issgro3 + trait3 + Inc + Gender + WHITE + Interest + Education2 
        + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo, robust=T)

#Issue extremity in Congress
ext_c=lm(Ext_Congress_diff~ issgro3 + trait3 + Inc + Gender + WHITE + Interest + Education2 
         + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ext_c, robust=T)

#Ideology in Congress
ideo_c=lm(Ideo_Congress_diff~ issgro3 + trait3 + Inc + Gender + WHITE + Interest + Education2 
          + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_c, robust=T)

#Ideology (self)
ideo_f=lm(Ideo_f~ issgro3 + trait3 + Inc + Gender + WHITE + Interest + Education2 + 
            Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(ideo_f, robust=T)

#FT
FT=lm(FT_diff~ issgro3 + trait3 + Inc + Gender + WHITE + Interest + Education2 
      + Hisp2 + Age + PK_Scale + PID7+issues+GenR_vmean+GenD_vmean, data=newdata2)
summary(FT, robust=T)

####Interactions between content and valence, fn. 24####
#First thing, then is to recode the valence measures
newdata2$Out_vmean[newdata2$PID7<4]=newdata2$GenR_vmean[newdata2$PID7<4]
newdata2$Out_vmean[newdata2$PID7>4]=newdata2$GenD_vmean[newdata2$PID7>4]
newdata2$Out_vmean[newdata2$PID7==4]=(newdata2$GenD_vmean[newdata2$PID7==4]+
                                        newdata2$GenR_vmean[newdata2$PID7==4])/2
library(interplot)
#FT
FT_2=lm(FT_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
          Hisp2 + Age + PK_Scale + PID7+issues+Out_vmean+ trait:Out_vmean+issgro:Out_vmean, data=newdata2)
summary(FT_2, robust=T)
#Graph
interplot(m=FT_2, var1='trait', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on FT polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of traits at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
interplot(m=FT_2, var1='issgro', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on FT polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of issues/groups at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))

#Issue extremity 
ext=lm(Ext_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
       + Hisp2 + Age + PK_Scale + PID7+issues+Out_vmean+ trait:Out_vmean+issgro:Out_vmean, data=newdata2)
summary(ext, robust=T)

interplot(m=ext, var1='trait', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on extremity polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of traits at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
interplot(m=ext, var1='issgro', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on extremity polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of issues/groups at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))

#Ideology:
ideo=lm(Ideo_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
        + Hisp2 + Age + PK_Scale + PID7+issues+Out_vmean + trait:Out_vmean+issgro:Out_vmean, data=newdata2)
summary(ideo, robust=T)

interplot(m=ideo, var1='trait', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on ideological polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of traits at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
interplot(m=ext, var1='issgro', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on ideological polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of issues/groups at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))

#Issue extremity in Congress
ext_c=lm(Ext_Congress_diff~ issgro + trait +ambig + Inc + Gender + WHITE + Interest + Education2 
         + Hisp2 + Age + PK_Scale + PID7+issues+Out_vmean+ trait:Out_vmean+issgro:Out_vmean, data=newdata2)
summary(ext_c, robust=T)

interplot(m=ext_c, var1='trait', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on extremity (Congress) polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of traits at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
interplot(m=ext, var1='issgro', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on extremity (Congress) polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of issues/groups at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))

#Ideology in Congress
ideo_c=lm(Ideo_Congress_diff~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 
          + Hisp2 + Age + PK_Scale + PID7+issues+Out_vmean+ trait:Out_vmean+issgro:Out_vmean, data=newdata2)
summary(ideo_c, robust=T)

interplot(m=ideo_c, var1='trait', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on ideological (Congress) polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of traits at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
interplot(m=ext, var1='issgro', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on ideological (Congress) polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of issues/groups at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))

#Ideology (self)
ideo_f=lm(Ideo_f~ issgro + trait + ambig + Inc + Gender + WHITE + Interest + Education2 + 
            Hisp2 + Age + PK_Scale + PID7+issues+Out_vmean+ trait:Out_vmean+issgro:Out_vmean, data=newdata2)
summary(ideo_f, robust=T)

interplot(m=ideo_f, var1='trait', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on self ideology polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of traits at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
interplot(m=ext, var1='issgro', var2='Out_vmean', plot=T, hist=T)+
  ylab("Effect on self ideology polarization")+
  xlab("Out_vmean ratings")+
  geom_hline(yintercept=0, size=1, linetype="dashed", color="red")+
  ggtitle("Effect of issues/groups at levels of Out_vmean")+
  theme(plot.title = element_text(hjust = 0.5))
