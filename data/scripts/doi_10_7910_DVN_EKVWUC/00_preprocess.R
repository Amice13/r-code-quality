rm(list=ls())
library(dplyr)
library(ggplot2)
library(cregg)
library(rio)
library(lubridate)
setwd('~/Dropbox/cm_project2/KenwickMaxey_JCR2024_replication/preprocess')

##############################################################################
#Dropping people who did not consent
#Note: this is commented out because it was run prior to making data public
#data <- read.csv('survey1_full.csv')
#data <- subset(data, Consent=="I agree")
#write.csv(data, file='survey1_full.csv',row.names=F, na="")

#data <- read.csv('survey2_full_nofreetext.csv')
#data <- subset(data, Consent=="I agree")
#write.csv(data, file='survey2_full_nofreetext.csv',row.names=F, na="")

#data <- read.csv('survey2_full.csv')
#data <- subset(data, Consent=="I agree")
#write.csv(data, file='survey2_full.csv',row.names=F, na="")
##############################################################################

#Experiment 1: Conjoint-Only
data <- read.csv('survey1_full.csv')

data <- subset(data,Q_RecaptchaScore>0.5) #drop potential bots

#Attention check
table(data$attention1)/sum(table(data$attention1)) #77% pass

data<-data[!(data$traits1a==""),] #drop people who do not make it to conjoint (including attn. check failures)
data$id<-seq(1,nrow(data))


#transform party variables
data$party_order<-NA
data$party_order[data$pid3=="Democrat"]<- -2
data$party_order[data$pid_lean=="Lean to the Democrat Party"]<- -1
data$party_order[data$pid_lean=="Lean to neither"]<- 0
data$party_order[data$pid_lean=="Lean to the Republican Party"]<- 1
data$party_order[data$pid3=="Republican"]<- 2
data$rep <- ifelse(data$party_order>0,1,0)

#new object for conjoint analysis
meta<-dplyr::select(data,id,party_order,rep)

cats<-c("agency","strategy","budget","immig","ht","dt","terror")

#Separate data sets for 38 cases where people didn't finish
data1 <- data[!(data$traits1a==""),]
data2 <- data[!(data$traits2a==""),]
data3 <- data[!(data$traits3a==""),]
data4 <- data[!(data$traits4a==""),]
data5 <- data[!(data$traits5a==""),]

meta1 <- meta[!(data$traits1a==""),]
meta2 <- meta[!(data$traits2a==""),]
meta3 <- meta[!(data$traits3a==""),]
meta4 <- meta[!(data$traits4a==""),]
meta5 <- meta[!(data$traits5a==""),]

#Choice 1
c1a<-do.call(rbind.data.frame, strsplit(data1[["traits1a"]], "\\|"))
names(c1a)<-cats
c1b<-do.call(rbind.data.frame, strsplit(data1[["traits1b"]], "\\|"))
names(c1b)<-cats

#Choice 2
c2a<-do.call(rbind.data.frame, strsplit(data2[["traits2a"]], "\\|"))
names(c2a)<-cats
c2b<-do.call(rbind.data.frame, strsplit(data2[["traits2b"]], "\\|"))
names(c2b)<-cats

#Choice 3
c3a<-do.call(rbind.data.frame, strsplit(data3[["traits3a"]], "\\|"))
names(c3a)<-cats
c3b<-do.call(rbind.data.frame, strsplit(data3[["traits3b"]], "\\|"))
names(c3b)<-cats

#Choice 4
c4a<-do.call(rbind.data.frame, strsplit(data4[["traits4a"]], "\\|"))
names(c4a)<-cats
c4b<-do.call(rbind.data.frame, strsplit(data4[["traits4b"]], "\\|"))
names(c4b)<-cats

#Choice 5
c5a<-do.call(rbind.data.frame, strsplit(data5[["traits5a"]], "\\|"))
names(c5a)<-cats
c5b<-do.call(rbind.data.frame, strsplit(data5[["traits5b"]], "\\|"))
names(c5b)<-cats

#Choices
c1a$chosen<-ifelse(data1$policy_preference1=="Policy A",1,0)
c1b$chosen<-ifelse(data1$policy_preference1=="Policy A",0,1)
c1a$chosen[data1$policy_preference1==""] <- NA
c1b$chosen[data1$policy_preference1==""] <- NA

c2a$chosen<-ifelse(data2$policy_preference2=="Policy A",1,0)
c2b$chosen<-ifelse(data2$policy_preference2=="Policy A",0,1)
c2a$chosen[data2$policy_preference2==""] <- NA
c2b$chosen[data2$policy_preference2==""] <- NA

c3a$chosen<-ifelse(data3$policy_preference3=="Policy A",1,0)
c3b$chosen<-ifelse(data3$policy_preference3=="Policy A",0,1)
c3a$chosen[data3$policy_preference3==""] <- NA
c3b$chosen[data3$policy_preference3==""] <- NA

c4a$chosen<-ifelse(data4$policy_preference4=="Policy A",1,0)
c4b$chosen<-ifelse(data4$policy_preference4=="Policy A",0,1)
c4a$chosen[data4$policy_preference4==""] <- NA
c4b$chosen[data4$policy_preference4==""] <- NA

c5a$chosen<-ifelse(data5$policy_preference5=="Policy A",1,0)
c5b$chosen<-ifelse(data5$policy_preference5=="Policy A",0,1)
c5a$chosen[data5$policy_preference5==""] <- NA
c5b$chosen[data5$policy_preference5==""] <- NA

c1a<-cbind(meta1,c1a)
c1b<-cbind(meta1,c1b)
c2a<-cbind(meta2,c2a)
c2b<-cbind(meta2,c2b)
c3a<-cbind(meta3,c3a)
c3b<-cbind(meta3,c3b)
c4a<-cbind(meta4,c4a)
c4b<-cbind(meta4,c4b)
c5a<-cbind(meta5,c5a)
c5b<-cbind(meta5,c5b)

c1a$task <- 1
c1b$task <- 1
c2a$task <- 2
c2b$task <- 2
c3a$task <- 3
c3b$task <- 3
c4a$task <- 4
c4b$task <- 4
c5a$task <- 5
c5b$task <- 5

#Combine and store as cj_df() object
res<-rbind(c1a,c1b,c2a,c2b,c3a,c3b,c4a,c4b,c5a,c5b)
res<-cj_df(res)

#Labeling for modeling and visualization
res$agency<-as.factor(res$agency)
res$strategy1<-as.factor(res$strategy)
levels(res$strategy1)
res$strategy<-factor(res$strategy1,levels=levels(res$strategy1)[rev(c(1,3,7,2,5,6,4))])
levels(res$strategy)
strats <- rev(c("Root Causes in Border Region","Enhancing Cooperation with Locals", 
            "Tracking with Tech.","Blocking at Crossings", "Stopping Outside Crossings", 
            "Patrolling Walls","Heavily Armed Patrols"))
levels(res$strategy) <- strats
levels(res$strategy)

res$budget<-factor(res$budget,levels=c("0 - 5% increase", "5-10% increase","10-15% increase","15-20% increase","20-40% increase","40-60% increase"))
res$immig<-factor(res$immig,levels=c("0-5% reduction","5-10% reduction","10-20% reduction","20-40% reduction","40-60% reduction","60-80% reduction"))

res$ht[res$ht=="0-5% reduction"]<-" 0-5% reduction"
res$ht[res$ht=="5-10% reduction"]<-" 5-10% reduction"
res$ht[res$ht=="10-20% reduction"]<-" 10-20% reduction"
res$ht[res$ht=="20-40% reduction"]<-" 20-40% reduction"
res$ht[res$ht=="40-60% reduction"]<-" 40-60% reduction"
res$ht[res$ht=="60-80% reduction"]<-" 60-80% reduction"
res$ht<-factor(res$ht,levels=c(" 0-5% reduction",
                               " 5-10% reduction",
                               " 10-20% reduction",
                               " 20-40% reduction",
                               " 40-60% reduction",
                               " 60-80% reduction"))

res$dt[res$dt=="0-5% reduction"]<-"  0-5% reduction"
res$dt[res$dt=="5-10% reduction"]<-"  5-10% reduction"
res$dt[res$dt=="10-20% reduction"]<-"  10-20% reduction"
res$dt[res$dt=="20-40% reduction"]<-"  20-40% reduction"
res$dt[res$dt=="40-60% reduction"]<-"  40-60% reduction"
res$dt[res$dt=="60-80% reduction"]<-"  60-80% reduction"
res$dt<-factor(res$dt,levels=c("  0-5% reduction",
                               "  5-10% reduction",
                               "  10-20% reduction",
                               "  20-40% reduction",
                               "  40-60% reduction",
                               "  60-80% reduction"))

res$terror[res$terror=="0-5% reduction"]<-"   0-5% reduction"
res$terror[res$terror=="5-10% reduction"]<-"   5-10% reduction"
res$terror[res$terror=="10-20% reduction"]<-"   10-20% reduction"
res$terror[res$terror=="20-40% reduction"]<-"   20-40% reduction"
res$terror[res$terror=="40-60% reduction"]<-"   40-60% reduction"
res$terror[res$terror=="60-80% reduction"]<-"   60-80% reduction"
res$terror<-factor(res$terror,levels=c("   0-5% reduction",
                                       "   5-10% reduction",
                                       "   10-20% reduction",
                                       "   20-40% reduction",
                                       "   40-60% reduction",
                                       "   60-80% reduction"))
res$agency<- rio::factorize(res$agency)
attr(res$agency, "label") <- "AGENCY"
attr(res$strategy, "label") <- "STRATEGY"
attr(res$budget, "label") <- "BUDGET"
attr(res$immig, "label") <- "EFFECT ON ILLEGAL IMMIGRATION"
attr(res$ht, "label") <- "HUMAN TRAFFICKING"
attr(res$dt, "label") <- "EFFECT ON DRUG TRAFFICKING"
attr(res$terror, "label") <- "EFFECT ON TERRORISM"

res$id <- as.factor(res$id)


##############################################################################
#Experiment 2: Treatment Analysis
data_tr <- read.csv('survey2_full_nofreetext.csv')

data_tr <- subset(data_tr,Q_RecaptchaScore>0.5)#drop likely bots

#Attention 1 Check
table(data_tr$attention1)/sum(table(data_tr$attention1)) #78% pass

data_tr<-data_tr[!(data_tr$traits1a==""),] #drop people who do not make it to conjoit, including attn. check failures


#Manipulation check question
data_tr$mancheck<-NA
data_tr$mancheck[data_tr$manipcheck=="People identifying as white are expected to REMAIN THE MAJORITY of the population for the foreseeable future."]<-1
data_tr$mancheck[data_tr$manipcheck=="People identifying as white are expected to FALL TO A MINORITY of the population in the foreseeable future."]<-2
data_tr$mancheck[data_tr$manipcheck=="The United States is expected to RETAIN its competitive military advantage and status for the next twenty years."]<-3
data_tr$mancheck[data_tr$manipcheck=="The United States is expected to LOSE its competitive military advantage and status in the next twenty years."]<-4
data_tr$treatment_condition<-data_tr$openended #new variable w/ more intuitive name for openended, which tracks treatment category
#Condition 1: Domestic status Retention
#Condition 2: Domestic status Loss
#Condition 3: Intl status Retention
#Condition 4: Intl status Loss
data_tr$manip_pass<-ifelse(data_tr$mancheck==data_tr$treatment_condition,1,0)

#Race/ethnicity dummies
data_tr$white_nonhisp <- ifelse(data_tr$race=="Caucasian (white)" & data_tr$hispanic=="No",1,0)
data_tr$white_nonhisp_na <- data_tr$white_nonhisp
data_tr$white_nonhisp_na[data_tr$race=="" & data_tr$hispanic==""] <- 1
data_tr$hisp <- ifelse(data_tr$hispanic=="Yes",1,0)
data_tr$black <- ifelse(data_tr$race=="African American (black)",1,0)
data_tr$asian <- ifelse(data_tr$race=="Asian-American",1,0)

#Age
data_tr$age_cat <- NA
data_tr$age_cat[data_tr$age=="18-19"] <- 1
data_tr$age_cat[data_tr$age=="20-29"] <- 2
data_tr$age_cat[data_tr$age=="30-39"] <- 3
data_tr$age_cat[data_tr$age=="40-49"] <- 4
data_tr$age_cat[data_tr$age=="50-59"] <- 5
data_tr$age_cat[data_tr$age=="60-69"] <- 6
data_tr$age_cat[data_tr$age=="Older than 69"] <- 7

data_tr$education_cat <- NA
data_tr$education_cat[data_tr$education=="Did not finish high school"] <- 1
data_tr$education_cat[data_tr$education=="High school graduate, no further schooling"] <- 2
data_tr$education_cat[data_tr$education=="Some college, but no degree"] <- 3
data_tr$education_cat[data_tr$education=="Community college or Associate's Degree"] <- 4
data_tr$education_cat[data_tr$education=="Bachelor's Degree or equivalent"] <- 5
data_tr$education_cat[data_tr$education=="Graduate or professional degree"] <- 6

data_tr$id<-seq(1,nrow(data_tr))

#Attention check 2
data_tr$attn2_pass <- 0
data_tr$attn2_pass[data_tr$attention2_block1_1 > 45 & data_tr$attention2_block1_1 < 55] <- 1
data_tr$attn2_pass[data_tr$attention2_block2_1 > 45 & data_tr$attention2_block2_1 < 55] <- 1
data_tr$attn2_pass[is.na(data_tr$attention2_block1_1) & is.na(data_tr$attention2_block2_1)] <- NA

#Party
data_tr$party_order<-NA
data_tr$party_order[data_tr$pid3=="Democrat"]<- -2
data_tr$party_order[data_tr$pid_lean=="Lean to the Democrat Party"]<- -1
data_tr$party_order[data_tr$pid_lean=="Lean to neither"]<- 0
data_tr$party_order[data_tr$pid_lean=="Lean to the Republican Party"]<- 1
data_tr$party_order[data_tr$pid3=="Republican"]<- 2
data_tr$rep <- ifelse(data_tr$party_order>0,1,0)
data_tr$dem <- ifelse(data_tr$party_order<0,1,0)

#ideology
data_tr$ideology_scale <- NA
data_tr$ideology_scale[data_tr$ideology=="Extremely liberal"] <- -3
data_tr$ideology_scale[data_tr$ideology=="Liberal"] <- -2
data_tr$ideology_scale[data_tr$ideology=="Slightly liberal"] <- -1
data_tr$ideology_scale[data_tr$ideology=="Moderate, middle of the road"] <- 0
data_tr$ideology_scale[data_tr$ideology=="Slightly conservative"] <- 1
data_tr$ideology_scale[data_tr$ideology=="Conservative"] <- 2
data_tr$ideology_scale[data_tr$ideology=="Extremely conservative"] <- 3
data_tr$ideology_scale[data_tr$ideology==""] <- NA

#gender (male w/ female/other as reference)
data_tr$male <- ifelse(data_tr$gender=="Male",1,0)

#treatment dummy variables 
data_tr$dom_ret <- ifelse(data_tr$treatment_condition==1,1,0)
data_tr$dom_loss <- ifelse(data_tr$treatment_condition==2,1,0)
data_tr$int_ret <- ifelse(data_tr$treatment_condition==3,1,0)
data_tr$int_loss <- ifelse(data_tr$treatment_condition==4,1,0)

#object for conjoint analysis
meta<-dplyr::select(data_tr,id,party_order,rep,white_nonhisp,
                    treatment_condition,manip_pass,dem,
                    attn2_pass)

cats<-c("agency","strategy","budget","immig","ht","dt","terror")

#Separate data_tr sets for cases where people didn't finish all five
data_tr1 <- data_tr[!(data_tr$traits1a==""),]
data_tr2 <- data_tr[!(data_tr$traits2a==""),]
data_tr3 <- data_tr[!(data_tr$traits3a==""),]
data_tr4 <- data_tr[!(data_tr$traits4a==""),]
data_tr5 <- data_tr[!(data_tr$traits5a==""),]

meta1 <- meta[!(data_tr$traits1a==""),]
meta2 <- meta[!(data_tr$traits2a==""),]
meta3 <- meta[!(data_tr$traits3a==""),]
meta4 <- meta[!(data_tr$traits4a==""),]
meta5 <- meta[!(data_tr$traits5a==""),]

#Choice 1
c1a<-do.call(rbind.data.frame, strsplit(data_tr1[["traits1a"]], "\\|"))
names(c1a)<-cats
c1b<-do.call(rbind.data.frame, strsplit(data_tr1[["traits1b"]], "\\|"))
names(c1b)<-cats

#Choice 2
c2a<-do.call(rbind.data.frame, strsplit(data_tr2[["traits2a"]], "\\|"))
names(c2a)<-cats
c2b<-do.call(rbind.data.frame, strsplit(data_tr2[["traits2b"]], "\\|"))
names(c2b)<-cats

#Choice 3
c3a<-do.call(rbind.data.frame, strsplit(data_tr3[["traits3a"]], "\\|"))
names(c3a)<-cats
c3b<-do.call(rbind.data.frame, strsplit(data_tr3[["traits3b"]], "\\|"))
names(c3b)<-cats

#Choice 4
c4a<-do.call(rbind.data.frame, strsplit(data_tr4[["traits4a"]], "\\|"))
names(c4a)<-cats
c4b<-do.call(rbind.data.frame, strsplit(data_tr4[["traits4b"]], "\\|"))
names(c4b)<-cats

#Choice 5
c5a<-do.call(rbind.data.frame, strsplit(data_tr5[["traits5a"]], "\\|"))
names(c5a)<-cats
c5b<-do.call(rbind.data.frame, strsplit(data_tr5[["traits5b"]], "\\|"))
names(c5b)<-cats

#Choices
c1a$chosen<-ifelse(data_tr1$policy_preference1=="Policy A",1,0)
c1b$chosen<-ifelse(data_tr1$policy_preference1=="Policy A",0,1)
c1a$chosen[data_tr1$policy_preference1==""] <- NA
c1b$chosen[data_tr1$policy_preference1==""] <- NA

c2a$chosen<-ifelse(data_tr2$policy_preference2=="Policy A",1,0)
c2b$chosen<-ifelse(data_tr2$policy_preference2=="Policy A",0,1)
c2a$chosen[data_tr2$policy_preference2==""] <- NA
c2b$chosen[data_tr2$policy_preference2==""] <- NA

c3a$chosen<-ifelse(data_tr3$policy_preference3=="Policy A",1,0)
c3b$chosen<-ifelse(data_tr3$policy_preference3=="Policy A",0,1)
c3a$chosen[data_tr3$policy_preference3==""] <- NA
c3b$chosen[data_tr3$policy_preference3==""] <- NA

c4a$chosen<-ifelse(data_tr4$policy_preference4=="Policy A",1,0)
c4b$chosen<-ifelse(data_tr4$policy_preference4=="Policy A",0,1)
c4a$chosen[data_tr4$policy_preference4==""] <- NA
c4b$chosen[data_tr4$policy_preference4==""] <- NA

c5a$chosen<-ifelse(data_tr5$policy_preference5=="Policy A",1,0)
c5b$chosen<-ifelse(data_tr5$policy_preference5=="Policy A",0,1)
c5a$chosen[data_tr5$policy_preference5==""] <- NA
c5b$chosen[data_tr5$policy_preference5==""] <- NA

c1a<-cbind(meta1,c1a)
c1b<-cbind(meta1,c1b)
c2a<-cbind(meta2,c2a)
c2b<-cbind(meta2,c2b)
c3a<-cbind(meta3,c3a)
c3b<-cbind(meta3,c3b)
c4a<-cbind(meta4,c4a)
c4b<-cbind(meta4,c4b)
c5a<-cbind(meta5,c5a)
c5b<-cbind(meta5,c5b)

c1a$task <- 1
c1b$task <- 1
c2a$task <- 2
c2b$task <- 2
c3a$task <- 3
c3b$task <- 3
c4a$task <- 4
c4b$task <- 4
c5a$task <- 5
c5b$task <- 5

c1a$profile <- 1
c2a$profile <- 1
c3a$profile <- 1
c4a$profile <- 1
c5a$profile <- 1

c1b$profile <- 2
c2b$profile <- 2
c3b$profile <- 2
c4b$profile <- 2
c5b$profile <- 2


res_tr<-rbind(c1a,c1b,c2a,c2b,c3a,c3b,c4a,c4b,c5a,c5b)
res_tr<-cj_df(res_tr)

#Labeling
res_tr$agency<-as.factor(res_tr$agency)
res_tr$strategy1<-as.factor(res_tr$strategy)
levels(res_tr$strategy1)
res_tr$strategy<-factor(res_tr$strategy1,levels=levels(res_tr$strategy1)[rev(c(1,3,7,2,5,6,4))])
res_tr$budget<-factor(res_tr$budget,levels=c("0 - 5% increase", "5-10% increase","10-15% increase","15-20% increase","20-40% increase","40-60% increase"))
res_tr$immig<-factor(res_tr$immig,levels=c("0-5% reduction","5-10% reduction","10-20% reduction","20-40% reduction","40-60% reduction","60-80% reduction"))
res_tr$ht[res_tr$ht=="0-5% reduction"]<-" 0-5% reduction"
res_tr$ht[res_tr$ht=="5-10% reduction"]<-" 5-10% reduction"
res_tr$ht[res_tr$ht=="10-20% reduction"]<-" 10-20% reduction"
res_tr$ht[res_tr$ht=="20-40% reduction"]<-" 20-40% reduction"
res_tr$ht[res_tr$ht=="40-60% reduction"]<-" 40-60% reduction"
res_tr$ht[res_tr$ht=="60-80% reduction"]<-" 60-80% reduction"
res_tr$ht<-factor(res_tr$ht,levels=c(" 0-5% reduction",
                                     " 5-10% reduction",
                                     " 10-20% reduction",
                                     " 20-40% reduction",
                                     " 40-60% reduction",
                                     " 60-80% reduction"))

res_tr$dt[res_tr$dt=="0-5% reduction"]<-"  0-5% reduction"
res_tr$dt[res_tr$dt=="5-10% reduction"]<-"  5-10% reduction"
res_tr$dt[res_tr$dt=="10-20% reduction"]<-"  10-20% reduction"
res_tr$dt[res_tr$dt=="20-40% reduction"]<-"  20-40% reduction"
res_tr$dt[res_tr$dt=="40-60% reduction"]<-"  40-60% reduction"
res_tr$dt[res_tr$dt=="60-80% reduction"]<-"  60-80% reduction"
res_tr$dt<-factor(res_tr$dt,levels=c("  0-5% reduction",
                                     "  5-10% reduction",
                                     "  10-20% reduction",
                                     "  20-40% reduction",
                                     "  40-60% reduction",
                                     "  60-80% reduction"))

res_tr$terror[res_tr$terror=="0-5% reduction"]<-"   0-5% reduction"
res_tr$terror[res_tr$terror=="5-10% reduction"]<-"   5-10% reduction"
res_tr$terror[res_tr$terror=="10-20% reduction"]<-"   10-20% reduction"
res_tr$terror[res_tr$terror=="20-40% reduction"]<-"   20-40% reduction"
res_tr$terror[res_tr$terror=="40-60% reduction"]<-"   40-60% reduction"
res_tr$terror[res_tr$terror=="60-80% reduction"]<-"   60-80% reduction"
res_tr$terror<-factor(res_tr$terror,levels=c("   0-5% reduction",
                                             "   5-10% reduction",
                                             "   10-20% reduction",
                                             "   20-40% reduction",
                                             "   40-60% reduction",
                                             "   60-80% reduction"))
res_tr$agency<- rio::factorize(res_tr$agency)
attr(res_tr$agency, "label") <- "AGENCY"
attr(res_tr$strategy, "label") <- "STRATEGY"
attr(res_tr$budget, "label") <- "BUDGET"
attr(res_tr$immig, "label") <- "EFFECT ON ILLEGAL IMMIGRATION"
attr(res_tr$ht, "label") <- "HUMAN TRAFFICKING"
attr(res_tr$dt, "label") <- "EFFECT ON DRUG TRAFFICKING"
attr(res_tr$terror, "label") <- "EFFECT ON TERRORISM"

res_tr$id <- as.factor(res_tr$id)

####
#Text data, with no screening
####
setwd('~/Dropbox/cm_project2/temp')
text <- read.csv('survey2_full.csv')
text$incomp <- ifelse((text$traits1a=="") | data$Q_RecaptchaScore<=0.5,1,0 )
text <- arrange(text, incomp)
text$id <- seq(1,nrow(text),1)
text <- dplyr::select(text,id,dom_retention,dom_loss,intl_retention,intl_loss)
text <- text[text$dom_retention!="" | 
               text$dom_loss!="" | 
               text$intl_retention!="" | 
               text$intl_loss!="",]

#####
#Subsetting to variables and observations used in analysis
####
data <- dplyr::select(data,id, StartDate, EndDate, gender,
                      education, ideology, milserve, attention_politics, 
                      hispanic, race, income, age, 
                      attention1, policy_preference1, policy_preference2,
                      policy_preference3, policy_preference4, policy_preference5,
                      attention2_block1_1, attention2_block2_1,
                      traits1a, traits1b,
                      traits2a, traits2b, traits3a, traits4a, traits4b, 
                      traits5a, traits5b,  
                      party_order, rep)

data_tr <- dplyr::select(data_tr,id, StartDate, EndDate,
                         gender,
                         education, ideology, milserve, attention_politics, 
                         hispanic, race, income, age, attention1, 
                         policy_preference1, policy_preference2, policy_preference3,
                         policy_preference4, policy_preference5, manipcheck,
                         traits1a, traits1b,
                         traits2a, traits2b, traits3a, traits4a, traits4b, 
                         traits5a, traits5b, openended,
                         attn2_pass, party_order, rep, dem, white_nonhisp, 
                         dom_ret, dom_loss, int_ret, int_loss)


setwd('~/Dropbox/cm_project2/KenwickMaxey_JCR2024_replication/')
save(data, res, file="survey1.RData")
save(data_tr,res_tr, text, file="survey2.RData")


