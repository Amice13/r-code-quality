### Replication File for 
### "Gender and Political Compliance under Authoritarian Rule", Comparative Political Studies.
### Yingjie Fan, Jennifer Pan, Tongtong Zhang

### This file: codes that replicate all tables and figures in the ONLINE APPENDIX.

######################################################################################
## Section A.3 Descriptive statistics of survey participants
######################################################################################
rm(list=ls()) 
## Install and load packages
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("likert")
#install.packages("ggthemes")
#install.packages("splitstackshape")
#install.packages("stringr")
#install.packages("stargazer")
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))
suppressMessages(library(likert))
suppressMessages(library(ggthemes))
suppressMessages(library(splitstackshape))
suppressMessages(library(stringr))
suppressMessages(library(stargazer))

## Please set the working directory to the folder where the ReadMe.txt is located.
setwd("")
d<-read.csv("Data/CI_RespondentDescriptives.csv",header=TRUE, stringsAsFactors = FALSE,sep=",") #Read in the data

###############################
####### Gender #############
###############################
gender<-as.data.frame(d$gender) %>%
  filter(d$gender!="")
gender_p<-ggplot(gender, aes(x='', fill=factor(`d$gender`,levels = c("Men","Women")))) +
  geom_bar(position = "fill")+
  labs(x="",y = "", 
       title = "Gender",
       fill="")+ 
  scale_fill_grey()+
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_gender.pdf",gender_p,height=2,width=7)

###################################################
####### Prior Teaching Experience #############
###################################################
teacher_yr<-as.data.frame(d$teacher_yr) 
teaching_yr_p<-ggplot(teacher_yr, aes(x='', fill=factor(`d$teacher_yr`,levels = c("4+ yrs","1-4 yrs","0-1 yr")))) +geom_bar(position = "fill")+
  labs(x="",y = "", 
       title = "Prior Teaching Experience",
       fill="")+ 
  scale_color_grey()+
  coord_flip()+
  scale_fill_grey()+
  theme_minimal()
ggsave("Log/A3_PriorTeaching.pdf",teaching_yr_p,height=2,width=7)

###################################################
####### CCP Membership #############
###################################################
ccp<-as.data.frame(d$ccp) %>%
  filter(d$ccp!="Missing")
ccp_p<-ggplot(ccp, aes(x='', fill=factor(`d$ccp`,levels = c("Yes","No","Decline to answer")))) +
  geom_bar(position = "fill")+
  labs(x="",y = "", 
       title = "CCP Membership",
       fill="")+ 
  scale_fill_grey()+
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_ccp.pdf",ccp_p,height=2,width=7)

##################################################
####### Education #############
###################################################
degree<-as.data.frame(d$educ)%>%
  filter(d$educ!="Missing")
edu_p<-ggplot(degree, aes(x='', fill=factor(`d$educ`,levels = c("Graduate","Bachelor","Less than Bachelor")))) +
  geom_bar(position = "fill")+
  labs(x="",y = "", 
       title = "Education",
       fill="")+ 
  scale_fill_grey()+
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_education.pdf",edu_p,height=2,width=7)

##################################################
####### CI Experience #############
###################################################
ci_year<-as.data.frame(d$ci_years)%>%
  filter(d$ci_years!="Missing")
ci_year_p<-ggplot(ci_year, aes(x='', fill=factor(`d$ci_years`,levels = c("> 2 yrs","2 yrs","1 yr","< 1 yr")))) +
  geom_bar(position = "fill")+
  labs(x="",y = "", 
       title = "CI Experience",
       fill="")+ 
  scale_fill_grey()+
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_CIExperience.pdf",ci_year_p,height=2,width=8)


##################################################
####### Region of host country #############
###################################################
country<-as.data.frame(d$country)
country$`d$country`<-as.character(country$`d$country`)
L <- list(`North America`=c("Bermuda","Canada","US","Guam"),
          `South America`=c("Costa Rica","El Salvador","Mexico","Argentina","Brazil","Chile",
                            "Colombia","Ecuador","Guyana","Uruguay","Dominica","Aruba"),
          `East, South, and Southeast Asia`=c("Mongolia","Korea","Japan","Hong Kong","India","Afghanistan",
                                              "Pakistan","Cambodia","Indonesia","Laos","Philippines","Thailand",
                                              "Vietnam"," Brunei"),
          `East Europe and Central Asia`=c("Belarus","Bulgaria","Czech","Romania","Russia","Ukraine","Estonia",
                                           "Lithuania","Croatia","Armenia","Tajikistan","Uzbekistan","Kazakhstan"),
          `West Europe`=c("British","British Virgin Islands","Spain","Portugal","France",
                          "Belgium","Germany","Austria","Italy","Greece","Malta","Norway",
                          "Finland","Denmark","French Polynesia","Mayo","Faroe Islands","Anguilla"),
          Ocenia=c("Australia","New Zealand"),
          Africa=c("Egypt","Burundi","Ethiopia","Tanzania","Zambia","Cameroon","Central African Republic",
                   "Chad","Congo","Comoros","Gabon","Cape Verde","Ghana","Nigeria"))
region <- stack(L)
names(region) <- c("Country", "Region")
country<-dplyr::inner_join(country,region,by=c("d$country"="Country"))
country_d<-as.data.frame(table(country$Region))
region_p<-ggplot(country, aes(x='', fill=Region)) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Region of host country",
       fill="")+scale_fill_grey() +
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_RegionHostCountry.pdf",region_p,height=2,width=7)

##################################################
####### Length of Hanban Training #############
###################################################
training<-as.data.frame(d$HanbanTrainingLength)
training_p<-ggplot(training, aes(x='', fill=factor(`d$HanbanTrainingLength`,levels=c( "> 1 month","1 month", "<1 month","Other","Missing")))) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Length of Hanban Training",
       fill="")+ coord_flip()+theme_minimal()+scale_fill_grey()
ggsave("Log/A3_LengthHanbanTrain.pdf",training_p,height=2,width=7)

##################################################
####### Motivation for joining CI #############
###################################################
ci_reason<-as.data.frame(d$motivation)
ci_reason[ci_reason==""] <- "Missing"
n_ci_reason<-length(ci_reason$`d$motivation`) #number of people who answered this question
suppressWarnings(ci_reason<-cSplit(ci_reason,"d$motivation","," , direction = "long") %>%
  dplyr::group_by(`d$motivation`) %>%
  dplyr::summarise(n=n(),pct=n/n_ci_reason))
ci_reason_p<-ggplot(ci_reason, aes(x=factor(`d$motivation`,levels = c("Missing", "Other","Broden horizon for myself","Arranged by my work unit or school in China","Help my career after returning to China")), y=pct)) + 
  geom_col()+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Motivation for Joining CI",
       fill="")+
  ylim(0,1) +
  coord_flip()+
  scale_fill_grey()+
  theme_minimal()
ggsave("Log/A3_motivation.pdf",ci_reason_p,height=2,width=7)

########################################################
####### Perceived mission of CI teachers #############
#########################################################
n_mission<-length(d$mission) #number of people who answered this question
mission<-as.data.frame(d$mission)
suppressWarnings(mission<-cSplit(mission,"d$mission","," ,direction = "long")%>%
  dplyr::group_by(`d$mission`)%>%
  dplyr::summarise(n=n(),pct=n/n_mission))
mission_p<-ggplot(mission, aes(x=factor(`d$mission`,levels = c("Missing","Other","Help foreigners to learn Mandarin","Help foreigners to learn Chinese traditional culture","Learn about the strengths of foreign education system","Attract foreigners to study and work in China","Reduce foreigners' political misunderstanding against China")), y=pct)) + 
  geom_col()+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Perceived mission of CI teachers",
       fill="")+
  ylim(0,1)+
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_PerceivedMission.pdf",mission_p,height=2,width=7)

########################################################
####### Mainly used textbook #############
#########################################################
textbook<-as.data.frame(d$textbook_source)
textbook_p<-ggplot(textbook, aes(x='', fill=factor(`d$textbook_source`,levels = c("Provided by Hanban","Provided by local schools","Materials prepared by myself","Other","Missing")))) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "The main textbook you use at school is:",
       fill="")+ scale_fill_grey() +
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_textbook.pdf",textbook_p,height=2,width=7)


########################################################
####### Why choose this textbook #############
#########################################################
textbook_reason<-as.data.frame(d$textbook_reason)
n_textbook_reason<-length(textbook_reason$`d$textbook_reason`) #number of people who answered this question
suppressWarnings(textbook_reason<-cSplit(textbook_reason,"d$textbook_reason","," ,direction = "long")%>%
  dplyr::group_by(`d$textbook_reason`)%>%
  dplyr::summarise(n=n(),pct=n/n_textbook_reason))
textbook_reason_p<-ggplot(textbook_reason, aes(x=factor(`d$textbook_reason`,levels = c("Missing","Other","Used by previous teacher","My own choice","Requested by local schools","Requested by Hanban")), y=pct)) + 
  geom_col()+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Why do you choose this textbook?",
       fill="")+ 
  scale_fill_grey() +
  ylim(0,0.5)+
  coord_flip()
ggsave("Log/A3_TextbookReason.pdf",textbook_reason_p,height=2,width=7)

########################################################
####### Teaching focus at CI #############
#########################################################
teaching_focus<-as.data.frame(d$teaching_focus)
teaching_focus_p<-ggplot(teaching_focus, aes(x='', fill=factor(`d$teaching_focus`,levels = c("Mandarin","Chinese history and culture","Not in charge of teaching","Other")))) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Main teaching focus at CI",
       fill="")+scale_fill_grey()+coord_flip()+
  theme_minimal()
ggsave("Log/A3_TeachingFocus.pdf",teaching_focus_p,height=2,width=8)

########################################################
####### Student composition #############
#########################################################
student<-as.data.frame(d$student_composition)
n_student<-length(student$`d$student_composition`) #number of people who answered this question
suppressWarnings(student<-cSplit(student,"d$student_composition","," , direction = "long") %>%
  dplyr::group_by(`d$student_composition`) %>%
  dplyr::summarise(n=n(),pct=n/n_student))
student_p<-ggplot(student, aes(y=pct, x=factor(`d$student_composition`,levels=c("Missing","General public","Primary school students", "Middle school students","High school students","College students")))) +   geom_col()+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Student Composition",
       fill="")+scale_fill_grey() +
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_StudentComposition.pdf",student_p,height=2,width=7)

########################################################
####### Lead class independently #############
#########################################################
teaching_independence<-d[,c("TeachInd_college","TeachInd_hs","TeachInd_ms","TeachInd_primary","TeachInd_Kid","TeachInd_public")]
colnames(teaching_independence)<-c("College students","High school students","Middle school students","Primary school students","Kindergarteners","General public")
for (val in names(teaching_independence)) {
  teaching_independence[,val] <- factor(teaching_independence[,val], 
                                        labels = c("co-teach with local teachers always","teach both types of classes","teach independently always"),
                                        levels =c("我和当地老师联合带班","我的班级两种情况均有","我独立带班"),ordered=TRUE)}
index<-which(is.na(teaching_independence[,4]) & (!is.na(teaching_independence[,5])))
teaching_independence[index,4]<-teaching_independence[index,5]
index<-which((!is.na(teaching_independence[,5])) & (teaching_independence[,4]!=teaching_independence[,5]))
teaching_independence[index,4]<-"teach both types of classes"
teaching_independence<-teaching_independence[,-5]
png(file="Log/A3_LeadClassIndependently.png",width = 800,height=240,units = "px")
plot(likert(teaching_independence), "bar", 
     ordered = FALSE,
     group.order=c("College students","High school students","Middle school students","Primary school students","General public")) +
  ggtitle("For each of your student group at local school, do you lead all classes independently?")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom", 
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey()
dev.off()

########################################################
####### Courses count into GPA #############
#########################################################
gpa<-d[,c("GPA_college","GPA_hs","GPA_ms","GPA_primary","GPA_kid","GPA_public")]
colnames(gpa)<-c("College students","High school students","Middle school students","Primary school students","Kindergarten kids","General public")
for (val in names(gpa)) {
  gpa[,val] <- factor(gpa[,val], 
                      labels = c("All course count into GPA","Certain courses count into GPA","No course count into GPA","Not applicable"),
                      levels =c("算入学分/\n总成绩","有些课算\n有些不算","不算入学分/\n总成绩","不适用"))}
for (val in names(gpa)) {
  gpa[,val] <- factor(gpa[,val], levels=c("All course count into GPA","Certain courses count into GPA","No course count into GPA","Not applicable"))
}         
index<-which(is.na(gpa[,4]) & (!is.na(gpa[,5])))
gpa[index,4]<-gpa[index,5]
index<-which((!is.na(gpa[,5])) & (gpa[,4]!=gpa[,5]))
gpa[index,4]<-"Certain courses count into GPA"; gpa<-gpa[,-5]
png(file="Log/A3_CountGPA.png",width = 800,height=240,units = "px")
plot(likert(gpa), "bar", ordered = F,group.order=c("College students","High school students","Middle school students","Primary school students","General public")) +
  ggtitle("For each of your student group at local school, does your course count into their GPA/grades?")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom", 
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey()
dev.off()

########################################################
####### Students of Chinese descent #############
#########################################################
race <-d[,c("student_race1","student_race2","student_race3")] %>%
  gather(key,value,student_race1:student_race3)
race$Chinese<-ifelse(race$value %in% c("Chinese", "中国", "中国人", "中国华侨", "华人","华人华侨","华族","华裔","汉族"), 1, 0)
race$Chinese[race$Chinese==1]<-"Yes"
race$Chinese[race$Chinese==0]<-"No"
race_p<-ggplot(race, aes(x='', fill=factor(Chinese,levels = c("Yes","No")))) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Does the class you teach have students of Chinese descent?",
       fill="")+   
  scale_fill_grey()+
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_StudentChineseDescent.pdf",race_p,height=2,width=7)

####################################################################
####### Frequency of interacting with local teachers #############
###################################################################
local_teacher_interaction<-as.data.frame(d$freq_interact_localcolleague)
local_teacher_interaction_p<-ggplot(local_teacher_interaction, aes(x='', fill=factor(`d$freq_interact_localcolleague`,levels = c("Daily","Weekly","Monthly","Missing")))) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "Frequency of interaction with local teachers",
       fill="")+ scale_fill_grey() +
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_LocalTeacherInteract.pdf",local_teacher_interaction_p,height=2,width=7)

########################################################
####### Perceptions on host country #############
#########################################################
host_statement <-d[,c("HostStatement1","HostStatement2","HostStatement3","HostStatement4","HostStatement5","HostStatement6","HostStatement7","HostStatement8")] %>%
  gather(statement_n,statement,HostStatement1:HostStatement8)
host_statement_value<-d[,c("HostValue1","HostValue2","HostValue3","HostValue4","HostValue5","HostValue6","HostValue7","HostValue8")] %>%
  gather(value_n,value,HostValue1:HostValue8)  
Statement<-as.data.frame(cbind(host_statement$statement,host_statement_value$value))
colnames(Statement)<-c("statement","value")
Statement <-as.data.frame(Statement)
Statement$value<-as.factor(Statement$value)
Statement <- Statement %>% 
  dplyr::group_by(statement) %>% 
  dplyr::mutate(grouped_id = row_number())
Statement <-Statement %>% 
  spread(statement, value) %>% 
  dplyr::select(-grouped_id)
Statement <-as.data.frame(Statement)
colnames(Statement)<-c("Local students love asking questions in class.",
                       "Local students do not closely follow teachers' instruction.",
                       "Local students are very creative.",
                       "Local students are not self-disciplined in their studies.",
                       "In general, the local community is well informed about current conditions of everyday life among people living in China.",
                       "In general, the local community has a strong interest in traditional Chinese culture.",
                       "In general, the local community has some misunderstandings of China’s political claims.",
                       "In general, the local community has some misunderstandings of the Confucius Institute.")
for (val in names(Statement)) {
  Statement[,val] <- factor(Statement[,val], 
                            labels =c("0-4","0-4","0-4","0-4","0-4","5-7","5-7","5-7","8-10","8-10","8-10"),
                            levels =c(0,1,2,3,4,5,6,7,8,9,10))}
png(file="Log/A3_PerceptionHostCountry.png",width = 800,height=600,units = "px")
plot(likert(Statement), "bar", ordered = F) +
  labs(title = "Perceptions on host country",
       subtitle = "0=Strongly Disagree; 10=Strongly Agree")+
  theme(axis.text.y = element_text(size=15),
        legend.title = element_blank(), 
        plot.title = element_text(size=18)) +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey() 
dev.off()

########################################################
####### Consumption of Chinese media #############
#########################################################
chn_media_freq<-as.data.frame(d[,c("ChineseSocialMedia","ChineseNewsWebsites","ChineseTVStations","ChineseNewspaper.magazine","Other")])
colnames(chn_media_freq)<-c("Chinese social media","Chinese news websites",
                            "Chinese TV stations","Chinese newspaper/magazine",
                            "Other")
for (val in names(chn_media_freq)) {
  chn_media_freq[,val] <- factor(chn_media_freq[,val], 
                                 labels = c("Almost never","Several times a month","Several times a week","Several times each day"),
                                 levels =c("几乎不看","每月几次","每周几次","每天几次"))}
png(file="Log/A3_ChineseMediaConsumption.png",width = 800,height=240,units = "px")
plot(likert(chn_media_freq), "bar", label.missing = "Missing",ordered = F, group.order = c("Chinese social media","Chinese news websites","Chinese TV stations","Chinese newspaper/magazine","Other"))+
  ggtitle("Consumption of Chinese media while abroad")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom", 
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey()
dev.off()

########################################################
####### Consumption of host-country media #############
#########################################################
local_media_freq<-d[,c("LocalSocialMedia","LocalNewsWebsites","LocalTVStations","LocalRadio","LocalNewspaper.magazine","Other")]
colnames(local_media_freq)<-c("Local social media","Local news websites","Local TV stations","Local radio","Local newspaper/magazine", "Other")
for (val in names(local_media_freq)) {
  local_media_freq[,val] <- factor(local_media_freq[,val], 
                                   labels = c("Almost never","Several times a month","Several times a week","Several times each day"),
                                   levels =c("几乎不看","每月几次","每周几次","每天几次"))}
png(file="Log/A3_HostCountryMediaConsumption.png",width = 800,height=240,units = "px")
plot(likert(local_media_freq), "bar", label.missing = "Missing",ordered = F,group.order = c("Local social media","Local news websites","Local TV stations","Local radio","Local newspaper/magazine", "Other"))+
  ggtitle("Consumption of host-country media while abroad")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom", 
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey()
dev.off()

####################################################################
####### Consumption of news/current affairs through Chinese media 
###################################################################
current_affair<-d[,c("news_ChineseSocialMedia","news_ChineseNewsWebsites","news_ChineseTVStations","news_ChineseNewspaper.magazine","news_Other")]
colnames(current_affair)<-c("Chinese social media","Chinese news websites","Chinese TV stations","Chinese newspaper/magazine","Other")
for (val in names(current_affair)) {
  current_affair[,val] <- factor(current_affair[,val], 
                                 labels = c("Never","Occasionally","Sometimes","Usually"),
                                 levels =c("从不","偶尔","有时","经常"))}
png(file="Log/A3_ChineseMediaConsumption_News.png",width = 800,height=240,units = "px")
plot(likert(current_affair), "bar", label.missing = "Missing",ordered = F,group.order = c("Chinese social media","Chinese news websites",
                                                                                          "Chinese TV stations","Chinese newspaper/magazine",
                                                                                          "Other"))+
  ggtitle("Consumption of news through Chinese media while abroad") +
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom", 
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey()
dev.off()

#######################################################################
####### Consumption of news/current affairs through host-country media 
########################################################################
local_media_freq_politics<-d[,c("news_LocalSocialMedia","news_LocalNewsWebsites","news_LocalTVStations","news_LocalRadio","news_LocalNewspaper.magazine","news_Other")]
colnames(local_media_freq_politics)<-c("Local social media","Local news websites","Local TV stations","Local radio","Local newspaper/magazine","Other")
for (val in names(local_media_freq_politics)) {
  local_media_freq_politics[,val] <- factor(local_media_freq_politics[,val], 
                                            labels = c("Never","Occasionally","Sometimes","Usually"),
                                            levels =c("从不","偶尔","有时","经常"))}

png(file="Log/A3_HostCountryMediaConsumption_News.png",width = 800,height=240,units = "px")
plot(likert(local_media_freq_politics), "bar", label.missing = "Missing",ordered = F,group.order = c("Local social media","Local news websites",
                                                                                                     "Local TV stations","Local radio","Local newspaper/magazine",
                                                                                                     "Other"))+
  ggtitle("Consumption of news through host-country media while abroad")+
  theme(axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12), 
        legend.text=element_text(size=12),
        plot.title = element_text(size=12),
        legend.title = element_blank(),
        legend.position="bottom", 
        legend.box = "vertical") +
  guides(fill = guide_legend(nrow = 1))+scale_fill_grey()
dev.off()

#######################################################################
####### How friendly are host-country media to China #######
########################################################################
local_media_friendly<-as.data.frame(d$LocalMediaFriendliness)
local_media_friendly_p<-ggplot(local_media_friendly, aes(x='', fill=factor(`d$LocalMediaFriendliness`,levels=c("8-10","5-7","0-4","Missing")))) + geom_bar(position = "fill")+
  labs(x="",y = "Proportion of CI Teachers", 
       title = "How friendly do you feel the media in your host country is towards China?\n(0 = very unfriendly, 10 = very friendly)",
       fill="")+ scale_fill_grey() +
  coord_flip()+
  theme_minimal()
ggsave("Log/A3_HostCountryMediaFriendliness.pdf",local_media_friendly_p,height=2,width=7)


#########################
### Table A1 ###
#########################
rm(list=ls()) 
# Install and load packages
#install.packages("sandwich")
#install.packages("plm")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages("lmtest")
#install.packages("multiwayvcov")
#install.packages("mfx")
suppressMessages(library("sandwich"))
suppressMessages(library(plm))
suppressMessages(library(dplyr))
suppressMessages(library(plyr))
suppressMessages(library(lmtest))
suppressMessages(library(multiwayvcov))
suppressMessages(library(mfx))

d<-read.csv("Data/CI_MainExperiment.csv",header=TRUE, stringsAsFactors = FALSE,sep=",") #Read in the data

#########################
### Table A1 ###
#########################
d<-d[!is.na(d$SelfCensor),];dd<-d[1:284,] #unique respondents
Table1<-matrix(NA, nrow=3,ncol=4)
colnames(Table1)<-c("","US respondents","Non-US respondents","p-value")
Table1[,1]<-c("Join CI for career improvement",
              "Believe CI has a political mission",
              "Use textbooks requested by Hanban")
# Join CI for career improvement
Table1[1,2]<-round(mean(dd$JoinMotivations_material[dd$US==1],na.rm=T), digits=3)
Table1[1,3]<-round(mean(dd$JoinMotivations_material[dd$US==0],na.rm=T), digits=3)
Table1[1,4]<-round(t.test(dd$JoinMotivations_material[dd$US==1],dd$JoinMotivations_material[dd$US==0])$p.value, digits=3)
# Believe CI has a political mission
Table1[2,2]<-round(mean(dd$mission_misunderstanding[dd$US==1],na.rm=T), digits=3)
Table1[2,3]<-round(mean(dd$mission_misunderstanding[dd$US==0],na.rm=T), digits=3)
Table1[2,4]<-round(t.test(dd$mission_misunderstanding[dd$US==1],dd$mission_misunderstanding[dd$US==0])$p.value, digits=3)
# Use textbooks requested by Hanban
Table1[3,2]<-round(mean(dd$textbook_HanbanRequire[dd$US==1],na.rm=T), digits=3)
Table1[3,3]<-round(mean(dd$textbook_HanbanRequire[dd$US==0],na.rm=T), digits=3)
Table1[3,4]<-round(t.test(dd$textbook_HanbanRequire[dd$US==1],dd$textbook_HanbanRequire[dd$US==0])$p.value, digits=3)
# Write the table
TableA1_final<-stargazer(Table1,title="Table A1: Comparing US respondents vs. non-US respondents on Descriptives",
                        type="text",summary=FALSE)
write.table(TableA1_final, file="Log/TableA1.txt", row.names = FALSE, quote=FALSE)

######################################################################################
## Section A.5 Details of Overall Treatment Effects
######################################################################################

#########################
### Table A2 ###
#########################
rm(TableA1_final,Table1)
# Subset into three experimental conditions
dcontrol<-dd[dd$control==1,]
dPRC<-dd[dd$PRC==1,]
dInterpersonal<-dd[dd$interpersonal==1,]
# Means for the control condition
Control<-c(mean(dcontrol$ProfTeacher,na.rm=T),round(mean(dcontrol$age,na.rm=T),digits=1),
           mean(dcontrol$female,na.rm=T),mean(dcontrol$ccp,na.rm=T),mean(dcontrol$Grad,na.rm=T),
           mean(dcontrol$CIYears_num,na.rm=T),mean(dcontrol$TeacherBefore,na.rm=T),mean(dcontrol$TrainingOverMonth,na.rm=T),
           mean(dcontrol$LocalMediaFriend,na.rm=T))
# Means for the Objectives prime treatment condition
Objectives<-c(mean(dPRC$ProfTeacher,na.rm=T),round(mean(dPRC$age,na.rm=T),digits=1),
       mean(dPRC$female,na.rm=T),mean(dPRC$ccp,na.rm=T),mean(dPRC$Grad,na.rm=T),
       mean(dPRC$CIYears_num,na.rm=T),mean(dPRC$TeacherBefore,na.rm=T),mean(dPRC$TrainingOverMonth,na.rm=T),
       mean(dPRC$LocalMediaFriend,na.rm=T))
# Means for the Social prime treatment condition
Social<-c(mean(dInterpersonal$ProfTeacher,na.rm=T),round(mean(dInterpersonal$age,na.rm=T),digits=1),
                 mean(dInterpersonal$female,na.rm=T),mean(dInterpersonal$ccp,na.rm=T),mean(dInterpersonal$Grad,na.rm=T),
                 mean(dInterpersonal$CIYears_num,na.rm=T),mean(dInterpersonal$TeacherBefore,na.rm=T),mean(dInterpersonal$TrainingOverMonth,na.rm=T),
                 mean(dInterpersonal$LocalMediaFriend,na.rm=T))
# P-value from the F test with both treatment dummies
p_ProfTeacher<-pf(summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[2],
                  summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_age<-pf(summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[2],
          summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_female<-pf(summary(lm(female~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(female~PRC+interpersonal,data=dd))$fstatistic[2],
             summary(lm(female~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_ccp<-pf(summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[2],
          summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_Grad<-pf(summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[2],
           summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_CIYears_num<-pf(summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[2],
                  summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_TeacherBefore<-pf(summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[2],
                    summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_TrainingOverMonth<-pf(summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[2],
                        summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_LocalMediaFriend<-pf(summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[2],
                       summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
obs<-rep(284,9)
pvalue<-c(p_ProfTeacher, p_age,p_female,p_ccp,p_Grad,p_CIYears_num,p_TeacherBefore,p_TrainingOverMonth,p_LocalMediaFriend)
# Write the table
balance<-cbind.data.frame(obs,Control, Objectives, Social,pvalue)
colnames(balance)<-c("# Respondents", "Control", "Objectives", "Social", "p-value")
rownames(balance)<-c("Seniority in CI",  "Age", "Female","CCP member", "Graduate degree", "Years at CI",
                     "Teaching exp before CI (Y/N)", "Hanban training over 1 month", 
                     "Perceived friendliness of host-country media on China")
TableA2_final<-stargazer(balance,title="Table A2: Covariate Balance across Treatment Conditions",
                        type="text",summary=FALSE)
write.table(TableA2_final, file="Log/TableA2.txt", row.names = FALSE, quote=FALSE)


#########################
### Table A3 ###
#########################
rm(balance,dcontrol,dPRC,dInterpersonal,dd,pvalue,Social,Control,Objectives, obs)
rm(TableA2_final,p_age,p_ccp,p_CIYears_num,p_female,p_Grad,p_LocalMediaFriend,p_ProfTeacher,p_TeacherBefore,p_TrainingOverMonth)
Remind<-d[d$Reminder==1,]
NoRemind<-d[d$Reminder==0,]
Table3<-matrix(NA, nrow=12,ncol=4)
colnames(Table3)<-c("","Reminder","No reminder","p-value")
Table3[,1]<-c("Overall","Self-censor", "One-sided","Two-sided","Open discussion","Observations",
              "Women","Self-censor", "One-sided","Two-sided","Open discussion","Observations")
# Self-censor(overall, women)
Table3[2,2]<-round(t.test(Remind$SelfCensor,NoRemind$SelfCensor)$estimate[1], digits=3)
Table3[2,3]<-round(t.test(Remind$SelfCensor,NoRemind$SelfCensor)$estimate[2], digits=3)
Table3[2,4]<-round(t.test(Remind$SelfCensor,NoRemind$SelfCensor)$p.value, digits=3)
Table3[8,2]<-round(t.test(Remind$SelfCensor[Remind$female==1],NoRemind$SelfCensor[NoRemind$female==1])$estimate[1], digits=3)
Table3[8,3]<-round(t.test(Remind$SelfCensor[Remind$female==1],NoRemind$SelfCensor[NoRemind$female==1])$estimate[2], digits=3)
Table3[8,4]<-round(t.test(Remind$SelfCensor[Remind$female==1],NoRemind$SelfCensor[NoRemind$female==1])$p.value, digits=3)
# One-sided(overall, women)
Table3[3,2]<-round(t.test(Remind$PosTaking1,NoRemind$PosTaking1)$estimate[1], digits=3)
Table3[3,3]<-round(t.test(Remind$PosTaking1,NoRemind$PosTaking1)$estimate[2], digits=3)
Table3[3,4]<-round(t.test(Remind$PosTaking1,NoRemind$PosTaking1)$p.value, digits=3)
Table3[9,2]<-round(t.test(Remind$PosTaking1[Remind$female==1],NoRemind$PosTaking1[NoRemind$female==1])$estimate[1], digits=3)
Table3[9,3]<-round(t.test(Remind$PosTaking1[Remind$female==1],NoRemind$PosTaking1[NoRemind$female==1])$estimate[2], digits=3)
Table3[9,4]<-round(t.test(Remind$PosTaking1[Remind$female==1],NoRemind$PosTaking1[NoRemind$female==1])$p.value, digits=3)
# Two-sided(overall, women)
Table3[4,2]<-round(t.test(Remind$PosTaking2,NoRemind$PosTaking2)$estimate[1], digits=3)
Table3[4,3]<-round(t.test(Remind$PosTaking2,NoRemind$PosTaking2)$estimate[2], digits=3)
Table3[4,4]<-round(t.test(Remind$PosTaking2,NoRemind$PosTaking2)$p.value, digits=3)
Table3[10,2]<-round(t.test(Remind$PosTaking2[Remind$female==1],NoRemind$PosTaking2[NoRemind$female==1])$estimate[1], digits=3)
Table3[10,3]<-round(t.test(Remind$PosTaking2[Remind$female==1],NoRemind$PosTaking2[NoRemind$female==1])$estimate[2], digits=3)
Table3[10,4]<-round(t.test(Remind$PosTaking2[Remind$female==1],NoRemind$PosTaking2[NoRemind$female==1])$p.value, digits=3)
# Open discussion(overall, women)
Table3[5,2]<-round(t.test(Remind$open,NoRemind$open)$estimate[1], digits=3)
Table3[5,3]<-round(t.test(Remind$open,NoRemind$open)$estimate[2], digits=3)
Table3[5,4]<-round(t.test(Remind$open,NoRemind$open)$p.value, digits=3)
Table3[11,2]<-round(t.test(Remind$open[Remind$female==1],NoRemind$open[NoRemind$female==1])$estimate[1], digits=3)
Table3[11,3]<-round(t.test(Remind$open[Remind$female==1],NoRemind$open[NoRemind$female==1])$estimate[2], digits=3)
Table3[11,4]<-round(t.test(Remind$open[Remind$female==1],NoRemind$open[NoRemind$female==1])$p.value, digits=3)
# Observations(overall, women)
Table3[6,2]<-nrow(Remind)
Table3[6,3]<-nrow(NoRemind)
Table3[12,2]<-sum(Remind$female)
Table3[12,3]<-sum(NoRemind$female)
# Write the table
TableA3_final<-stargazer(Table3,title="Table A3: Difference in Responses between Reminder of Confidentiality and No Reminder",
                        type="text",summary=FALSE)
write.table(TableA3_final, file="Log/TableA3.txt", row.names = FALSE, quote=FALSE)


#########################
### Table A4 ###
#########################
rm(TableA3_final,Table3)
rm(NoRemind,Remind)
Table4<-matrix(NA, nrow=5,ncol=4)
colnames(Table4)<-c("","US respondents","Non-US respondents","p-value")
Table4[,1]<-c("Self-censor", "One-sided","Two-sided","Open discussion","Observations")
US<-d[(d$control==1 & d$US==1),] #US respondents in control
NonUS<-d[(d$control==1 & d$US==0),] #Non-US respondents in control
# Self-censor
Table4[1,2]<-round(t.test(US$SelfCensor,NonUS$SelfCensor)$estimate[1], digits=3)
Table4[1,3]<-round(t.test(US$SelfCensor,NonUS$SelfCensor)$estimate[2], digits=3)
Table4[1,4]<-round(t.test(US$SelfCensor,NonUS$SelfCensor)$p.value, digits=3)
# One-sided
Table4[2,2]<-round(t.test(US$PosTaking1,NonUS$PosTaking1)$estimate[1], digits=3)
Table4[2,3]<-round(t.test(US$PosTaking1,NonUS$PosTaking1)$estimate[2], digits=3)
Table4[2,4]<-round(t.test(US$PosTaking1,NonUS$PosTaking1)$p.value, digits=3)
# Two-sided
Table4[3,2]<-round(t.test(US$PosTaking2,NonUS$PosTaking2)$estimate[1], digits=3)
Table4[3,3]<-round(t.test(US$PosTaking2,NonUS$PosTaking2)$estimate[2], digits=3)
Table4[3,4]<-round(t.test(US$PosTaking2,NonUS$PosTaking2)$p.value, digits=3)
# Open discussion
Table4[4,2]<-round(t.test(US$open,NonUS$open)$estimate[1], digits=3)
Table4[4,3]<-round(t.test(US$open,NonUS$open)$estimate[2], digits=3)
Table4[4,4]<-round(t.test(US$open,NonUS$open)$p.value, digits=3)
# Observations
Table4[5,2]<-nrow(US)
Table4[5,3]<-nrow(NonUS)
# Write the table
TableA4_final<-stargazer(Table4,title="Table A4: Difference in Responses between US and non-US respondents in Control",
                        type="text",summary=FALSE)
write.table(TableA4_final, file="Log/TableA4.txt", row.names = FALSE, quote=FALSE)

#########################
### Table A5 ###
#########################
rm(Table4,US,NonUS,TableA4_final)
#Read in all respondents data including those who drop-out before answering the Taiwan vignettes
d<-read.csv("Data/CI_MainExperiment.csv",header=TRUE, stringsAsFactors = FALSE,sep=",")
Table5<-matrix(NA, nrow=2,ncol=6)
colnames(Table5)<-c("","Control","Objectives","Social","Objectives vs. Control", "Objectives vs. Social")
Table5[,1]<-c("Drop-out rate after prime only", "Drop-out rate after prime and the first Taiwan vignette")

# Drop out after treatment prime only - can only test between Objectives vs. Social Primes because there's no reflection question after the prime in control group
ID<-unique(d$ResponseId[d$control==0 & ((!is.na(d$personality)) | (!is.na(d$mission_misunderstanding)))]) #unique respondents that complete the question right before the prime
dd<-data.frame(ID,0); dd[,2:5]<-NA
colnames(dd)[2:5]<-c("dropout_after_prime","control","objectives","social")
for(i in 1:nrow(dd)){
  id<-dd$ID[i]
  dd[i,2:5]<-d[which(d$ResponseId==id)[1], c("dropout_after_prime","control","PRC","interpersonal")]
}
Table5[1,3]<-round(t.test(dd$dropout_after_prime[dd$objectives==1],dd$dropout_after_prime[dd$social==1])$estimate[1],digits=2)
Table5[1,4]<-round(t.test(dd$dropout_after_prime[dd$objectives==1],dd$dropout_after_prime[dd$social==1])$estimate[2],digits=2)
Table5[1,6]<-round(t.test(dd$dropout_after_prime[dd$objectives==1],dd$dropout_after_prime[dd$social==1])$p.value,digits=3)
# Drop out after treatment prime + first Taiwan vignette
ID<-unique(d$ResponseId[(!is.na(d$personality)) | (!is.na(d$mission_misunderstanding))]) #unique respondents that complete the question right before the prime
dd<-data.frame(ID,0);dd[,2:5]<-NA
colnames(dd)[2:5]<-c("dropout_after_prime_vignette","control","objectives","social")
for(i in 1:nrow(dd)){
  id<-dd$ID[i]
  dd[i,2:5]<-d[which(d$ResponseId==id)[1], c("dropout_after_prime_vignette","control","PRC","interpersonal")]
}
Table5[2,2]<-round(t.test(dd$dropout_after_prime_vignette[dd$control==1],dd$dropout_after_prime_vignette[dd$objectives==1])$estimate[1],digits=2)
Table5[2,3]<-round(t.test(dd$dropout_after_prime_vignette[dd$control==1],dd$dropout_after_prime_vignette[dd$objectives==1])$estimate[2],digits=2)
Table5[2,5]<-round(t.test(dd$dropout_after_prime_vignette[dd$control==1],dd$dropout_after_prime_vignette[dd$objectives==1])$p.value, digits=2)
Table5[2,4]<-round(t.test(dd$dropout_after_prime_vignette[dd$social==1],dd$dropout_after_prime_vignette[dd$objectives==1])$estimate[1],digits=2)
Table5[2,6]<-round(t.test(dd$dropout_after_prime_vignette[dd$social==1],dd$dropout_after_prime_vignette[dd$objectives==1])$p.value, digits=2)
# Write the table
TableA5_final<-stargazer(Table5,title="Table A5: Difference in Attrition Rates across Treatments",
                        type="text",summary=FALSE)
write.table(TableA5_final, file="Log/TableA5.txt", row.names = FALSE, quote=FALSE)


#########################
### Table A6 ###
#########################
rm(Table5, TableA5_final, d, dd, ID, i, id)
## Read in the placebo experiment data
Placebo<-read.csv("Data/CI_Placebo.csv",header=TRUE, stringsAsFactors = FALSE,sep=",") 
## Overall panel:
# Column 1: Self-censorship
model1<- glm(SelfCensorPlacebo ~ PRC + interpersonal + ccp +  Grad + female + ProfTeacher + colleague_everyday +TeachYearsBefore0 + Chn_news_freq +  Local_news_freq  + CIYearsOverOne +  current + AgeOver30, family=binomial(), data = Placebo) 
# Column 2: One-sided position taking
model2<- glm(PosTakingPlacebo1 ~ PRC + interpersonal + ccp +  Grad + female + ProfTeacher + colleague_everyday +  TeachYearsBefore0 + Chn_news_freq +  Local_news_freq  + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth + AgeOver30 + misChinahigh, family=binomial(), data = Placebo) 
# Column 3: Two-sided position taking
model3<- glm(PosTakingPlacebo2 ~ PRC + interpersonal + ccp +  Grad + female + ProfTeacher + colleague_everyday + CIYearsOverOne+ JoinMotivations_material + TrainingOverMonth + AgeOver30 + misChinahigh, family=binomial(), data = Placebo) #base cateogry is comic
# Column 4: Open discussion
model4<- glm(openPlacebo ~ PRC + interpersonal + ccp +  Grad + female + ProfTeacher + colleague_everyday + CIYearsOverOne+ JoinMotivations_material + TrainingOverMonth + AgeOver30 + misChinahigh, family=binomial(), data = Placebo) 
# Baseline rates
control_SelfCensor<-round(mean(Placebo$SelfCensorPlacebo[Placebo$control==1],na.rm=T),digits=3)
control_Twosided<-round(mean(Placebo$PosTakingPlacebo2[Placebo$control==1],na.rm=T),digits=3); control_Onesided<-round(mean(Placebo$PosTakingPlacebo1[Placebo$control==1],na.rm=T)-mean(Placebo$PosTakingPlacebo2[Placebo$control==1],na.rm=T),digits=3)
control_Open<-round(mean(Placebo$openPlacebo[Placebo$control==1],na.rm=T),digits=3)
# Number of observations
n<-sum(!is.na(Placebo$SelfCensorPlacebo)) 
# Write the panel
Table6_overall<-stargazer(model1, model2, model3, model4,
                   covariate.labels = c("Objectives Prime","Social Prime"),
                   title=c("Table A6: Effects of Objectives Prime and Social Prime on Placebo Topic - Overall"), 
                   digits=3, type="text", dep.var.labels = c("Self-censor","One-sided","Two-sided","Open discussion"),
                   dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh"),
                   no.space = T, model.names=F, model.numbers = F,
                   add.lines = list(c("Baseline rate",control_SelfCensor, control_Onesided, control_Twosided, control_Open), c("Observation",rep(n,4)))) 
rm(model1,model2,model3,model4)
## Men panel:
# Column 1: Self-censorship
model1 <- glm(SelfCensorPlacebo ~ PRC + interpersonal  + ccp +  Grad + AgeOver30  + Chn_news_freq +  Local_news_freq + TeachYearsBefore0 + ProfTeacher + colleague_everyday + CIYearsOverOne + current, family=binomial(), data = Placebo[Placebo$female==0,]) 
# Column 2: One-sided position taking
model2 <- glm(PosTakingPlacebo1 ~ PRC + interpersonal + ccp +  Grad + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher, family=binomial(), data = Placebo[Placebo$female==0,]) 
# Column 3: Two-sided position taking
model3<- glm(PosTakingPlacebo2 ~ PRC + interpersonal + ccp +  Grad + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher, family=binomial(), data = Placebo[Placebo$female==0,]) 
#Column 4: Open discussion
model4<- glm(openPlacebo ~ PRC + interpersonal + ccp +  Grad + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher, family=binomial(), data = Placebo[Placebo$female==0,]) 
# Baseline rates
control_SelfCensor<-round(mean(Placebo$SelfCensorPlacebo[Placebo$control==1 & Placebo$female==0],na.rm=T),digits=3)
control_Twosided<-round(mean(Placebo$PosTakingPlacebo2[Placebo$control==1 & Placebo$female==0],na.rm=T),digits=3)
control_Onesided<-round(mean(Placebo$PosTakingPlacebo1[Placebo$control==1 & Placebo$female==0],na.rm=T),digits=3) - control_Twosided
control_Open<-round(mean(Placebo$openPlacebo[Placebo$control==1 & Placebo$female==0],na.rm=T),digits=3)
# Number of observations
n<-sum(!is.na(Placebo$SelfCensorPlacebo[Placebo$female==0]))
# Write the panel
Table6_men<-stargazer(model1, model2, model3, model4,
                          covariate.labels = c("Objectives Prime","Social Prime"),
                          title=c("Effects of Objectives Prime and Social Prime on Placebo Topic - Men"), 
                          digits=3, type="text", dep.var.labels = c("Self-censor","One-sided","Two-sided","Open discussion"),
                          dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh"),
                          no.space = T, model.names=F, model.numbers = F,
                          add.lines = list(c("Baseline rate",control_SelfCensor, control_Onesided, control_Twosided, control_Open), c("Observation",rep(n,4)))) 
rm(model1,model2,model3,model4)
## Women panel:
# Column 1: Self-censorship
model1 <-glm(SelfCensorPlacebo ~ PRC + interpersonal  + ccp +  Grad + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher + TeachYearsBefore0 + colleague_everyday + CIYearsOverOne + current, family=binomial(), data = Placebo[Placebo$female==1,]) 
# Column 2: One-sided position taking
model2 <-glm(PosTakingPlacebo1 ~ PRC + interpersonal + ccp +  Grad  + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher, family=binomial(), data = Placebo[Placebo$female==1,]) 
# Column 3: Two-sided position taking
model3<-glm(PosTakingPlacebo2 ~ PRC + interpersonal + ccp +  Grad  + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher, family=binomial(), data = Placebo[Placebo$female==1,]) 
#Column 4: Open discussion
model4<-glm(openPlacebo ~ PRC + interpersonal + ccp +  Grad  + AgeOver30 + Chn_news_freq +  Local_news_freq + ProfTeacher, family=binomial(), data = Placebo[Placebo$female==1,]) 
# Baseline rates
control_SelfCensor<-round(mean(Placebo$SelfCensorPlacebo[Placebo$control==1 & Placebo$female==1],na.rm=T),digits=3)
control_Twosided<-round(mean(Placebo$PosTakingPlacebo2[Placebo$control==1 & Placebo$female==1],na.rm=T),digits=3)
control_Onesided<-round(mean(Placebo$PosTakingPlacebo1[Placebo$control==1 & Placebo$female==1],na.rm=T),digits=3) - control_Twosided
control_Open<-round(mean(Placebo$openPlacebo[Placebo$control==1 & Placebo$female==1],na.rm=T),digits=3)
# Number of observations
n<-sum(!is.na(Placebo$SelfCensorPlacebo[Placebo$female==1]))
# Write the panel
Table6_women<-stargazer(model1, model2, model3, model4,
                          covariate.labels = c("Objectives Prime","Social Prime"),
                          title=c("Effects of Objectives Prime and Social Prime on Placebo Topic - Women"), 
                          digits=3, type="text", dep.var.labels = c("Self-censor","One-sided","Two-sided","Open discussion"),
                          dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh"),
                          no.space = T, header = F, model.names=F, model.numbers = F,
                          add.lines = list(c("Baseline rate",control_SelfCensor, control_Onesided, control_Twosided, control_Open), c("Observation",rep(n,4)))) 
rm(model1,model2,model3,model4)
## Write the final table
write.table(Table6_overall, file="Log/TableA6.txt", row.names = FALSE, quote=F)
suppressWarnings(write.table(Table6_men, file="Log/TableA6.txt", append=T, row.names = FALSE, quote=F))
suppressWarnings(write.table(Table6_women, file="Log/TableA6.txt", append=T, row.names = FALSE, quote=F))
rm(Placebo,Table6_overall,Table6_men,Table6_women,n,control_Onesided,control_Open,control_SelfCensor,control_Twosided)


#########################
### Table A7 ###
#########################
#Read in the main experiment data
d<-read.csv("Data/CI_MainExperiment.csv",header=TRUE, stringsAsFactors = FALSE,sep=",") 
d<-d[!is.na(d$SelfCensor),];dd<-d[d$VigOrder==1,] #Use only the first answered vignette per respondent
# Column 1: Self-censor, OLS
ols.nc1 <- lm(SelfCensor ~ PRC + interpersonal +  female + Grad + ProfTeacher + TeachYearsBefore0  + Chn_news_freq +  Local_news_freq + colleague_everyday + current + JoinMotivations_material, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: Self-censor, Logit
m2<-logitmfx(SelfCensor ~ PRC + interpersonal +  female + Grad + ProfTeacher + TeachYearsBefore0  + Chn_news_freq +  Local_news_freq + colleague_everyday + current + JoinMotivations_material, data = dd, atmean=F) 
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: One-sided, OLS
ols.nc1 <- lm(PosTaking1 ~ PRC + interpersonal + ccp + Grad + female + ProfTeacher + JoinMotivations_material + Chn_news_freq + Local_news_freq + NorthAmerica + Ocenia  + ChnPol_news_freq  + current + TeachYearsBefore0, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: One-sided, Logit
m4<-logitmfx(PosTaking1 ~ PRC + interpersonal + ccp + Grad + female + ProfTeacher + JoinMotivations_material + Chn_news_freq + Local_news_freq + NorthAmerica + Ocenia  + ChnPol_news_freq  + current + TeachYearsBefore0, data = dd, atmean=F) 
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Column 5: Two-sided, OLS
ols.nc1 <- lm(PosTaking2 ~ PRC + interpersonal + ccp +  Grad + female +TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher +colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material+ TrainingOverMonth, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m5<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 6: Two-sided, Logit
m6<-logitmfx(PosTaking2 ~ PRC + interpersonal + ccp +  Grad + female +TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher +colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material+ TrainingOverMonth, data = dd, atmean=F) 
coef6<-m6$mfxest[,1]
se6<-m6$mfxest[,2]
# Column 7: Open discussion, OLS
ols.nc1 <- lm(open ~ PRC + interpersonal + ccp +  Grad + female + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + current + JoinMotivations_material + TrainingOverMonth, data = dd) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m7<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 8: Open discussion, Logit
m8<-logitmfx(open~PRC + interpersonal + ccp +  Grad + female + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + current + JoinMotivations_material + TrainingOverMonth, data = dd, atmean=F) 
coef8<-m8$mfxest[,1]
se8<-m8$mfxest[,2]
# Baseline rates
control_SelfCensor<-round(mean(dd[(dd$control==1),"SelfCensor"],na.rm=T),digits=3) 
control_Onesided<-round(mean(dd[(dd$control==1),"PosTaking1"],na.rm=T),digits=3) 
control_Twosided<-round(mean(dd[(dd$control==1),"PosTaking2"],na.rm=T),digits=3)
control_Open<-round(mean(dd[(dd$control==1),"open"],na.rm=T),digits=3)
# Write the table
TableA7_final<-stargazer(m1, m2$fit, m3, m4$fit, m5, m6$fit, m7, m8$fit,
                         coef=list(m1[,1],coef2, m3[,1], coef4, m5[,1], coef6, m7[,1], coef8),
                         se=list(m1[,2],se2, m3[,2],se4, m5[,2],se6, m7[,2],se8),
                        covariate.labels = c("Objectives Prime","Social Prime"),
                        title=c("Table A7: Overall Treatment Effects  (first vignette of each respondent)"), digits=3, 
                        type="text", align=TRUE, model.names=F, model.numbers = F,
                        dep.var.labels = c("Self-censor","","One-sided","","Two-sided","","Open discussion",""),
                        dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), 
                        column.labels = c(rep(c("OLS","Logit"),4)), 
                        omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US","NorthAmerica","Ocenia","ChnPol_news_freq"),
                        no.space = T, header = F,
                        add.lines = list(c("Baseline rate",rep(control_SelfCensor,2), rep(control_Onesided,2), rep(control_Twosided,2), rep(control_Open,2)), c("Observation",rep(284,8)))) 
write.table(TableA7_final, file="Log/TableA7.txt", row.names = FALSE,quote=F)

#########################
### Table A8 ###
#########################
rm(m1,m2,m3,m4,m5,m6,m7,m8,coef2,coef4,coef6,coef8,se2,se4,se6,se8,TableA7_final,control_SelfCensor,control_Onesided,control_Twosided,control_Open)
rm(ols.nc1,vcovWhite.nc1,dd)
# Column 1: Self-censor, OLS
ols.nc1 <- lm(SelfCensor ~ PRC + interpersonal + ccp +  Grad + TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth + AgeOver30, data = d) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: Self-censor, Logit
m2<-logitmfx(SelfCensor ~ PRC + interpersonal + ccp +  Grad + TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth + AgeOver30, data = d, atmean=F)
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: One-sided, OLS
ols.nc1 <- lm(PosTaking1 ~ PRC + interpersonal + ccp +  Grad + TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = d)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: One-sided, Logit
m4<-logitmfx(PosTaking1 ~ PRC + interpersonal + ccp +  Grad +  TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth, data = d, atmean=F)
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Column 5: Two-sided, OLS
ols.nc1 <- lm(PosTaking2 ~ PRC + interpersonal + ccp +  Grad +  TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth+ AgeOver30 + misChinahigh, data = d) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m5<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 6: Two-sided, Logit
m6<-logitmfx(PosTaking2 ~ PRC + interpersonal + ccp +  Grad + TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth+ AgeOver30 + misChinahigh, data = d, atmean=F)
coef6<-m6$mfxest[,1]
se6<-m6$mfxest[,2]
# Column 7: Open discussion, OLS
ols.nc1 <- lm(open ~ PRC + interpersonal + ccp +  Grad + TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth + AgeOver30 + misChinahigh, data = d) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m7<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 8: Open discussion, Logit
m8<-logitmfx(open ~ PRC + interpersonal + ccp +  Grad +  TeachYearsBefore0 + Chn_news_freq +  Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne +  current + JoinMotivations_material + TrainingOverMonth + AgeOver30 + misChinahigh, data = d, atmean=F)
coef8<-m8$mfxest[,1]
se8<-m8$mfxest[,2]
# Baseline rates
control_SelfCensor<-round(mean(d[(d$control==1),"SelfCensor"],na.rm=T),digits=3) 
control_Onesided<-round(mean(d[(d$control==1),"PosTaking1"],na.rm=T),digits=3) 
control_Twosided<-round(mean(d[(d$control==1),"PosTaking2"],na.rm=T),digits=3)
control_Open<-round(mean(d[(d$control==1),"open"],na.rm=T),digits=3)
# Write the table
TableA8_final<-stargazer(m1, m2$fit, m3, m4$fit, m5, m6$fit, m7, m8$fit,
                         coef=list(m1[,1],coef2, m3[,1], coef4, m5[,1], coef6, m7[,1], coef8),
                         se=list(m1[,2],se2, m3[,2],se4, m5[,2],se6, m7[,2],se8),
                         covariate.labels = c("Objectives Prime","Social Prime"),
                         title=c("Table A8: Overall Treatment Effects  (all vignettes)"), digits=3, 
                         type="text", align=TRUE, model.names=F, model.numbers = F,
                         dep.var.labels = c("Self-censor","","One-sided","","Two-sided","","Open discussion",""),
                         dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), 
                         column.labels = c(rep(c("OLS","Logit"),4)), 
                         omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                         no.space = T, header = F,
                         add.lines = list(c("Baseline rate",rep(control_SelfCensor,2), rep(control_Onesided,2), rep(control_Twosided,2), rep(control_Open,2)), c("Observation",rep(429,8)))) 
write.table(TableA8_final, file="Log/TableA8.txt", row.names = FALSE,quote=F)


######################################################################################
## Section A.6 Details of heterogeneous effects by gender
######################################################################################

#########################
### Table A9 ###
#########################
rm(m1,m2,m3,m4,m5,m6,m7,m8,coef2,coef4,coef6,coef8,se2,se4,se6,se8,TableA8_final,control_SelfCensor,control_Onesided,control_Twosided,control_Open)
rm(ols.nc1,vcovWhite.nc1)
dd<-d[d$VigOrder==1,]
Male<-dd[dd$female==0,]
Female<-dd[dd$female==1,]
## Men
# Column 1: Self-censor, OLS
ols.nc1 <- lm(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: Self-censor, Logit
m2<- logitmfx(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material+ TrainingOverMonth, data = Male, atmean=F)
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: One-sided, OLS
ols.nc1 <- lm(PosTaking1 ~ PRC + interpersonal+ Grad + AgeOver30 + Local_news_freq + ChnPol_news_freq + CIYearsOverOne + current + JoinMotivations_material +TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: One-sided, Logit
m4<- logitmfx(PosTaking1 ~ PRC + interpersonal+ Grad + AgeOver30 + Local_news_freq + ChnPol_news_freq + CIYearsOverOne + current + JoinMotivations_material +TrainingOverMonth, data = Male, atmean=F) 
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Column 5: Two-sided, OLS
ols.nc1 <- lm(PosTaking2 ~ PRC + interpersonal+ Grad + AgeOver30 + ccp + Local_news_freq + InClassVig + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m5<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 6: Two-sided, Logit
m6<-logitmfx(PosTaking2 ~ PRC + interpersonal+ Grad + AgeOver30 + ccp + Local_news_freq + InClassVig + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male, atmean=F)
coef6<-m6$mfxest[,1]
se6<-m6$mfxest[,2]
# Column 7: Open discussion, OLS
ols.nc1 <- lm(open ~ PRC + interpersonal  + AgeOver30 + Grad  + misChinahigh + Local_news_freq  + ccp + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m7<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 8: Open discussion, Logit
m8<- logitmfx(open ~ PRC + interpersonal  + AgeOver30 + Grad  + misChinahigh + Local_news_freq  + ccp + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male, atmean=F)
coef8<-m8$mfxest[,1]
se8<-m8$mfxest[,2]
# Baseline rates
control_SelfCensor<-round(mean(Male[(Male$control==1),"SelfCensor"],na.rm=T),digits=3) 
control_Onesided<-round(mean(Male[(Male$control==1),"PosTaking1"],na.rm=T),digits=3) 
control_Twosided<-round(mean(Male[(Male$control==1),"PosTaking2"],na.rm=T),digits=3)
control_Open<-round(mean(Male[(Male$control==1),"open"],na.rm=T),digits=3)
# Write the panel
TableA9_men<-stargazer(m1, m2$fit, m3, m4$fit, m5, m6$fit, m7, m8$fit,
                         coef=list(m1[,1],coef2, m3[,1], coef4, m5[,1], coef6, m7[,1], coef8),
                         se=list(m1[,2],se2, m3[,2],se4, m5[,2],se6, m7[,2],se8),
                         covariate.labels = c("Objectives Prime","Social Prime"),
                         title=c("Table A9: Effects by Gender (first vignette of each respondent) - Men"), digits=3, 
                         type="text", align=TRUE, model.names=F, model.numbers = F,
                         dep.var.labels = c("Self-censor","","One-sided","","Two-sided","","Open discussion",""),
                         dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), 
                         column.labels = c(rep(c("OLS","Logit"),4)), 
                         omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","InClassVig","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                         no.space = T, header = F,
                         add.lines = list(c("Baseline rate",rep(control_SelfCensor,2), rep(control_Onesided,2), rep(control_Twosided,2), rep(control_Open,2)), c("Observation",rep(119,8)))) 
## Women
# Column 1: Self-censor, OLS
ols.nc1 <- lm(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + Chn_news_freq + Local_news_freq +ccp + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + ProfTeacher + TrainingOverMonth, data = Female) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: Self-censor, Logit
m2<-logitmfx(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + Chn_news_freq + Local_news_freq +ccp + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + ProfTeacher + TrainingOverMonth, data = Female, atmean=F)
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: One-sided, OLS
ols.nc1 <-lm(PosTaking1 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: One-sided, Logit
m4<- logitmfx(PosTaking1 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female, atmean=F)
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Column 5: Two-sided, OLS
ols.nc1 <- lm(PosTaking2 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh + Chn_news_freq + Local_news_freq + ChnPol_news_freq + JoinMotivations_material + ProfTeacher + TrainingOverMonth, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m5<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 6: Two-sided, Logit
m6<-logitmfx(PosTaking2 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh + Chn_news_freq + Local_news_freq + ChnPol_news_freq + JoinMotivations_material + ProfTeacher + TrainingOverMonth, data = Female, atmean=F)
coef6<-m6$mfxest[,1]
se6<-m6$mfxest[,2]
# Column 7: Open discussion, OLS
ols.nc1 <- lm(open ~ PRC + interpersonal  + AgeOver30 + ccp + Grad + misChinahigh + Chn_news_freq + Local_news_freq  + colleague_everyday + current + JoinMotivations_material + TrainingOverMonth, data = Female) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m7<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 8: Open discussion, Logit
m8<- logitmfx(open ~ PRC + interpersonal  + AgeOver30 + ccp + Grad + misChinahigh + Chn_news_freq + Local_news_freq  + colleague_everyday + current + JoinMotivations_material + TrainingOverMonth,  data = Female, atmean = F)
coef8<-m8$mfxest[,1]
se8<-m8$mfxest[,2]
# Baseline rates
control_SelfCensor<-round(mean(Female[(Female$control==1),"SelfCensor"],na.rm=T),digits=3) 
control_Onesided<-round(mean(Female[(Female$control==1),"PosTaking1"],na.rm=T),digits=3) 
control_Twosided<-round(mean(Female[(Female$control==1),"PosTaking2"],na.rm=T),digits=3)
control_Open<-round(mean(Female[(Female$control==1),"open"],na.rm=T),digits=3)
# Write the panel
TableA9_women<-stargazer(m1, m2$fit, m3, m4$fit, m5, m6$fit, m7, m8$fit,
                       coef=list(m1[,1],coef2, m3[,1], coef4, m5[,1], coef6, m7[,1], coef8),
                       se=list(m1[,2],se2, m3[,2],se4, m5[,2],se6, m7[,2],se8),
                       covariate.labels = c("Objectives Prime","Social Prime"),
                       title=c("Table A9: Effects by Gender (first vignette of each respondent) - Women"), digits=3, 
                       type="text", align=TRUE, model.names=F, model.numbers = F,
                       dep.var.labels = c("Self-censor","","One-sided","","Two-sided","","Open discussion",""),
                       dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), 
                       column.labels = c(rep(c("OLS","Logit"),4)), 
                       omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","InClassVig","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                       no.space = T, header = F,
                       add.lines = list(c("Baseline rate",rep(control_SelfCensor,2), rep(control_Onesided,2), rep(control_Twosided,2), rep(control_Open,2)), c("Observation",rep(165,8)))) 
# Write the final table
write.table(TableA9_men, file="Log/TableA9.txt", row.names = FALSE,quote=F)
suppressWarnings(write.table(TableA9_women, file="Log/TableA9.txt", append=T, row.names = FALSE, quote=F))


#########################
### Table A10 ###
#########################
rm(m1,m2,m3,m4,m5,m6,m7,m8,coef2,coef4,coef6,coef8,se2,se4,se6,se8,TableA9_men,TableA9_women,control_SelfCensor,control_Onesided,control_Twosided,control_Open)
rm(ols.nc1,vcovWhite.nc1,dd,Female,Male)
Male<-d[d$female==0,]
Female<-d[d$female==1,]
## Men
# Column 1: Self-censor, OLS
ols.nc1 <- lm(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: Self-censor, Logit
m2<- logitmfx(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth,  data = Male, atmean = F)
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: One-sided, OLS
ols.nc1 <- lm(PosTaking1 ~ PRC + interpersonal + ccp + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: One-sided, Logit
m4<-logitmfx(PosTaking1 ~ PRC + interpersonal + ccp + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth,  data = Male, atmean = F)
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Column 5: Two-sided, OLS
ols.nc1 <-lm(PosTaking2 ~ PRC + interpersonal + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m5<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 6: Two-sided, Logit
m6<-logitmfx(PosTaking2~ PRC + interpersonal + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male, atmean=F) 
coef6<-m6$mfxest[,1]
se6<-m6$mfxest[,2]
# Column 7: Open discussion, OLS
ols.nc1 <-lm(open ~ PRC + interpersonal  + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh  + ccp  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m7<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 8: Open discussion, Logit
m8<-logitmfx(open ~ PRC + interpersonal  + AgeOver30 + Grad +  TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male, atmean=F)
coef8<-m8$mfxest[,1]
se8<-m8$mfxest[,2]
# Baseline rates
control_SelfCensor<-round(mean(Male[(Male$control==1),"SelfCensor"],na.rm=T),digits=3) 
control_Onesided<-round(mean(Male[(Male$control==1),"PosTaking1"],na.rm=T),digits=3) 
control_Twosided<-round(mean(Male[(Male$control==1),"PosTaking2"],na.rm=T),digits=3)
control_Open<-round(mean(Male[(Male$control==1),"open"],na.rm=T),digits=3)
# Write the panel
TableA10_men<-stargazer(m1, m2$fit, m3, m4$fit, m5, m6$fit, m7, m8$fit,
                       coef=list(m1[,1],coef2, m3[,1], coef4, m5[,1], coef6, m7[,1], coef8),
                       se=list(m1[,2],se2, m3[,2],se4, m5[,2],se6, m7[,2],se8),
                       covariate.labels = c("Objectives Prime","Social Prime"),
                       title=c("Table A10: Effects by Gender (all vignettes) - Men"), digits=3, 
                       type="text", align=TRUE, model.names=F, model.numbers = F,
                       dep.var.labels = c("Self-censor","","One-sided","","Two-sided","","Open discussion",""),
                       dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), 
                       column.labels = c(rep(c("OLS","Logit"),4)), 
                       omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","InClassVig","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                       no.space = T, header = F,
                       add.lines = list(c("Baseline rate",rep(control_SelfCensor,2), rep(control_Onesided,2), rep(control_Twosided,2), rep(control_Open,2)), c("Observation",rep(180,8)))) 
## Women
# Column 1: Self-censor, OLS
ols.nc1 <-lm(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") #robust s.e. 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: Self-censor, Logit
m2<-logitmfx(SelfCensor ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + ccp + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth,  data = Female, atmean = F)
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: One-sided, OLS
ols.nc1 <-lm(PosTaking1 ~ PRC + interpersonal  + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: One-sided, Logit
m4<-logitmfx(PosTaking1 ~ PRC + interpersonal + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh + ccp   + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female, atmean=F) 
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Column 5: Two-sided, OLS
ols.nc1 <-lm(PosTaking2 ~ PRC + interpersonal  + AgeOver30 +  Grad + TeachYearsBefore0 + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m5<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 6: Two-sided, Logit
m6<-logitmfx(PosTaking2 ~ PRC + interpersonal  + AgeOver30 +  Grad + TeachYearsBefore0 + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female, atmean=F)
coef6<-m6$mfxest[,1]
se6<-m6$mfxest[,2]
# Column 7: Open discussion, OLS
ols.nc1 <-lm(open ~ PRC + interpersonal  + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Female) 
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m7<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 8: Open discussion, Logit
m8<-logitmfx(open ~ PRC + interpersonal  + AgeOver30 + Grad + TeachYearsBefore0 + misChinahigh  + Chn_news_freq + Local_news_freq + ProfTeacher + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth,  data = Female, atmean = F)
coef8<-m8$mfxest[,1]
se8<-m8$mfxest[,2]
# Baseline rates
control_SelfCensor<-round(mean(Female[(Female$control==1),"SelfCensor"],na.rm=T),digits=3) 
control_Onesided<-round(mean(Female[(Female$control==1),"PosTaking1"],na.rm=T),digits=3) 
control_Twosided<-round(mean(Female[(Female$control==1),"PosTaking2"],na.rm=T),digits=3)
control_Open<-round(mean(Female[(Female$control==1),"open"],na.rm=T),digits=3)
# Write the panel
TableA10_women<-stargazer(m1, m2$fit, m3, m4$fit, m5, m6$fit, m7, m8$fit,
                         coef=list(m1[,1],coef2, m3[,1], coef4, m5[,1], coef6, m7[,1], coef8),
                         se=list(m1[,2],se2, m3[,2],se4, m5[,2],se6, m7[,2],se8),
                         covariate.labels = c("Objectives Prime","Social Prime"),
                         title=c("Table A10: Effects by Gender (all vignettes) - Women"), digits=3, 
                         type="text", align=TRUE, model.names=F, model.numbers = F,
                         dep.var.labels = c("Self-censor","","One-sided","","Two-sided","","Open discussion",""),
                         dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), 
                         column.labels = c(rep(c("OLS","Logit"),4)), 
                         omit=c("Constant","ccp","Grad","female","TeachYearsBefore0","Chn_news_freq","Local_news_freq","InClassVig","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                         no.space = T, header = F,
                         add.lines = list(c("Baseline rate",rep(control_SelfCensor,2), rep(control_Onesided,2), rep(control_Twosided,2), rep(control_Open,2)), c("Observation",rep(249,8)))) 
# Write the final table
write.table(TableA10_men, file="Log/TableA10.txt", row.names = FALSE,quote=F)
suppressWarnings(write.table(TableA10_women, file="Log/TableA10.txt", append=T, row.names = FALSE, quote=F))


#########################
### Figure 6 ###
#########################
rm(m1,m2,m3,m4,m5,m6,m7,m8,coef2,coef4,coef6,coef8,se2,se4,se6,se8,TableA10_men,TableA10_women,control_SelfCensor,control_Onesided,control_Twosided,control_Open)
rm(ols.nc1,vcovWhite.nc1,res_control)
res_control<-matrix(NA,nrow=8, ncol=4)
colnames(res_control)<-c("estimate","s.e.","lower","upper")
row.names(res_control)<-c("male_selfcensor","male_oneside","male_twoside","male_open",
                          "female_selfcensor","female_oneside","female_twoside","female_open")
res_control[1,"estimate"]<-mean(Male$SelfCensor[Male$control==1],na.rm=T)
res_control[1,"lower"]<-t.test(Male$SelfCensor[Male$control==1])$conf.int[1]
res_control[1,"upper"]<-t.test(Male$SelfCensor[Male$control==1])$conf.int[2]
res_control[2,"estimate"]<-mean(Male$PosTaking1[Male$control==1],na.rm=T)
res_control[2,"lower"]<-t.test(Male$PosTaking1[Male$control==1])$conf.int[1]
res_control[2,"upper"]<-t.test(Male$PosTaking1[Male$control==1])$conf.int[2]
res_control[3,"estimate"]<-mean(Male$PosTaking2[Male$control==1],na.rm=T)
res_control[3,"lower"]<-t.test(Male$PosTaking2[Male$control==1])$conf.int[1]
res_control[3,"upper"]<-t.test(Male$PosTaking2[Male$control==1])$conf.int[2]
res_control[4,"estimate"]<-mean(Male$open[Male$control==1],na.rm=T)
res_control[4,"lower"]<-t.test(Male$open[Male$control==1])$conf.int[1]
res_control[4,"upper"]<-t.test(Male$open[Male$control==1])$conf.int[2]

res_control[5,"estimate"]<-mean(Female$SelfCensor[Female$control==1],na.rm=T)
res_control[5,"lower"]<-t.test(Female$SelfCensor[Female$control==1])$conf.int[1]
res_control[5,"upper"]<-t.test(Female$SelfCensor[Female$control==1])$conf.int[2]
res_control[6,"estimate"]<-mean(Female$PosTaking1[Female$control==1],na.rm=T)
res_control[6,"lower"]<-t.test(Female$PosTaking1[Female$control==1])$conf.int[1]
res_control[6,"upper"]<-t.test(Female$PosTaking1[Female$control==1])$conf.int[2]
res_control[7,"estimate"]<-mean(Female$PosTaking2[Female$control==1],na.rm=T)
res_control[7,"lower"]<-t.test(Female$PosTaking2[Female$control==1])$conf.int[1]
res_control[7,"upper"]<-t.test(Female$PosTaking2[Female$control==1])$conf.int[2]
res_control[8,"estimate"]<-mean(Female$open[Female$control==1],na.rm=T)
res_control[8,"lower"]<-t.test(Female$open[Female$control==1])$conf.int[1]
res_control[8,"upper"]<-t.test(Female$open[Female$control==1])$conf.int[2]
res_control<-as.data.frame(res_control)
min(res_control$lower)
max(res_control$upper)
png("Log/Figure6.png", 
    units="in", width=10, height=7, res=240)
par(mar=c(8, 5, 1, 2) + 0.1)
x<-1:4
plot(x-0.1,res_control[1:4,1],ylim=c(-0.01,0.6),xlim=c(0.4,4.6),main = "",
     ylab = "Probability of the outcome",
     pch=16,cex.lab=1.5,cex.axis=1.5, xaxt="n",xlab="",cex=2)
points(x+0.1,res_control[5:8,1],pch=6)
axis(1,1:4,labels=c("Self-censor","One-sided","Two-sided","Open discussion"),
     cex.axis=1.5,mgp=c(3,2,0))
for(i in 1:4){
  segments(i-0.1,res_control[i,3],i-0.1,res_control[i,4],lwd=2)
  segments(i+0.1,res_control[i+4,3],i+0.1,res_control[i+4,4],lwd=2,lty=2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()

#########################
### Figure 7 ###
#########################
rm(Male,Female,res_control)
## Figure 7a: open discussion by gender - aged 30 or below
Male<-d[(d$female==0 & d$AgeOver30==0),]
Female<-d[(d$female==1 & d$AgeOver30==0),]
res.raw<-matrix(NA,nrow=6, ncol=5) 
colnames(res.raw)<-c("gender","treatment","estimate","lower","upper")
res.raw[,1]<-c("male","male","male","female","female","female")
res.raw[,2]<-c("control","PRC","interpersonal","control","PRC","interpersonal")
# Control
res.raw[1,"estimate"]<-mean(Male$open[Male$control==1],na.rm=T)
res.raw[1,"lower"]<-t.test(Male$open[Male$control==1])$conf.int[1]
res.raw[1,"upper"]<-t.test(Male$open[Male$control==1])$conf.int[2]
res.raw[4,"estimate"]<-mean(Female$open[Female$control==1],na.rm=T)
res.raw[4,"lower"]<-t.test(Female$open[Female$control==1])$conf.int[1]
res.raw[4,"upper"]<-t.test(Female$open[Female$control==1])$conf.int[2]
# Objectives Prime
res.raw[2,"estimate"]<-mean(Male$open[Male$PRC==1],na.rm=T)
res.raw[2,"lower"]<-t.test(Male$open[Male$PRC==1])$conf.int[1]
res.raw[2,"upper"]<-t.test(Male$open[Male$PRC==1])$conf.int[2]
res.raw[5,"estimate"]<-mean(Female$open[Female$PRC==1],na.rm=T)
res.raw[5,"lower"]<-t.test(Female$open[Female$PRC==1])$conf.int[1]
res.raw[5,"upper"]<-t.test(Female$open[Female$PRC==1])$conf.int[2]
# Social Prime
res.raw[3,"estimate"]<-mean(Male$open[Male$interpersonal==1],na.rm=T)
res.raw[3,"lower"]<-t.test(Male$open[Male$interpersonal==1])$conf.int[1]
res.raw[3,"upper"]<-t.test(Male$open[Male$interpersonal==1])$conf.int[2]
res.raw[6,"estimate"]<-mean(Female$open[Female$interpersonal==1],na.rm=T)
res.raw[6,"lower"]<-t.test(Female$open[Female$interpersonal==1])$conf.int[1]
res.raw[6,"upper"]<-t.test(Female$open[Female$interpersonal==1])$conf.int[2]
res.raw<-as.data.frame(res.raw)
res.raw[,3]<-as.numeric(as.character(res.raw[,3]))
res.raw[,4]<-as.numeric(as.character(res.raw[,4]))
res.raw[,5]<-as.numeric(as.character(res.raw[,5]))
min(res.raw$lower)
max(res.raw$upper)
pdf("Log/Figure7a.pdf",width=8)
par(mar=c(5, 5, 3, 2) + 0.1)
x<-1:3 
plot(x-0.1,res.raw[1:3,3],ylim=c(-0.05,0.6),col = 1,xlim=c(0.4,3.6),
     main = "Open Discussion by Gender - Aged 30 or below",
     ylab = "Probability of open discussion",cex.axis=1.5, 
     pch=16, cex.lab=1.5,xaxt="n",xlab="",cex=2)
axis(1,1:3,labels=c("Control\n","Objectives\nPrime","Social\nPrime"),cex.axis=1.5,mgp=c(3,3,0))
points(x+0.1,res.raw[4:6,3],pch=6)
for(x in 1:3){
  segments(x-0.1,res.raw[x,4],x-0.1,res.raw[x,5],lwd=2, col=1, lty=1:1)
  segments(x+0.1,res.raw[x+3,4],x+0.1,res.raw[x+3,5],lwd=2, col=1, lty=2:2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()

## Figure 7b: open discussion by gender - aged over 30
Male<-d[(d$female==0 & d$AgeOver30==1),]
Female<-d[(d$female==1 & d$AgeOver30==1),]
res.raw<-matrix(NA,nrow=6, ncol=5) 
colnames(res.raw)<-c("gender","treatment","estimate","lower","upper")
res.raw[,1]<-c("male","male","male","female","female","female")
res.raw[,2]<-c("control","PRC","interpersonal","control","PRC","interpersonal")
# Control
res.raw[1,"estimate"]<-mean(Male$open[Male$control==1],na.rm=T)
res.raw[1,"lower"]<-t.test(Male$open[Male$control==1])$conf.int[1]
res.raw[1,"upper"]<-t.test(Male$open[Male$control==1])$conf.int[2]
res.raw[4,"estimate"]<-mean(Female$open[Female$control==1],na.rm=T)
res.raw[4,"lower"]<-t.test(Female$open[Female$control==1])$conf.int[1]
res.raw[4,"upper"]<-t.test(Female$open[Female$control==1])$conf.int[2]
# Objectives Prime
res.raw[2,"estimate"]<-mean(Male$open[Male$PRC==1],na.rm=T)
res.raw[2,"lower"]<-t.test(Male$open[Male$PRC==1])$conf.int[1]
res.raw[2,"upper"]<-t.test(Male$open[Male$PRC==1])$conf.int[2]
res.raw[5,"estimate"]<-mean(Female$open[Female$PRC==1],na.rm=T)
res.raw[5,"lower"]<-t.test(Female$open[Female$PRC==1])$conf.int[1]
res.raw[5,"upper"]<-t.test(Female$open[Female$PRC==1])$conf.int[2]
# Social Prime
res.raw[3,"estimate"]<-mean(Male$open[Male$interpersonal==1],na.rm=T)
res.raw[3,"lower"]<-t.test(Male$open[Male$interpersonal==1])$conf.int[1]
res.raw[3,"upper"]<-t.test(Male$open[Male$interpersonal==1])$conf.int[2]
res.raw[6,"estimate"]<-mean(Female$open[Female$interpersonal==1],na.rm=T)
res.raw[6,"lower"]<-t.test(Female$open[Female$interpersonal==1])$conf.int[1]
res.raw[6,"upper"]<-t.test(Female$open[Female$interpersonal==1])$conf.int[2]
res.raw<-as.data.frame(res.raw)
res.raw[,3]<-as.numeric(as.character(res.raw[,3]))
res.raw[,4]<-as.numeric(as.character(res.raw[,4]))
res.raw[,5]<-as.numeric(as.character(res.raw[,5]))
min(res.raw$lower)
max(res.raw$upper)
pdf("Log/Figure7b.pdf",width=8)
par(mar=c(5, 5, 3, 2) + 0.1)
x<-1:3 
plot(x-0.1,res.raw[1:3,3],ylim=c(-0.05,0.6),col = 1,xlim=c(0.4,3.6),
     main = "Open Discussion by Gender - Aged over 30",
     ylab = "Probability of open discussion",cex.axis=1.5, 
     pch=16, cex.lab=1.5,xaxt="n",xlab="",cex=2)
axis(1,1:3,labels=c("Control\n","Objectives\nPrime","Social\nPrime"),cex.axis=1.5,mgp=c(3,3,0))
points(x+0.1,res.raw[4:6,3],pch=6)
for(x in 1:3){
  segments(x-0.1,res.raw[x,4],x-0.1,res.raw[x,5],lwd=2, col=1, lty=1:1)
  segments(x+0.1,res.raw[x+3,4],x+0.1,res.raw[x+3,5],lwd=2, col=1, lty=2:2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()

## Figure 7c: effect of objectives prime by gender and age
rm(Female,Male,res.raw)
Male<-d[d$female==0,]
Female<-d[d$female==1,]
res_effects<-matrix(NA,nrow=4, ncol=4)
colnames(res_effects)<-c("estimate","s.e.","lower","upper")
row.names(res_effects)<-c("male_below30","male_over30","female_below30","female_over30")
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + ProfTeacher + ccp +colleague_everyday + misChinahigh  + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data= Male[Male$AgeOver30==0,])
res_effects[1,1:2]<-coeftest(lgt.nc2)[2,1:2]
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + ProfTeacher + ccp  +colleague_everyday + misChinahigh  + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data= Male[Male$AgeOver30==1,])
res_effects[2,1:2]<-coeftest(lgt.nc2)[2,1:2]
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + ProfTeacher + ccp  + colleague_everyday + misChinahigh  + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data= Female[Female$AgeOver30==0,])
res_effects[3,1:2]<-coeftest(lgt.nc2)[2,1:2]
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material, data= Female[Female$AgeOver30==1,])
res_effects[4,1:2]<-coeftest(lgt.nc2)[2,1:2]
res_effects<-as.data.frame(res_effects)
res_effects$lower<-res_effects[,1] - 1.96*res_effects[,2] 
res_effects$upper<-res_effects[,1] + 1.96*res_effects[,2] 
pdf("Log/Figure7c.pdf",width=8)
par(mar=c(5, 5, 3, 2) + 0.1)
x<-1:2
plot(x-0.1,res_effects[1:2,1],ylim=c(-0.6,0.4),xlim=c(0.4,2.6),main = "Effect of Objectives Prime by Gender and Age",
     ylab = "Effect of objectives prime on open discussion",
     pch=16,cex.lab=1.5,cex.axis=1.5, xaxt="n",xlab="",cex=2)
points(x+0.1,res_effects[3:4,1],pch=6)
axis(1,1:2,labels=c("Aged 30\nor below","Aged over\n30"),cex.axis=1.5,mgp=c(3,4,0))
abline(h=0,lty=2)
for(i in 1:2){
  segments(i-0.1,res_effects[i,3],i-0.1,res_effects[i,4],lwd=2)
  segments(i+0.1,res_effects[i+2,3],i+0.1,res_effects[i+2,4],lwd=2,lty=2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()

## Figure 7d: effect of social prime by gender and age
rm(lgt.nc2)
res_effects<-matrix(NA,nrow=4, ncol=4)
colnames(res_effects)<-c("estimate","s.e.","lower","upper")
row.names(res_effects)<-c("male_below30","male_over30","female_below30","female_over30")
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + colleague_everyday + ProfTeacher + misChinahigh  + CIYearsOverOne + current + ccp +  JoinMotivations_material + TrainingOverMonth, data= Male[Male$AgeOver30==0,])
res_effects[1,1:2]<-coeftest(lgt.nc2)[3,1:2]
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + colleague_everyday + ProfTeacher + misChinahigh  + CIYearsOverOne + current + ccp+ JoinMotivations_material + TrainingOverMonth, data= Male[Male$AgeOver30==1,])
res_effects[2,1:2]<-coeftest(lgt.nc2)[3,1:2]
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + colleague_everyday + ProfTeacher + misChinahigh  + CIYearsOverOne + current + ccp + JoinMotivations_material + TrainingOverMonth, data= Female[Female$AgeOver30==0,])
res_effects[3,1:2]<-coeftest(lgt.nc2)[3,1:2]
lgt.nc2 <- lm(open ~ PRC + interpersonal + Grad + TeachYearsBefore0 + Chn_news_freq + Local_news_freq + colleague_everyday + current + JoinMotivations_material, data= Female[Female$AgeOver30==1,])
res_effects[4,1:2]<-coeftest(lgt.nc2)[3,1:2]
res_effects<-as.data.frame(res_effects)
res_effects$lower<-res_effects[,1] - 1.96*res_effects[,2] #lower bound of 95% CI
res_effects$upper<-res_effects[,1] + 1.96*res_effects[,2] #upper bound of 95% CI
pdf("Log/Figure7d.pdf",width=8)
par(mar=c(5, 5, 3, 2) + 0.1)
x<-1:2
plot(x-0.1,res_effects[1:2,1],ylim=c(-0.6,0.4),xlim=c(0.4,2.6),main = "Effect of Social Prime by Gender and Age",
     ylab = "Effect of social prime on open discussion",
     pch=16,cex.lab=1.5,cex.axis=1.5, xaxt="n",xlab="",cex=2)
points(x+0.1,res_effects[3:4,1],pch=6)
axis(1,1:2,labels=c("Aged 30\nor below","Aged over\n30"),cex.axis=1.5,mgp=c(3,4,0))
abline(h=0,lty=2)
for(i in 1:2){
  segments(i-0.1,res_effects[i,3],i-0.1,res_effects[i,4],lwd=2)
  segments(i+0.1,res_effects[i+2,3],i+0.1,res_effects[i+2,4],lwd=2,lty=2)
}
legend("topright",legend = c("Men","Women"), col = 1, lty = 1:2, cex = 1.5, lwd = 2:2, bty="n", pch=c(16, 6))
dev.off()


#########################
### Table A11 ###
#########################
rm(res_effects,Male,Female,lgt.nc2,i,x)
# Column 1: self-censor
m1<-logitmfx(SelfCensor ~ PRC*female*ccp + interpersonal*female*ccp + Grad +  TeachYearsBefore0 + AgeOver30 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef1<-m1$mfxest[,1]
se1<-m1$mfxest[,2]
# Column 2: one-sided
m2<-logitmfx(PosTaking1 ~ PRC*female*ccp + interpersonal*female*ccp + Grad + TrainingOverMonth + AgeOver30 +  TeachYearsBefore0 + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef2<-m2$mfxest[,1]
se2<-m2$mfxest[,2]
# Column 3: two-sided
m3<-logitmfx(PosTaking2 ~ PRC*female*ccp + interpersonal*female*ccp + Grad + TeachYearsBefore0 + AgeOver30 + current + JoinMotivations_material, data = d, atmean=F) 
coef3<-m3$mfxest[,1]
se3<-m3$mfxest[,2]
# Column 4: open discussion
m4<-logitmfx(open ~ PRC*female*ccp + interpersonal*female*ccp + Grad +  TeachYearsBefore0 + AgeOver30 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef4<-m4$mfxest[,1]
se4<-m4$mfxest[,2]
# Baseline rates:
control_SelfCensor<-round(mean(d$SelfCensor[d$female==0 & d$control==1 & d$ccp==0],na.rm=T),digits=2)
control_Onesided<-round(mean(d$PosTaking1[d$female==0 & d$control==1 & d$ccp==0],na.rm=T),digits=2)
control_Twosided<-round(mean(d$PosTaking2[d$female==0 & d$control==1 & d$ccp==0],na.rm=T),digits=2)
control_Open<-round(mean(d$open[d$female==0 & d$control==1 & d$ccp==0],na.rm=T),digits=2)
# Write the table
TableA11<-stargazer(m1$fit, m2$fit, m3$fit, m4$fit,
  coef=list(coef1,coef2,coef3,coef4),
  se=list(se1,se2,se3,se4),
  covariate.labels = c("Objectives * female * ccp", "Social * female * ccp", "Objectives * female", "Objectives * ccp", "female * ccp", "Social * female", "Social * ccp", "Objectives", "Social", "Female","CCP"),
  order = c(17,18,12,13,14,15,16,1,4,2,3),
  title=c("Table A11: Difference in gendered effects between CCP vs. non-CCP members"), digits=3, 
  type="text", align=TRUE, model.names=F, model.numbers = F,
  dep.var.labels = c("Self-censor","One-sided","Two-sided","Open discussion"), dep.var.labels.include = T, dep.var.caption ="", omit.stat=c("all"), omit=c("Constant","Grad","TeachYearsBefore0","Chn_news_freq","Local_news_freq","InClassVig","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
  no.space = T, header = F, add.lines = list(c("Control rate in male and non-CCP group",control_SelfCensor, control_Onesided, control_Twosided, control_Open), c("Observation",rep(429,4)))) 
write.table(TableA11, file="Log/TableA11.txt", row.names = FALSE,quote=F)


#########################
### Table A12 ###
#########################
rm(m1,m2,m3,m4,coef1,coef2,coef3,coef4,se1,se2,se3,se4,control_Onesided,control_Open,control_SelfCensor,control_Twosided,TableA11)
# Column 1: effect on open discussion among non-CCP members
dd<-d[d$ccp==0  & !is.na(d$ccp),]
m1<-logitmfx(dd$open ~ PRC*female + interpersonal*female + Grad +  TeachYearsBefore0 + AgeOver30 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = dd, atmean=F) 
coef1<-m1$mfxest[,1]; se1<-m1$mfxest[,2]; obs1<-sum(!is.na(dd$open))
rm(dd)
# Column 2: effect on open discussion among CCP members
dd<-d[d$ccp==1 & !is.na(d$ccp),]
m2<-logitmfx(dd$open ~ PRC*female + interpersonal*female + Grad +  TeachYearsBefore0 + AgeOver30 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = dd, atmean=F) 
coef2<-m2$mfxest[,1]; se2<-m2$mfxest[,2]; obs2<-sum(!is.na(dd$open))
# Baseline rates:
control_1<-round(mean(d$open[d$female==0 & d$control==1 & d$ccp==0],na.rm=T),digits=2)
control_2<-round(mean(d$open[d$female==0 & d$control==1 & d$ccp==1],na.rm=T),digits=2)
# Write the table
TableA12<-stargazer(m1$fit, m2$fit,
                    coef=list(coef1,coef2),
                    se=list(se1,se2),
                    covariate.labels = c("Objectives * female", "Social * female", "Objectives", "Social", "Female"),
                    order = c(11,12,1,3,2),
                    title=c("Table A12: Gender-based heterogeneous effects by CCP membership"), digits=3, 
                    type="text", align=TRUE, model.names=F, model.numbers = F,
                    column.labels = c("non-CCP", "CCP"), 
                    dep.var.labels.include = F, dep.var.caption ="Open discussion", omit.stat=c("all"), omit=c("Constant","Grad","TeachYearsBefore0","Chn_news_freq","Local_news_freq","InClassVig","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                    no.space = T, header = F, add.lines = list(c("Control rate among men", control_1, control_2), c("Observation", obs1, obs2))) 
write.table(TableA12, file="Log/TableA12.txt", row.names = FALSE,quote=F)


#########################
### Table A13 ###
#########################
rm(dd,m1,m2,coef1,coef2,se1,se2,TableA12,obs1,obs2,control_1,control_2)
Male<-d[d$female==0,]
# Column 1: Self-censor
ols.nc1 <- lm(SelfCensor ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + Grad + AgeOver30  + TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + US + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: One-sided
ols.nc1 <- lm(PosTaking1 ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + Grad + ProfTeacher+ misChinahigh + TrainingOverMonth + Chn_news_freq + US+ colleague_everyday + CIYearsOverOne + current, data = Male)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m2<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 3: Two-sided
ols.nc1 <- lm(PosTaking2 ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + Grad + AgeOver30 +  TeachYearsBefore0 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + ProfTeacher + US + colleague_everyday + CIYearsOverOne + current + JoinMotivations_material + TrainingOverMonth, data = Male)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: Open discussion
ols.nc1<- lm(open ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + Grad + AgeOver30 + misChinahigh  + ccp + Chn_news_freq + Local_news_freq + current+ CIYearsOverOne + JoinMotivations_material +TrainingOverMonth, data = Male)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1")
m4<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Baseline rates:
control_SelfCensor<-round(mean(Male$SelfCensor[Male$control==1 & Male$Prv_stu==1],na.rm=T),digits=2)
control_Onesided<-round(mean(Male$PosTaking1[Male$control==1 & Male$Prv_stu==1],na.rm=T),digits=2)
control_Twosided<-round(mean(Male$PosTaking2[Male$control==1 & Male$Prv_stu==1],na.rm=T),digits=2)
control_Open<-round(mean(Male$open[Male$control==1 & Male$Prv_stu==1],na.rm=T),digits=2)
# Write the table
TableA13<-stargazer(m1,m2,m3,m4,
                    coef=list(m1[,1],m2[,1],m3[,1],m4[,1]),
                    se=list(m1[,2],m2[,2],m3[,2],m4[,2]),
                    covariate.labels = c("Objectives * in-class", "Objectives * private colleague", "Social * in-class", "Social * private colleague", "Objectives Prime", "Social Prime", "In-class vignette", "Private colleague vignette"),
                    order = c(19,21,20,22,1,3,2,4),
title=c("Table A13: Heterogeneous effects by vignettes among men"), digits=3, 
type="text", align=TRUE, model.names=F, model.numbers = F, dep.var.caption = "",
column.labels = c("Self-censor","One-sided","Two-sided","Open discussion"), 
dep.var.labels.include = T, omit.stat=c("all"), omit=c("Constant","Grad","TeachYearsBefore0","ccp","Chn_news_freq","Local_news_freq","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
no.space = T, header = F, add.lines = list(c("Control rate in private student vignette", control_SelfCensor, control_Onesided, control_Twosided, control_Open), c("Observation", rep(sum(!is.na(Male$SelfCensor)),4)))) 
write.table(TableA13, file="Log/TableA13.txt", row.names = FALSE,quote=F)


#########################
### Table A14 ###
#########################
rm(m1,m2,m3,m4,Male,ols.nc1,vcovWhite.nc1,control_Onesided,control_Open,control_SelfCensor,control_Twosided,TableA13)
Female<-d[d$female==1,]
# Column 1: Self-censor
ols.nc1 <- lm(SelfCensor ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + JoinMotivations_material + CIYearsOverOne + AgeOver30, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m1<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 2: One-sided
ols.nc1 <- lm(PosTaking1 ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + JoinMotivations_material + CIYearsOverOne + AgeOver30, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m2<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 3: Two-sided
ols.nc1 <- lm(PosTaking2 ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + JoinMotivations_material + CIYearsOverOne + AgeOver30, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m3<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Column 4: Open discussion
ols.nc1 <- lm(open ~ PRC*InClassVig  + interpersonal*InClassVig + PRC*Prv_clg + interpersonal*Prv_clg + misChinahigh  + colleague_everyday + JoinMotivations_material + TrainingOverMonth, data = Female)
vcovWhite.nc1<-vcovHC(ols.nc1, type="HC1") 
m4<-coeftest(ols.nc1, vcov = vcovWhite.nc1)
# Baseline rates
control_SelfCensor<-round(mean(Female$SelfCensor[Female$control==1 & Female$Prv_stu==1],na.rm=T),digits=3)
control_Onesided<-round(mean(Female$PosTaking1[Female$control==1 & Female$Prv_stu==1],na.rm=T),digits=3)
control_Twosided<-round(mean(Female$PosTaking2[Female$control==1 & Female$Prv_stu==1],na.rm=T),digits=3)
control_Open<-round(mean(Female$open[Female$control==1 & Female$Prv_stu==1],na.rm=T),digits=3)
# Write the table
TableA14<-stargazer(m1,m2,m3,m4,
                    coef=list(m1[,1],m2[,1],m3[,1],m4[,1]),
                    se=list(m1[,2],m2[,2],m3[,2],m4[,2]),
                    covariate.labels = c("Objectives * in-class", "Objectives * private colleague", "Social * in-class", "Social * private colleague", "Objectives Prime", "Social Prime", "In-class vignette", "Private colleague vignette"),
                    order = c(11,13,12,14,1,3,2,4),
                    title=c("Table A14: Heterogeneous effects by vignettes among women"), digits=3, 
                    type="text", align=TRUE, model.names=F, model.numbers = F, dep.var.caption = "",
                    column.labels = c("Self-censor","One-sided","Two-sided","Open discussion"), 
                    dep.var.labels.include = T, omit.stat=c("all"), 
                    omit=c("Constant","Grad","TeachYearsBefore0","ccp","Chn_news_freq","Local_news_freq","ChnPol_news_freq","ProfTeacher", "colleague_everyday","CIYearsOverOne","current","JoinMotivations_material","TrainingOverMonth", "AgeOver30", "misChinahigh","US"),
                    no.space = T, header = F, add.lines = list(c("Control rate in private student vignette", control_SelfCensor, control_Onesided, control_Twosided, control_Open), c("Observation", rep(sum(!is.na(Female$SelfCensor)),4)))) 
write.table(TableA14, file="Log/TableA14.txt", row.names = FALSE,quote=F)


#########################
### Table A15 ###
#########################
rm(m1,m2,m3,m4,Female,ols.nc1,vcovWhite.nc1,control_Onesided,control_Open,control_SelfCensor,control_Twosided,TableA14)
dd<-d[1:284,] #unique respondents
dd<-dd[dd$female==1,] #women respondents
dcontrol<-dd[dd$control==1,] #Control
dPRC<-dd[dd$PRC==1,] #Objectives treatment group
dInterpersonal<-dd[dd$interpersonal==1,] #Social treatment group
# Calculate means and p-values
Control<-c(mean(dcontrol$ProfTeacher,na.rm=T),
           mean(dcontrol$age,na.rm=T),
           mean(dcontrol$ccp,na.rm=T),
           mean(dcontrol$Grad,na.rm=T),
           mean(dcontrol$CIYears_num,na.rm=T),
           mean(dcontrol$TeacherBefore,na.rm=T), 
           mean(dcontrol$TrainingOverMonth,na.rm=T),
           mean(dcontrol$LocalMediaFriend,na.rm=T))
Objectives<-c(mean(dPRC$ProfTeacher,na.rm=T),
              mean(dPRC$age,na.rm=T),
              mean(dPRC$ccp,na.rm=T),
              mean(dPRC$Grad,na.rm=T),
              mean(dPRC$CIYears_num,na.rm=T),
              mean(dPRC$TeacherBefore,na.rm=T), 
              mean(dPRC$TrainingOverMonth,na.rm=T),
              mean(dPRC$LocalMediaFriend,na.rm=T))
Social<-c(mean(dInterpersonal$ProfTeacher,na.rm=T),
          mean(dInterpersonal$age,na.rm=T),
          mean(dInterpersonal$ccp,na.rm=T),
          mean(dInterpersonal$Grad,na.rm=T),
          mean(dInterpersonal$CIYears_num,na.rm=T),
          mean(dInterpersonal$TeacherBefore,na.rm=T), 
          mean(dInterpersonal$TrainingOverMonth,na.rm=T),
          mean(dInterpersonal$LocalMediaFriend,na.rm=T))
p_ProfTeacher<-pf(summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[2],
                  summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_age<-pf(summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[2],
          summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_ccp<-pf(summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[2],
          summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_Grad<-pf(summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[2],
           summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_CIYears_num<-pf(summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[2],
                  summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_TeacherBefore<-pf(summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[2],
                    summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_TrainingOverMonth<-pf(summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[2],
                        summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_LocalMediaFriend<-pf(summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[2],
                       summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
Pvalue<-c(p_ProfTeacher,p_age,p_ccp,p_Grad,p_CIYears_num,p_TeacherBefore,p_TrainingOverMonth, p_LocalMediaFriend)
obs<-c(nrow(dcontrol),nrow(dPRC),nrow(dInterpersonal),"-")
# Write the table
balance<-cbind.data.frame(Control,Objectives,Social,Pvalue)
balance<-round(balance,digits=3)
balance[2,1:3]<-round(balance[2,1:3],digits=1) #rounding age to 0.1 decimal points
balance[9,]<-obs
colnames(balance)<-c("Control","Objectives","Social","P-value")
rownames(balance)<-c("Seniority in CI", "Age", "CCP member", "Graduate degree", "Years at CI", "Teaching before CI (Y/N)", "Hanban training over 1 month", "Perceived friendliness of host-country media", "Number of respondents")
TableA15<-stargazer(balance,title="Table A15: Balance of Covariates within Women CI teachers",
                         type="text",summary=FALSE, digits=3)
write.table(TableA15, file="Log/TableA15.txt", row.names = FALSE, quote=F)


#########################
### Table A16 ###
#########################
rm(dd,dcontrol,dPRC,dInterpersonal,balance,Control,Objectives,Social,Pvalue,obs,p_age,p_ccp,p_Grad,TableA15)
rm(p_CIYears_num,p_LocalMediaFriend,p_ProfTeacher,p_TeacherBefore,p_TrainingOverMonth)
dd<-d[1:284,] #unique respondents
dd<-dd[dd$female==0,] #men respondents
dcontrol<-dd[dd$control==1,] #Control
dPRC<-dd[dd$PRC==1,] #Objectives treatment group
dInterpersonal<-dd[dd$interpersonal==1,] #Social treatment group
# Calculate means and p-values
Control<-c(mean(dcontrol$ProfTeacher,na.rm=T),
           mean(dcontrol$age,na.rm=T),
           mean(dcontrol$ccp,na.rm=T),
           mean(dcontrol$Grad,na.rm=T),
           mean(dcontrol$CIYears_num,na.rm=T),
           mean(dcontrol$TeacherBefore,na.rm=T), 
           mean(dcontrol$TrainingOverMonth,na.rm=T),
           mean(dcontrol$LocalMediaFriend,na.rm=T))
Objectives<-c(mean(dPRC$ProfTeacher,na.rm=T),
              mean(dPRC$age,na.rm=T),
              mean(dPRC$ccp,na.rm=T),
              mean(dPRC$Grad,na.rm=T),
              mean(dPRC$CIYears_num,na.rm=T),
              mean(dPRC$TeacherBefore,na.rm=T), 
              mean(dPRC$TrainingOverMonth,na.rm=T),
              mean(dPRC$LocalMediaFriend,na.rm=T))
Social<-c(mean(dInterpersonal$ProfTeacher,na.rm=T),
          mean(dInterpersonal$age,na.rm=T),
          mean(dInterpersonal$ccp,na.rm=T),
          mean(dInterpersonal$Grad,na.rm=T),
          mean(dInterpersonal$CIYears_num,na.rm=T),
          mean(dInterpersonal$TeacherBefore,na.rm=T), 
          mean(dInterpersonal$TrainingOverMonth,na.rm=T),
          mean(dInterpersonal$LocalMediaFriend,na.rm=T))
p_ProfTeacher<-pf(summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[2],
                  summary(lm(ProfTeacher~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_age<-pf(summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[2],
          summary(lm(age~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_ccp<-pf(summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[2],
          summary(lm(ccp~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_Grad<-pf(summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[2],
           summary(lm(Grad~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_CIYears_num<-pf(summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[2],
                  summary(lm(CIYears_num~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_TeacherBefore<-pf(summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[2],
                    summary(lm(TeacherBefore~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_TrainingOverMonth<-pf(summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[2],
                        summary(lm(TrainingOverMonth~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
p_LocalMediaFriend<-pf(summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[1],summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[2],
                       summary(lm(LocalMediaFriend~PRC+interpersonal,data=dd))$fstatistic[3], lower.tail=FALSE)
Pvalue<-c(p_ProfTeacher,p_age,p_ccp,p_Grad,p_CIYears_num,p_TeacherBefore,p_TrainingOverMonth, p_LocalMediaFriend)
obs<-c(nrow(dcontrol),nrow(dPRC),nrow(dInterpersonal),"-")
# Write the table
balance<-cbind.data.frame(Control,Objectives,Social,Pvalue)
balance<-round(balance,digits=3)
balance[2,1:3]<-round(balance[2,1:3],digits=1) #rounding age to 0.1 decimal points
balance[9,]<-obs
colnames(balance)<-c("Control","Objectives","Social","P-value")
rownames(balance)<-c("Seniority in CI", "Age", "CCP member", "Graduate degree", "Years at CI", "Teaching before CI (Y/N)", "Hanban training over 1 month", "Perceived friendliness of host-country media", "Number of respondents")
TableA16<-stargazer(balance,title="Table A16: Balance of Covariates within Men CI teachers",
                    type="text",summary=FALSE, digits=3)
write.table(TableA16, file="Log/TableA16.txt", row.names = FALSE, quote=F)


#########################
### Table A17 ###
#########################
rm(dd,dcontrol,dPRC,dInterpersonal,balance,Control,Objectives,Social,Pvalue,obs,p_age,p_ccp,p_Grad,TableA16)
rm(p_CIYears_num,p_LocalMediaFriend,p_ProfTeacher,p_TeacherBefore,p_TrainingOverMonth)
dd<-d[1:284,] #unique respondents
Male<-dd[dd$female==0,] 
Female<-dd[dd$female==1,] 
# Calculate means of each gender and p-values
Male_cov <-c(mean(Male$ProfTeacher,na.rm=T),
             mean(Male$age,na.rm=T),
             mean(Male$ccp,na.rm=T),
             mean(Male$Grad,na.rm=T),
             mean(Male$CIYears_num,na.rm=T),
             mean(Male$TeacherBefore,na.rm=T),
             mean(Male$TrainingOverMonth,na.rm=T), 
             mean(Male$LocalMediaFriend,na.rm=T))
Female_cov <-c(mean(Female$ProfTeacher,na.rm=T),
               mean(Female$age,na.rm=T),
               mean(Female$ccp,na.rm=T),
               mean(Female$Grad,na.rm=T),
               mean(Female$CIYears_num,na.rm=T),
               mean(Female$TeacherBefore,na.rm=T),
               mean(Female$TrainingOverMonth,na.rm=T), 
               mean(Female$LocalMediaFriend,na.rm=T))
pvalue_cov<-c(t.test(Male$ProfTeacher, Female$ProfTeacher)$p.value, 
              t.test(Male$age, Female$age)$p.value,
              t.test(Male$ccp, Female$ccp)$p.value,
              t.test(Male$Grad, Female$Grad)$p.value,
              t.test(Male$CIYears_num, Female$CIYears_num)$p.value,
              t.test(Male$TeacherBefore, Female$TeacherBefore)$p.value,
              t.test(Male$TrainingOverMonth, Female$TrainingOverMonth)$p.value,
              t.test(Male$LocalMediaFriend, Female$LocalMediaFriend)$p.value)
obs<-c(nrow(Male),nrow(Female),"-")
# Write the table
gender_balance<-cbind.data.frame(Male_cov, Female_cov, pvalue_cov)
gender_balance<-round(gender_balance,digits=3)
gender_balance[2,1:2]<-round(gender_balance[2,1:2],digits=1) #rounding age to 0.1 decimal points
gender_balance[9,]<-obs
colnames(gender_balance)<-c("Men","Women","P-value")
rownames(gender_balance)<-c("Seniority in CI", "Age", "CCP member", "Graduate degree", "Years at CI", "Teaching before CI (Y/N)", "Hanban training over 1 month", "Perceived friendliness of host-country media", "Number of respondents")
TableA17<-stargazer(gender_balance,title="Table A17: Comparing Pre-treatment Covariates between Genders",
                    type="text",summary=FALSE, digits=3)
write.table(TableA17, file="Log/TableA17.txt", row.names = FALSE, quote=F)


#########################
### Table A18 ###
#########################
rm(dd,Female,Male,gender_balance,obs,Male_cov,Female_cov,pvalue_cov,TableA17)

## Step 1: Calculate difference in effect, SE, and unadjusted p-value for each of the 14 pre-registered covariates
coef<-matrix(NA,nrow=14,ncol=2); SE<-matrix(NA,nrow=14,ncol=2); pvalue<-matrix(NA,nrow=14,ncol=2)
row.names(coef)<-c("female","AgeOver30","ccp","Grad","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "LocalMediaFriendOver8","colleague_everyday", "misChinahigh","agree_personality","mission_misunderstanding","US")
colnames(coef)<-c("Objectives","Social")
row.names(SE)<-c("female","AgeOver30","ccp","Grad","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "LocalMediaFriendOver8","colleague_everyday", "misChinahigh","agree_personality","mission_misunderstanding","US")
colnames(SE)<-c("Objectives","Social")
row.names(pvalue)<-c("female","AgeOver30","ccp","Grad","TeachYearsBefore0","Chn_news_freq","Local_news_freq","ProfTeacher", "LocalMediaFriendOver8","colleague_everyday", "misChinahigh","agree_personality","mission_misunderstanding","US")
colnames(pvalue)<-c("Objectives","Social")
# Estimate the diff in effect btw genders (then repeat the same process for each covariate)
m1<-logitmfx(open ~ PRC*female + interpersonal*female + AgeOver30 + ccp + Grad + TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[1,]<-tail(m1$mfxest[,1],n=2);SE[1,]<-tail(m1$mfxest[,2],n=2)
pvalue[1,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*AgeOver30 + interpersonal*AgeOver30 + female + ccp + Grad + TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[2,]<-tail(m1$mfxest[,1],n=2);SE[2,]<-tail(m1$mfxest[,2],n=2)
pvalue[2,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*ccp + interpersonal*ccp + female + Grad + AgeOver30 + TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[3,]<-tail(m1$mfxest[,1],n=2);SE[3,]<-tail(m1$mfxest[,2],n=2)
pvalue[3,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*Grad + interpersonal*Grad + female + ccp + AgeOver30 + TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[4,]<-tail(m1$mfxest[,1],n=2);SE[4,]<-tail(m1$mfxest[,2],n=2)
pvalue[4,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*TeachYearsBefore0 + interpersonal*TeachYearsBefore0 + female + ccp + Grad + AgeOver30 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[5,]<-tail(m1$mfxest[,1],n=2);SE[5,]<-tail(m1$mfxest[,2],n=2)
pvalue[5,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*Chn_news_freq + interpersonal*Chn_news_freq + female + TeachYearsBefore0 + ccp + Grad + AgeOver30 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[6,]<-tail(m1$mfxest[,1],n=2);SE[6,]<-tail(m1$mfxest[,2],n=2)
pvalue[6,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*Local_news_freq + interpersonal*Local_news_freq + female + AgeOver30 + Grad + CIYearsOverOne + TrainingOverMonth + TeachYearsBefore0 + US + misChinahigh + Chn_news_freq, data = d, atmean=F) 
coef[7,]<-tail(m1$mfxest[,1],n=2);SE[7,]<-tail(m1$mfxest[,2],n=2)
pvalue[7,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*ProfTeacher + interpersonal*ProfTeacher + female + AgeOver30 + ccp + TrainingOverMonth + CIYearsOverOne + TeachYearsBefore0 + US + misChinahigh+ Local_news_freq + Chn_news_freq, data = d, atmean=F) 
coef[8,]<-tail(m1$mfxest[,1],n=2);SE[8,]<-tail(m1$mfxest[,2],n=2)
pvalue[8,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*LocalMediaFriendOver8 + interpersonal*LocalMediaFriendOver8 + female + TeachYearsBefore0 + ccp + Grad + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[9,]<-tail(m1$mfxest[,1],n=2);SE[9,]<-tail(m1$mfxest[,2],n=2)
pvalue[9,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*colleague_everyday + interpersonal*colleague_everyday + female + AgeOver30 + Grad +TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[10,]<-tail(m1$mfxest[,1],n=2);SE[10,]<-tail(m1$mfxest[,2],n=2)
pvalue[10,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*misChinahigh + interpersonal*misChinahigh + female + Grad + AgeOver30+TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[11,]<-tail(m1$mfxest[,1],n=2);SE[11,]<-tail(m1$mfxest[,2],n=2)
pvalue[11,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*agree_personality + interpersonal*agree_personality + female + ccp + Grad + AgeOver30 + TeachYearsBefore0 + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[12,]<-tail(m1$mfxest[,1],n=2);SE[12,]<-tail(m1$mfxest[,2],n=2)
pvalue[12,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*mission_misunderstanding + interpersonal*mission_misunderstanding + female + Grad + AgeOver30 + TeachYearsBefore0  + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[13,]<-tail(m1$mfxest[,1],n=2);SE[13,]<-tail(m1$mfxest[,2],n=2)
pvalue[13,]<-tail(m1$mfxest[,4],n=2);rm(m1)
m1<-logitmfx(open ~ PRC*US + interpersonal*US + female + Grad + AgeOver30 +  TeachYearsBefore0 + ccp + TrainingOverMonth + CIYearsOverOne + current + JoinMotivations_material, data = d, atmean=F) 
coef[14,]<-tail(m1$mfxest[,1],n=2);SE[14,]<-tail(m1$mfxest[,2],n=2)
pvalue[14,]<-tail(m1$mfxest[,4],n=2);rm(m1)

## Step 2: Multiple testing correction on p-values
alpha<-0.05
# Benjamini-Hochberg correction (FDR)
BH_result<-p.adjust(pvalue[,1], "BH") < alpha
# Holm correction (FWER)
Holm_result<-p.adjust(pvalue[,1], "holm") < alpha
# Bonferroni correction 
BF_result<-p.adjust(pvalue[,1], "bonferroni") < alpha

# Step 3: Write the table
TableA18<-cbind.data.frame(coef[,1], SE[,1], pvalue[,1], BH_result, Holm_result, BF_result)
TableA18[,1:3]<-round(TableA18[,1:3],digits=3)
colnames(TableA18)<-c("Estimate","SE","Unadjusted p-value","BH","Holm","BF")
rownames(TableA18)<-c("Women vs. Men","Age (>30 vs. <=30)","CCP vs. Non-CCP","Graduate vs. below Graduate",
                      "Work experience before CI (Yes vs. No)","Freq PRC news consumer (Yes vs. No)","Freq local news consumer (Yes vs. No)",
                      "Seniority in CI (Senior vs. Junior)","Perceived host-country media friendly (Yes vs. No)",
                      "Interact with host-country teacher everyday (Yes vs. No)","Perceived misunderstanding on China (high vs. low)",
                      "Self-reported agreeableness (Yes vs. No)","Perceived political mission (Yes vs. No)","US vs. Non-US")
TableA18_final<-stargazer(TableA18,title="Table A18: Difference in Effects of Objectives Prime",
                    type="text",summary=FALSE, digits=3)
write.table(TableA18_final, file="Log/TableA18.txt", row.names = FALSE, quote=F)


#########################
### Table A19 ###
#########################
rm(BH_result,Holm_result,BF_result,TableA18_final,TableA18)
## Multiple testing correction on p-values
alpha<-0.05
# Benjamini-Hochberg correction (FDR)
BH_result<-p.adjust(pvalue[,2], "BH") < alpha
# Holm correction (FWER)
Holm_result<-p.adjust(pvalue[,2], "holm") < alpha
# Bonferroni correction 
BF_result<-p.adjust(pvalue[,2], "bonferroni") < alpha

# Step 3: Write the table
TableA19<-cbind.data.frame(coef[,2], SE[,2], pvalue[,2], BH_result, Holm_result, BF_result)
TableA19[,1:3]<-round(TableA19[,1:3],digits=3)
colnames(TableA19)<-c("Estimate","SE","Unadjusted p-value","BH","Holm","BF")
rownames(TableA19)<-c("Women vs. Men","Age (>30 vs. <=30)","CCP vs. Non-CCP","Graduate vs. below Graduate",
                      "Work experience before CI (Yes vs. No)","Freq PRC news consumer (Yes vs. No)","Freq local news consumer (Yes vs. No)",
                      "Seniority in CI (Senior vs. Junior)","Perceived host-country media friendly (Yes vs. No)",
                      "Interact with host-country teacher everyday (Yes vs. No)","Perceived misunderstanding on China (high vs. low)",
                      "Self-reported agreeableness (Yes vs. No)","Perceived political mission (Yes vs. No)","US vs. Non-US")
TableA19_final<-stargazer(TableA19,title="Table A19: Difference in Effects of Social Prime",
                          type="text",summary=FALSE, digits=3)
write.table(TableA19_final, file="Log/TableA19.txt", row.names = FALSE, quote=F)

